/*
 * ============LICENSE_START========================================================================
 * ONAP : tr-069-adapter
 * =================================================================================================
 * Copyright (C) 2020 CommScope Inc Intellectual Property.
 * =================================================================================================
 * This tr-069-adapter software file is distributed by CommScope Inc under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except in compliance with the License. You
 * may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions and
 * limitations under the License.
 * ===============LICENSE_END=======================================================================
 */

package org.commscope.tr069adapter.netconf.server;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.NetconfServerManagementError;
import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.commscope.tr069adapter.netconf.dao.NetConfServerDetailsRepository;
import org.commscope.tr069adapter.netconf.entity.NetConfServerDetailsEntity;
import org.commscope.tr069adapter.netconf.error.RetryFailedException;
import org.commscope.tr069adapter.netconf.error.ServerPortAllocationException;
import org.commscope.tr069adapter.netconf.server.helper.ServerPortAllocationHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class NetConfServerManagerImpl {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfServerManagerImpl.class);

  @Autowired
  ServerPortAllocationHelper serverPortAllocator;

  @Autowired
  NetConfServerDetailsRepository netconfDAO;

  @Autowired
  NetConfServerProperties config;

  @Autowired
  NetconfServerStarter ncServerStarter;

  @Autowired
  RestartNetconfServerHandler restartServersHandler;

  public void restartServers() {
    LOG.debug("Restarting all netconf servers during startup...");
    Iterable<NetConfServerDetailsEntity> entities = netconfDAO.findAll();
    List<NetConfServerDetailsEntity> serverDetailsList = new ArrayList<>();
    for (NetConfServerDetailsEntity entity : entities) {
      serverDetailsList.add(entity);
    }
    restartServersOnStartup(serverDetailsList);

    if (!serverDetailsList.isEmpty()) {
      LOG.debug("Attempting to start failed netconf servers {}", serverDetailsList);
      try {
        restartServersHandler.restart(serverDetailsList);
      } catch (RetryFailedException e) {
        LOG.error("Failed to restart all netconf servers. {}", e.toString());
      }
    }
    LOG.debug("Restarting netconf servers during startup is completed.");
  }

  public NetConfServerDetails createServer(String deviceId, String enodeBName) {
    NetConfServerDetails result = new NetConfServerDetails();
    NetConfServerDetailsEntity entity = null;
    if (deviceId != null) {
      entity = netconfDAO.findByDeviceId(deviceId);
    } else if (enodeBName != null) {
      entity = netconfDAO.findByEnodeBName(enodeBName);
    } else {
      // none is specified
      LOG.error(
          "Both deviceID and enodeBName are null. Hence failed to create the netconf server.");
      return null;
    }

    if (null != entity) {
      // found the entity. server is already running. double check and run
      // if
      // required. else return the details.

      // update the ENB Name if Changed
      entity.setEnodeBName(enodeBName);
      netconfDAO.save(entity);
      result = getNetConfServerDetails(deviceId, entity);
      return result;
    }

    try {

      String port = serverPortAllocator.reserveServerPort();
      LOG.debug("Successfully reserved a port for deviceID={} ,port={}", deviceId, port);

      // start the server
      boolean isServerStarted = ncServerStarter.startServer(port, deviceId);
      boolean isPortInUse = serverPortAllocator.isServerPortInUse(port);

      if (!isServerStarted || !isPortInUse) {
        LOG.error(
            "Failed to start netconf server for deviceID: {}, at port:{} , isServerStarted={} ,isPortInUse={}",
            deviceId, port, isServerStarted, isPortInUse);
        return null;
      }

      // save the record in db
      entity = new NetConfServerDetailsEntity();
      entity.setDeviceId(deviceId);
      entity.setListenPort(port);
      entity.setEnodeBName(enodeBName);
      netconfDAO.save(entity);

      result = getNetConfServerDetails(deviceId, entity);
      LOG.debug("Successfully started netconf server for deviceID= {}, port={}", deviceId, port);

    } catch (ServerPortAllocationException e) {
      LOG.error("Failed to allocate a port {}", e.toString());
    }

    if (entity != null) {
      result.setDeviceId(deviceId);
      result.setListenPort(entity.getListenPort());
      String netconfListenAddress = getServiceHost();
      if (netconfListenAddress == null) {
        netconfListenAddress = config.getNetconfServerIP();
      }
      result.setListenAddress(netconfListenAddress);
      result.setError(NetconfServerManagementError.SUCCESS);
    }
    return result;
  }

  public void restartServersOnStartup(List<NetConfServerDetailsEntity> serverDetailsList) {

    List<NetConfServerDetailsEntity> startedServers = new ArrayList<>();
    for (NetConfServerDetailsEntity entity : serverDetailsList) {

      serverPortAllocator.checkAndReserveServerPort(entity.getListenPort());

      serverPortAllocator.isServerPortInUse(entity.getListenPort());
      boolean isServerStarted =
          ncServerStarter.startServer(entity.getListenPort(), entity.getDeviceId());

      if (isServerStarted) {
        LOG.info("Successfully restarted NETCONF server {}  on port {}  upon application startup.",
            entity.getDeviceId(), entity.getListenPort());
        // we need to push the pnfEntry for IP updated
        NetConfServerDetails details = getNetConfServerDetails(entity.getDeviceId(), entity);

        final String baseUrl = config.getMapperPath() + "/registerNetconfServer";
        URI uri = null;
        try {
          uri = new URI(baseUrl);
        } catch (URISyntaxException e) {
          LOG.error("error while contructing the URI {}", e.toString());
        }
        RestTemplate restTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();
        HttpEntity<NetConfServerDetails> httpentity = new HttpEntity<>(details, headers);
        boolean isSuccess = false;
        if (uri != null) {
          isSuccess = restTemplate.postForObject(uri, httpentity, Boolean.class);
        }

        if (!isSuccess) {
          LOG.error("Netconf Register request is failed update the updated host details..");
        } else {
          LOG.debug("successfully started the server");
        }
        startedServers.add(entity);
      } else {
        LOG.error("Failed to restart NETCONF server {}  on port {}  upon application startup.",
            entity.getDeviceId(), entity.getListenPort());
      }
    }
    if (!serverDetailsList.isEmpty()) {
      serverDetailsList.removeAll(startedServers);
    }
  }

  private NetConfServerDetails getNetConfServerDetails(String deviceId,
      NetConfServerDetailsEntity entity) {
    NetConfServerDetails result = new NetConfServerDetails();
    result.setDeviceId(deviceId);
    result.setListenPort(entity.getListenPort());
    result.setEnodeBName(entity.getEnodeBName());
    String netconfListenAddress = getServiceHost();
    if (netconfListenAddress == null) {
      netconfListenAddress = config.getNetconfServerIP();
    }
    result.setListenAddress(netconfListenAddress);
    result.setError(NetconfServerManagementError.SUCCESS);
    return result;
  }

  public boolean unregister(String deviceId, String enodeBName) {
    LOG.debug("Unregister is not yet supported deviceId={} enodeBName={}", deviceId, enodeBName);
    return false;
  }

  public List<NetConfServerDetails> getServersInfo() {
    Iterable<NetConfServerDetailsEntity> serverEntities = netconfDAO.findAll();
    String netconfListenAddress = getServiceHost();
    if (netconfListenAddress == null) {
      netconfListenAddress = config.getNetconfServerIP();
    }
    List<NetConfServerDetails> result = new ArrayList<>();

    for (NetConfServerDetailsEntity entity : serverEntities) {
      NetConfServerDetails server = new NetConfServerDetails();
      server.setDeviceId(entity.getDeviceId());
      server.setEnodeBName(entity.getEnodeBName());
      server.setError(NetconfServerManagementError.SUCCESS);
      server.setListenAddress(netconfListenAddress);
      server.setListenPort(entity.getListenPort());
      result.add(server);
    }
    return result;
  }

  private String getServiceHost() {
    Map<String, String> envs = System.getenv();
    for (Entry<String, String> entry : envs.entrySet()) {
      if (entry.getKey() != null && entry.getKey().endsWith("_NETCONF_SERVICE_SERVICE_HOST")) {
        return entry.getValue();
      }
    }
    return null;
  }
}
