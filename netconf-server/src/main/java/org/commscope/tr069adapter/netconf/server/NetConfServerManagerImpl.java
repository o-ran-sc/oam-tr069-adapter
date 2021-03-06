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

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.apache.commons.io.FileUtils;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.common.deviceversion.DeviceVersionManager;
import org.commscope.tr069adapter.common.deviceversion.ProfileDefinition;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.NetconfServerManagementError;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.commscope.tr069adapter.netconf.dao.NetConfServerDetailsRepository;
import org.commscope.tr069adapter.netconf.entity.NetConfServerDetailsEntity;
import org.commscope.tr069adapter.netconf.error.RetryFailedException;
import org.commscope.tr069adapter.netconf.error.ServerPortAllocationException;
import org.commscope.tr069adapter.netconf.server.helper.ServerPortAllocationHelper;
import org.commscope.tr069adapter.netconf.server.utils.NetConfServerConstants;
import org.commscope.tr069adapter.netconf.server.ves.VESNotificationSender;
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

  @Autowired
  VESNotificationSender vesNotificationSender;

  @Autowired
  DeviceVersionManager versionManager;

  ExecutorService executorService = Executors.newFixedThreadPool(10);

  private static final String ENODEBNAME = "enodeBName";

  private static final String FAIL_DEVICE_UNREGISTER = "Failed to unregister the device ";
  
  public boolean loadSchemas() {
    LOG.debug("Loading yang schema started");
    List<ProfileDefinition> profiles = versionManager.getSupportedProfileDefinitions();
    try {
      String commonSchemaPath = config.getSchemaDirPath() + "/common";

      for (ProfileDefinition profile : profiles) {
        String verSpecificSchemaPath =
            config.getSchemaDirPath() + File.separator + profile.getNetConfSchemaPath();
        File schemaDir = new File(commonSchemaPath);
        File schemaVerDir = new File(verSpecificSchemaPath);

        if (!schemaVerDir.isDirectory()) {
          LOG.error("No folder path found for given version path {}",
              schemaVerDir.getAbsolutePath());
          return false;
        }

        copyDir(schemaDir, schemaVerDir);
        boolean isSchemaLoaded = ncServerStarter.loadSchemas(schemaVerDir);
        if (!isSchemaLoaded) {
          LOG.debug("Failed to load schema for profile {}", profile.getProfileId());
          return false;
        }
      }
    } catch (Exception e) {
      LOG.error("Load schemas failed in netconf server {}", e.getMessage());
      return false;
    }
    LOG.debug("Loading yang schema completed");
    return true;
  }

private void copyDir(File schemaDir, File schemaVerDir) {
    try {
      FileUtils.copyDirectory(schemaDir, schemaVerDir);
    } catch (IOException e) {
      LOG.error("Failed to copy directory {} ", e.getMessage());
    }
  }
  
  public void restartServers() {
    LOG.debug("Restarting all netconf servers during startup...");
    Iterable<NetConfServerDetailsEntity> entities = netconfDAO.findAll();

    for (NetConfServerDetailsEntity entity : entities) {
      boolean isReserved = serverPortAllocator.checkAndReserveServerPort(entity.getListenPort());
      if (isReserved) {
        ServerStartTask task = new ServerStartTask(entity, this);
        executorService.execute(task);
      } else {
        try {
          restartServersHandler.restart(entity);
        } catch (RetryFailedException e) {
          LOG.error("Error while restarting netconf servers {}",e.getMessage());
        }
      }
    }
    LOG.debug("Restarting netconf servers during startup is completed.");
  }

  public NetConfServerDetails createServer(String deviceId, String enodeBName, String swVersion,
      String hwVersion) {
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

    if (null != entity && ncServerStarter.isNetConfServerRunning(deviceId)) {
      if (isVersionChanged(entity, swVersion, hwVersion)) {
        return restartOnVersionChange(deviceId, enodeBName, swVersion, hwVersion);
      }

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
      if (port == null) {
        result.setError(NetconfServerManagementError.PORT_NOT_AVAILBLE);
        LOG.error(
            "All ports are exhausted. Hence cannot allocate a port to start new netconf server");
        return result;
      } else {
        LOG.debug("Successfully reserved a port for deviceID={} ,port={}", deviceId, port);

        // start the server
        boolean isServerStarted = ncServerStarter.startServer(port, deviceId, swVersion, hwVersion);
        boolean isPortInUse = serverPortAllocator.isServerPortInUse(port);

        if (!isServerStarted || !isPortInUse) {
          LOG.error(
              "Failed to start netconf server for deviceID: {}, at port:{} , isServerStarted={} ,isPortInUse={}",
              deviceId, port, isServerStarted, isPortInUse);
          result.setError(NetconfServerManagementError.PORT_IN_USE);
          return result;
        }
      }

      // save the record in db
      entity = new NetConfServerDetailsEntity();
      entity.setDeviceId(deviceId);
      entity.setListenPort(port);
      entity.setEnodeBName(enodeBName);
      entity.setSwVersion(swVersion);
      entity.setHwVersion(hwVersion);
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

  public NetConfServerDetails restartOnVersionChange(String deviceId, String enodeBName,
      String swVersion, String hwVersion) {

    NetConfServerDetailsEntity entity = null;
    if (deviceId != null) {
      entity = netconfDAO.findByDeviceId(deviceId);
    }
    if (entity == null) {
      return null;
    }

    boolean isVersionChanged = isVersionChanged(entity, swVersion, hwVersion);
    if (isVersionChanged) {
      LOG.debug("software version changed, stopping the the existing netconf instance");
      boolean result = this.ncServerStarter.stopServer(deviceId);
      if (result) {
        LOG.debug(
            "successfully stopped the netconf instance; trying to start with new version yang models");
        entity.setSwVersion(swVersion);
        entity.setHwVersion(hwVersion);
        netconfDAO.save(entity);

        boolean isSuccess = startNetConfServerInstance(entity);

        if (!isSuccess) {
          try {
            restartServersHandler.restart(entity);
          } catch (RetryFailedException e) {
            LOG.debug("");
          }
        }
      }
    }
    return getNetConfServerDetails(deviceId, entity);
  }


  public boolean startNetConfServerInstance(NetConfServerDetailsEntity entity) {
    boolean isSuccess = false;

    boolean isServerStarted = ncServerStarter.startServer(entity.getListenPort(),
        entity.getDeviceId(), entity.getSwVersion(), entity.getHwVersion());
    if (isServerStarted) {
      LOG.info("Successfully restarted NETCONF server {}  on port {} .",
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
      if (uri != null) {
        isSuccess = restTemplate.postForObject(uri, httpentity, Boolean.class);
      }

      if (!isSuccess) {
        LOG.error("Netconf Register request is failed update the updated host details..");
      } else {
        LOG.debug("successfully started the server");
      }
    } else {
      LOG.error("Failed to restart NETCONF server {}  on port {}  upon application startup.",
          entity.getDeviceId(), entity.getListenPort());
    }
    return isSuccess;
  }

  private NetConfServerDetails getNetConfServerDetails(String deviceId,
      NetConfServerDetailsEntity entity) {
    NetConfServerDetails result = new NetConfServerDetails();
    result.setDeviceId(deviceId);
    result.setListenPort(entity.getListenPort());
    result.setEnodeBName(entity.getEnodeBName());
    result.setSwVersion(entity.getSwVersion());
    result.setHwVersion(entity.getHwVersion());
    String netconfListenAddress = getServiceHost();
    if (netconfListenAddress == null) {
      netconfListenAddress = config.getNetconfServerIP();
    }
    result.setListenAddress(netconfListenAddress);
    result.setError(NetconfServerManagementError.SUCCESS);
    return result;
  }

  public String unregister(String deviceId, String enodeBName) {
    String resultMsg = null;
    NetConfServerDetailsEntity entity = null;
    if (deviceId != null) {
      entity = this.netconfDAO.findByDeviceId(deviceId);
    } else if (enodeBName != null) {
      entity = this.netconfDAO.findByEnodeBName(enodeBName);
    } else {
      LOG.error(
          "Both deviceID and enodeBName are null. Hence failed to unregister the netconf server.");
      resultMsg = FAIL_DEVICE_UNREGISTER + deviceId + ", " + ENODEBNAME + "=" + enodeBName
              + ". Invalid deviceId/enodeBName specified.";
      LOG.info(resultMsg);
    }
    if (entity == null) {
      resultMsg = FAIL_DEVICE_UNREGISTER + deviceId + ", " + ENODEBNAME + "=" + enodeBName
              + ". Invalid deviceId/enodeBName specified.";
      LOG.info(resultMsg);
      return resultMsg;
    }
    boolean result = this.ncServerStarter.stopServer(deviceId);
    if (result) {
      resultMsg =
          "Successfully unregistered the device " + deviceId + " and enodeBName=" + enodeBName;
      this.serverPortAllocator.unReserveServerPort(entity.getListenPort());
      this.netconfDAO.delete(entity);
      LOG.info(resultMsg);
      delteHeartBeatTimer(deviceId);
    } else {
      resultMsg = FAIL_DEVICE_UNREGISTER + deviceId + ", " + ENODEBNAME + "=" + enodeBName;
      LOG.error(resultMsg);
    }

    return resultMsg;
  }

  private void delteHeartBeatTimer(String deviceId) {
    VESNotification vesNotification = new VESNotification();

    vesNotification.seteNodeBName(deviceId);

    ParameterDTO paramDTO = new ParameterDTO();
    paramDTO.setParamName(NetConfServerConstants.HEART_BEAT);

    List<ParameterDTO> paramDTOList = new ArrayList<>();
    paramDTOList.add(paramDTO);

    OperationDetails opDetails = new OperationDetails();
    opDetails.setOpCode(TR069OperationCode.DELETE_OBJECT);
    opDetails.setParmeters(paramDTOList);

    vesNotification.setOperationDetails(opDetails);

    VESNotificationResponse response =
        vesNotificationSender.sendDeleteConfigNotification(vesNotification);

    if (response.getStatusCode() == NetConfServerConstants.SUCCESS) {
      LOG.info("Heart beat timer is deleted successfully for device {}", deviceId);
    } else {
      LOG.error("Failed to delete heart beat timer for device {}. ErrorMsg : {}", deviceId,
          response.getResponseMsg());
    }

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
      server.setSwVersion(entity.getSwVersion());
      server.setHwVersion(entity.getHwVersion());
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

  private boolean isVersionChanged(NetConfServerDetailsEntity entity, String swVersion,
      String hwVersion) {
    String existingProfileId =
        versionManager.getAssociatedProfileId(entity.getSwVersion(), entity.getHwVersion());
    String newProfiled = versionManager.getAssociatedProfileId(swVersion, hwVersion);
    return !existingProfileId.equalsIgnoreCase(newProfiled) ? true : false;
  }

  class ServerStartTask implements Runnable {

    NetConfServerDetailsEntity entity;
    NetConfServerManagerImpl netconfServerManager;

    public ServerStartTask(NetConfServerDetailsEntity entity,
        NetConfServerManagerImpl netconfServerManager) {
      this.entity = entity;
      this.netconfServerManager = netconfServerManager;
    }

    @Override
    public void run() {
      boolean isSuccess = netconfServerManager.startNetConfServerInstance(entity);
      if (!isSuccess) {
        try {
          netconfServerManager.restartServersHandler.restart(entity);
        } catch (RetryFailedException e) {
          LOG.debug("");
        }
      }
    }

  }
}
