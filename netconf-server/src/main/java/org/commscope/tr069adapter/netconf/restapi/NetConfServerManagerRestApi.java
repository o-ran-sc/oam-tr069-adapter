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

package org.commscope.tr069adapter.netconf.restapi;

import java.util.List;

import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.netconf.server.NetConfServerManagerImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/netConfServerManagerService")
public class NetConfServerManagerRestApi {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfServerManagerRestApi.class);

  @Autowired
  NetConfServerManagerImpl manager;

  @PostMapping("/createServer")
  public NetConfServerDetails createNetConfServerInstance(@RequestParam String deviceId,
      @RequestParam String enodeBName, @RequestParam String swVersion,
      @RequestParam String hwVersion) {
    LOG.info("Received Create NetConf Server request for deviceID: {}, enodeBName: {}, swversion: {}", deviceId,
        enodeBName, swVersion);
    NetConfServerDetails serverDetails =
        manager.createServer(deviceId, enodeBName, swVersion, hwVersion);
    LOG.info("Successfully processed NetConf Server wit server details : {}", serverDetails);
    return serverDetails;
  }

  @PostMapping("/restartOnVersionChange")
  public NetConfServerDetails restartOnVersionChange(@RequestParam String deviceId,
      @RequestParam String enodeBName, @RequestParam String swVersion,
      @RequestParam String hwVersion) {
    LOG.info("Received Create NetConf Server request for deviceID: {}, enodeBName: {}", deviceId,
        enodeBName);
    NetConfServerDetails serverDetails =
        manager.restartOnVersionChange(deviceId, enodeBName, swVersion, hwVersion);
    LOG.info("Successfully processed NetConf Server wit server details : {}", serverDetails);
    return serverDetails;
  }
  
  @GetMapping("/listServers")
  public List<NetConfServerDetails> listNetConfServersInfo() {
    LOG.info("Received request to list all NetConf Servers information");
    List<NetConfServerDetails> serverDetails = manager.getServersInfo();
    LOG.info("Successfully processed request to list all NetConf Servers information");
    return serverDetails;
  }

  @PostMapping("/unregisterServer")
  public String unregisterNetConfServerInstance(@RequestParam String deviceId,
      @RequestParam String enodeBName) {
    LOG.info("Received request for Unregister NetConf Server for deviceID: {}, enodeBName: {} ",
        deviceId, enodeBName);
    String result = manager.unregister(deviceId, enodeBName);
    LOG.info("Unregister request is processed. NetConf Server for deviceID: {} , unregisted= {}",
        deviceId, result);
    return result;
  }

}
