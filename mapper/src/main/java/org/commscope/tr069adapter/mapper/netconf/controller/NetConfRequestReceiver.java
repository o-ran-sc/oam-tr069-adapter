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

package org.commscope.tr069adapter.mapper.netconf.controller;

import org.commscope.tr069adapter.mapper.model.NetConfRequest;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.netconf.NetConfRequestHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/tr069MapperNBI")
public class NetConfRequestReceiver {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfRequestReceiver.class);

  @Autowired
  NetConfRequestHandler handler;

  @PostMapping("/setConfig")
  public NetConfResponse setConfigRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for SET-CONFIG. Request : {}", request);
    NetConfResponse response = handler.handleSetConfigRequest(request);
    LOG.info("Processed SET-CONFIG request. Response : {}", response);
    return response;
  }

  @PostMapping("/get")
  public NetConfResponse getRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for GET. Request : {}", request);
    NetConfResponse response = handler.handleGetRequest(request);
    LOG.info("Processed GET request. Response : {}", response);
    return response;
  }

  @PostMapping("/getConfig")
  public NetConfResponse getConfigRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for GET-CONFIG. Request : {}", request);
    NetConfResponse response = handler.handleGetConfigRequest(request);
    LOG.info("Processed GET-CONFIG request. Response : {}", response);
    return response;
  }

  @PostMapping("/delConfig")
  public NetConfResponse delConfigRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for DEL-CONFIG. Request : {}", request);
    NetConfResponse response = handler.handleDelConfigRequest(request);
    LOG.info("Processed DEL-CONFIG request. Response : {}", response);
    return response;
  }

  @PostMapping("/softwareDowload")
  public NetConfResponse swDownloadRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for SW-DOWNLOAD. Request : {}", request);
    NetConfResponse response = handler.handleSWDownloadRequest(request);
    LOG.info("Processed SW-DOWNLOAD request. Response : ");
    return response;
  }

  @PostMapping("/softwareActivate")
  public NetConfResponse swActivateRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for SW-ACTIVATE. Request : {}", request);
    LOG.info("Processed SW-ACTIVATE request. Response : ");
    return null;
  }

  @PostMapping("/registerNetconfServer")
  public boolean handelRegisterEvent(@RequestBody NetConfServerDetails request) {
    LOG.info("Received request for register event. Request : {}", request);
    boolean result = handler.handelRegisterEvent(request);
    LOG.info("Processed register event request. Response : {}", request);
    return result;
  }

  @PostMapping("/addobject")
  public NetConfResponse addObjectRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for addObject. Request : {}", request);
    NetConfResponse response = handler.handleAddObjectRequest(request);
    LOG.info("Processed addObject request. Response : {}", response);
    return response;
  }

  @PostMapping("/deleteobject")
  public NetConfResponse deleteObjectRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for deleteObject. Request : {}", request);
    NetConfResponse response = handler.handleDeleteObjectRequest(request);
    LOG.info("Processed deleteObject request. Response : {}", response);
    return response;
  }

  @PostMapping("/reboot")
  public NetConfResponse rebootRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for Reboot. Request : {}", request);
    NetConfResponse response = handler.handleRequestWithoutInputParams(request);
    LOG.info("Processed Reboot request. Response : {}", response);
    return response;
  }

  @PostMapping("/reset")
  public NetConfResponse resetRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for Reset. Request : {}", request);
    NetConfResponse response = handler.handleRequestWithoutInputParams(request);
    LOG.info("Processed Reset request. Response : {}", response);
    return response;
  }

  @PostMapping("/spaobject")
  public NetConfResponse spaObjectRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for SPAObject. Request : {}", request);
    NetConfResponse response = handler.handleSPAObjectRequest(request);
    LOG.info("Processed SPAObject request. Response : {}", response);
    return response;
  }

  @PostMapping("/gpaobject")
  public NetConfResponse gpaObjectRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for GPAObject. Request : {}", request);
    NetConfResponse response = handler.handleGPAObjectRequest(request);
    LOG.info("Processed GPAObject request. Response : {}", response);
    return response;
  }

  @PostMapping("/connectionstatus")
  public NetConfResponse connectionStatusRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for Connection Status. Request : {}", request);
    NetConfResponse response = handler.handleConnectionStatusRequest(request);
    LOG.info("Processed Connection Status request. Response : {}", response);
    return response;
  }

  @PostMapping("/download")
  public NetConfResponse downloadRequest(@RequestBody NetConfRequest request) {
    LOG.info("Received request for download. Request : {}", request);
    NetConfResponse response = handler.handleDownloadRequest(request);
    LOG.info("Processed download request. Response : {}", response);
    return response;
  }

}
