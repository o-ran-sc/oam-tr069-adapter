package org.commscope.tr069adapter.mapper.ves;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/tr069MapperVesNBI")
public class VesRequestReceiver {

  private static final Logger LOG = LoggerFactory.getLogger(VesRequestReceiver.class);

  @Autowired
  VesRequestHandler handler;

  @PostMapping("/checkDeviceConnectivity")
  public DeviceRPCResponse deviceConnectivityCheckRequest(@RequestBody DeviceRPCRequest request) {
    LOG.info("Received request for Device Reachability check. Request : {}", request);
    DeviceRPCResponse response = handler.handleDeviceConnectivityRequest(request);
    LOG.info("Received request for Device Reachability check. Response : {}", response);
    return response;
  }
}
