package org.commscope.tr069adapter.vesagent.mapper;

import java.util.concurrent.Future;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.async.WaitForNotifications;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
// @EnableAsync
public class MapperRequestSender {
  private static final Logger LOG = LoggerFactory.getLogger(MapperRequestSender.class);
  private RestTemplate restTemplate = new RestTemplate();

  @Autowired
  VesConfiguration config;

  @Autowired
  WaitForNotifications waitForNotifications;

  // public DeviceRPCResponse sendRequest(DeviceRPCRequest deviceRPCRequest) {
  // return restTemplate.postForObject(config.getMapperPath(), deviceRPCRequest,
  // DeviceRPCResponse.class);
  // }

  @Async
  public Future<DeviceRPCResponse> sendRequest(DeviceRPCRequest deviceRPCRequest) {
    LOG.info("Sending device connectivity request to ACS for device {}",
        deviceRPCRequest.getDeviceDetails().getDeviceId());
    DeviceRPCResponse response = restTemplate.postForObject(config.getMapperPath(),
        deviceRPCRequest, DeviceRPCResponse.class);

    waitForNotifications.notifyResult(response);

    return new AsyncResult<>(response);
  }

}
