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
