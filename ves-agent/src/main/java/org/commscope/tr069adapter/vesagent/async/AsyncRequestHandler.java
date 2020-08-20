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

package org.commscope.tr069adapter.vesagent.async;

import java.util.concurrent.Future;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.OperationOptions;
import org.commscope.tr069adapter.acs.common.dto.CustomOperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.controller.HeartBeatMessageHandler;
import org.commscope.tr069adapter.vesagent.entity.DeviceDataEntity;
import org.commscope.tr069adapter.vesagent.mapper.MapperRequestSender;
import org.commscope.tr069adapter.vesagent.util.VesAgentConstants;
import org.commscope.tr069adapter.vesagent.util.VesAgentUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

/**
 * 
 * @version 1.0
 * @since June 12, 2020
 * @author Prashant Kumar
 */
@Component
public class AsyncRequestHandler {

  private static final Logger LOG = LoggerFactory.getLogger(AsyncRequestHandler.class);

  @Autowired
  MapperRequestSender mapperRequestSender;

  @Autowired
  WaitForNotifications waitForNotifications;

  @Autowired
  HeartBeatMessageHandler heartBeatMessageHandler;

  @Autowired
  VesConfiguration config;

  public DeviceRPCResponse performDeviceOperation(DeviceRPCRequest deviceRPCRequest) {
    LOG.info("Initiating device connectivity request to ACS for device {}",
        deviceRPCRequest.getDeviceDetails().getDeviceId());

    Future<DeviceRPCResponse> futureResponse = mapperRequestSender.sendRequest(deviceRPCRequest);
    if (null == futureResponse) {
      LOG.error("Request could not be sent. response is null");
      return null;
    }

    DeviceRPCResponse response = null;

    OperationCode opCode = deviceRPCRequest.getOpDetails().getOpCode();
    String deviceId = deviceRPCRequest.getDeviceDetails().getDeviceId();
    long timeOut = getOperationTimeOut(deviceRPCRequest.getOptions().getExecutionTimeout());

    try {
      waitForNotifications.waitForResult(deviceId, opCode, futureResponse, timeOut);
      response = waitForNotifications.getOperationResult(deviceId, opCode);

      if (null == response) {
        LOG.error("Request got timed out.");
      } else {
        LOG.debug("Received operation result for device : {}, operation = {} as {}", deviceId,
            opCode, response);
      }
      waitForNotifications.stopOperation(deviceId, opCode);

    } catch (InterruptedException e) {
      LOG.debug(
          "InterruptedException while waiting for mapper operation result for device : {}, operation : {} request.",
          deviceId, opCode);
      Thread.currentThread().interrupt();
    }

    return response;
  }

  private long getOperationTimeOut(long timeOut) {
    if (timeOut > 0) {
      return timeOut;
    }

    if (null != config.getRequestTimeout()) {
      timeOut = Long.valueOf(config.getRequestTimeout());
    }

    return timeOut;
  }

  @Async("threadPoolTaskExecutor1")
  public void initiateDeviceReachabilityCheck(DeviceDataEntity deviceDataEntity) {
    deviceDataEntity.setStartEpochMicrosec(VesAgentUtils.getStartEpochTime() * 1000);
    DeviceDetails deviceDetails = new DeviceDetails();
    deviceDetails.setDeviceId(deviceDataEntity.getDeviceId());
    deviceDetails.setOui(deviceDataEntity.getOui());
    deviceDetails.setProductClass(deviceDataEntity.getProductClass());

    TR069OperationDetails operationDetails = new TR069OperationDetails();
    operationDetails.setOpCode(CustomOperationCode.CONNECT);

    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();

    deviceRPCRequest.setDeviceDetails(deviceDetails);
    deviceRPCRequest.setOpDetails(operationDetails);

    OperationOptions options = new OperationOptions();
    if (null != config.getRequestTimeout()) {
      options.setExecutionTimeout(Integer.valueOf(config.getRequestTimeout()));
    }

    deviceRPCRequest.setOptions(options);

    DeviceRPCResponse deviceRPCResponse = performDeviceOperation(deviceRPCRequest);

    if (VesAgentUtils.isDeviceReachable(deviceRPCResponse)) {
      LOG.debug("Device {} is reachable.", deviceDataEntity.getDeviceId());
      try {
        LOG.debug("Sending heatbeat event for device {}.", deviceDataEntity.getDeviceId());
        heartBeatMessageHandler.sendHeartBeatEvent(deviceDataEntity, Integer.parseInt(
            deviceDataEntity.getAttributesMap().get(VesAgentConstants.HEART_BEAT_PERIOD)));
      } catch (NumberFormatException e) {
        LOG.error("heartBeatPeriod doesn't have numeric value. ErrorMsg: {}", e.getMessage());
      } catch (Exception e) {
        LOG.error("Error while sending heart beat ves event. ErrorMsg: {}", e.getMessage());
      }
    }
  }
}
