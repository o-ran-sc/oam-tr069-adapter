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

    boolean isSuccess = false;
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

      // if(isSuccess) {
      // response = waitForNotifications.getOperationResult(deviceId, opCode);
      // LOG.debug("Received operation result for device : {}, operation = {} as {}",deviceId,
      // opCode,response);
      //
      // waitForNotifications.stopOperation(deviceId, opCode);
      // }else {
      // LOG.error("Request got timed out.");
      // }
    } catch (InterruptedException e) {
      LOG.debug(
          "InterruptedException while waiting for mapper operation result for device : {}, operation : {} request.",
          deviceId, opCode);
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

  @Async
  public void initiateDeviceReachabilityCheck(DeviceDataEntity deviceDataEntity) {
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
