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

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.dto.CustomOperationCode;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.vesagent.util.VesAgentConstants;
import org.commscope.tr069adapter.vesagent.util.VesAgentUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class WaitForNotifications {

  private static final Logger LOG = LoggerFactory.getLogger(WaitForNotifications.class);

  private static Map<String, Future<DeviceRPCResponse>> opFutureMap = new HashMap<>();
  private static Map<String, DeviceRPCResponse> opResultMap = new HashMap<>();
  private static Map<String, Semaphore> semaphoreMap = new HashMap<>();

  public void notifyDeviceNotification(VESNotification notification) {
    String deviceOperationKey = VesAgentUtils.getDeviceOperationKey(
        notification.getDevnotification().getDeviceDetails().getDeviceId(),
        CustomOperationCode.CONNECT);

    if (!semaphoreMap.containsKey(deviceOperationKey)) {
      return;
    }

    DeviceRPCResponse response = new DeviceRPCResponse();
    response.setDeviceDetails(notification.getDevnotification().getDeviceDetails());

    OperationResponse operationResponse = new OperationResponse();
    operationResponse.setStatus(VesAgentConstants.DEVICE_IS_REACHABLE);
    operationResponse.setOperationCode(CustomOperationCode.CONNECT);

    response.setOperationResponse(operationResponse);

    opResultMap.put(deviceOperationKey, response);
    Semaphore mutex = semaphoreMap.remove(deviceOperationKey);
    mutex.release();
  }


  public void notifyResult(DeviceRPCResponse opResult) {
    String deviceOperationKey =
        VesAgentUtils.getDeviceOperationKey(opResult.getDeviceDetails().getDeviceId(),
            opResult.getOperationResponse().getOperationCode());

    if (!semaphoreMap.containsKey(deviceOperationKey)) {
      return;
    }

    opResultMap.put(deviceOperationKey, opResult);
    Semaphore mutex = semaphoreMap.remove(deviceOperationKey);
    mutex.release();
  }

  public DeviceRPCResponse getOperationResult(String deviceId, OperationCode opCode) {
    return opResultMap.remove(VesAgentUtils.getDeviceOperationKey(deviceId, opCode));
  }

  public boolean waitForResult(String deviceId, OperationCode opCode,
      Future<DeviceRPCResponse> futureResponse, long timeout) throws InterruptedException {
    LOG.debug("Waiting for operation result for device:{}, operation: {}", deviceId, opCode);

    String deviceOperationKey = VesAgentUtils.getDeviceOperationKey(deviceId, opCode);
    opFutureMap.put(deviceOperationKey, futureResponse);

    Semaphore semaphore = new Semaphore(0);
    semaphoreMap.put(deviceOperationKey, semaphore);

    LOG.debug("Semaphore MAP size = {}", semaphoreMap.size());
    LOG.debug("opResultMap MAP size = {}", opResultMap.size());
    LOG.debug("opFutureMap MAP size = {}", opFutureMap.size());

    return semaphore.tryAcquire(timeout, TimeUnit.SECONDS);
  }

  public void stopOperation(String deviceId, OperationCode opCode) {
    LOG.debug("Stopping waiting for operation result thread for device:{}, operation: {}", deviceId,
        opCode);

    Future<DeviceRPCResponse> operationInstance =
        opFutureMap.remove(VesAgentUtils.getDeviceOperationKey(deviceId, opCode));

    if (null != operationInstance) {
      LOG.info("Stopping operation result waiting thread for operation : {}", operationInstance);
      operationInstance.cancel(true);
    }
  }
}
