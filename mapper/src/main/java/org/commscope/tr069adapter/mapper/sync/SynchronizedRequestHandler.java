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

package org.commscope.tr069adapter.mapper.sync;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.acs.ACSRequestSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SynchronizedRequestHandler {

  private static final Logger LOG = LoggerFactory.getLogger(SynchronizedRequestHandler.class);

  private static Map<String, DeviceRPCResponse> opResultMap = new HashMap<>();
  private static Map<String, Semaphore> semaphoreMap = new HashMap<>();

  @Autowired
  ACSRequestSender tr069RequestSender;

  @Autowired
  MapperConfigProperties config;

  public DeviceRPCResponse performDeviceOperation(DeviceRPCRequest deviceRPCRequest) {
    Long acsOperationId = tr069RequestSender.sendRequest(deviceRPCRequest);

    if (null == acsOperationId) {
      LOG.error("Request could not be sent. opId is null");
      return null;
    }

    String mapperUniqOperId =
        deviceRPCRequest.getDeviceDetails().getDeviceId() + "_" + acsOperationId;
    LOG.debug("Received operation mapperUniqOperId = {}", mapperUniqOperId);

    boolean isSuccess = false;
    try {
      isSuccess = waitForResult(mapperUniqOperId);
    } catch (InterruptedException e) {
      LOG.debug(
          "InterruptedException while waiting for tr069 operation result for operation request {}",
          deviceRPCRequest);
      LOG.error("Exception : {}", e.getMessage());
      Thread.currentThread().interrupt();
    }
    DeviceRPCResponse result = null;
    if (!isSuccess) {
      LOG.error("Request got timed out for operation {}", mapperUniqOperId);
      semaphoreMap.remove(mapperUniqOperId);
    } else {
      result = getOperationResult(mapperUniqOperId);
      LOG.debug("Received operation result for mapperUniqOperId = {} result : {}", mapperUniqOperId,
          result);
    }
    return result;

  }

  public void notifyResult(DeviceRPCResponse opResult) {
    Semaphore mutex = semaphoreMap
        .remove(opResult.getDeviceDetails().getDeviceId() + "_" + opResult.getOperationId());
    if (mutex != null) {
      opResultMap.put(opResult.getDeviceDetails().getDeviceId() + "_" + opResult.getOperationId(),
          opResult);
      mutex.release();
    }
  }

  private DeviceRPCResponse getOperationResult(String mapperUniqOperId) {
    return opResultMap.remove(mapperUniqOperId);
  }

  private boolean waitForResult(String mapperUniqOperId) throws InterruptedException {
    LOG.debug("Waiting for operation result for mapperUniqOperId : {}", mapperUniqOperId);
    Semaphore semaphore = new Semaphore(0);
    semaphoreMap.put(mapperUniqOperId, semaphore);
    LOG.debug("Semaphore MAP size = {}", semaphoreMap.size());
    LOG.debug("opResultMap MAP size = {}", opResultMap.size());
    Integer timeout = 0;
    if (null != config.getRequesTimeout()) {
      timeout = Integer.valueOf(config.getRequesTimeout());
    }
    return semaphore.tryAcquire(timeout, TimeUnit.SECONDS);
  }
}
