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

  private static Map<Long, DeviceRPCResponse> opResultMap = new HashMap<>();
  private static Map<Long, Semaphore> semaphoreMap = new HashMap<>();

  @Autowired
  ACSRequestSender tr069RequestSender;

  @Autowired
  MapperConfigProperties config;

  public DeviceRPCResponse performDeviceOperation(DeviceRPCRequest deviceRPCRequest) {
    Long opId = tr069RequestSender.sendRequest(deviceRPCRequest);
    if (null == opId) {
      LOG.error("Request could not be sent. opId is null");
      return null;
    }
    boolean isSuccess = false;
    try {
      isSuccess = waitForResult(opId);
    } catch (InterruptedException e) {
      LOG.debug(
          "InterruptedException while waiting for tr069 operation result for operation request {}",
          deviceRPCRequest);
      LOG.error("Exception : {}", e.getMessage());
      Thread.currentThread().interrupt();
    }
    DeviceRPCResponse result = null;
    if (!isSuccess) {
      LOG.error("Request got timed out.");
    } else {
      result = getOperationResult(opId);
      LOG.debug("Received operation result for opId = {} GET-CONFIG : {}", opId, result);

    }
    return result;

  }

  public void notifyResult(DeviceRPCResponse opResult) {
    opResultMap.put(opResult.getOperationId(), opResult);
    Semaphore mutex = semaphoreMap.remove(opResult.getOperationId());
    mutex.release();
  }

  private DeviceRPCResponse getOperationResult(long opId) {
    return opResultMap.remove(opId);
  }

  private boolean waitForResult(long opId) throws InterruptedException {
    LOG.debug("Waiting for operation result for opId : {}", opId);
    Semaphore semaphore = new Semaphore(0);
    semaphoreMap.put(opId, semaphore);
    LOG.debug("Semaphore MAP size = {}", semaphoreMap.size());
    LOG.debug("opResultMap MAP size = {}", opResultMap.size());
    Integer timeout = 0;
    if (null != config.getRequesTimeout()) {
      timeout = Integer.valueOf(config.getRequesTimeout());
    }
    return semaphore.tryAcquire(timeout, TimeUnit.SECONDS);
  }

}
