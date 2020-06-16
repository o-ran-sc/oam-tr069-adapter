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

package org.commscope.tr069adapter.acs.requestprocessor;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.*;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.dto.DeviceOperationRequestDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.exception.DeviceOperationException;
import org.commscope.tr069adapter.acs.common.exception.SessionConcurrentAccessException;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.requestprocessor.service.TR069DeviceEventHandler;
import org.commscope.tr069adapter.acs.common.response.DeviceInformResponse;
import org.commscope.tr069adapter.acs.requestprocessor.impl.TR069RequestProcessEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
public class TR069DeviceEventHandlerImpl implements TR069DeviceEventHandler {

  private static final String RETRY_LIMIT_REACHED_AND_FAILING_THE_DEVICE_UNREGISTER_REQUEST =
      "Retry limit reached and failing the device unregister request";

  private static final Logger logger = LoggerFactory.getLogger(TR069DeviceEventHandlerImpl.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private TR069RequestProcessEngine tr069RequestProcessEngine;

  public TR069RequestProcessEngine getProcessEngine() {
    return tr069RequestProcessEngine;
  }

  public void setProcessEngine(TR069RequestProcessEngine processEngine) {
    this.tr069RequestProcessEngine = processEngine;
  }

  @Override
  @Transactional(isolation = Isolation.DEFAULT, propagation = Propagation.REQUIRED, timeout = 300,
      rollbackFor = RuntimeException.class)
  public DeviceInformResponse processDeviceInform(final DeviceInform deviceNotification)
      throws Exception {
    DeviceInformResponse deviceNotificationResponse = null;
    try {
      String deviceId = deviceNotification.getDeviceDetails().getDeviceId();
      MDC.put(CLIENT_STR, deviceId);
      deviceNotificationResponse =
          processDeviceInformWithRetryOnConcurrentAccess(deviceNotification);
    } finally {
      MDC.remove(CLIENT_STR);
    }

    return deviceNotificationResponse;
  }

  @Override
  @Transactional(isolation = Isolation.DEFAULT, propagation = Propagation.REQUIRED, timeout = 300,
      rollbackFor = RuntimeException.class)
  public DeviceRPCRequest processDeviceRPCResponse(DeviceRPCResponse operationResult)
      throws Exception {
    DeviceRPCRequest deviceRPCRequest = null;
    try {
      String deviceId = operationResult.getDeviceDetails().getDeviceId();
      MDC.put(CLIENT_STR, deviceId);
      deviceRPCRequest = processDeviceRPCResponseWithRetryOnConcurrentAccess(operationResult);
    } finally {
      MDC.remove(CLIENT_STR);
    }
    return deviceRPCRequest;
  }

  @Override
  @Transactional(isolation = Isolation.DEFAULT, propagation = Propagation.REQUIRED, timeout = 300,
      rollbackFor = RuntimeException.class)
  public DeviceRPCRequest processEmptyDeviceRequest(TR069DeviceDetails deviceDetails)
      throws Exception {
    DeviceRPCRequest deviceRPCRequest = null;
    try {
      String deviceId = deviceDetails.getDeviceId();
      MDC.put(CLIENT_STR, deviceId);
      deviceRPCRequest = processEmptyDeviceRequestWithRetryOnConcurrentAccess(deviceDetails);
    } finally {
      MDC.remove(CLIENT_STR);
    }
    return deviceRPCRequest;
  }

  @Override
  @Transactional(isolation = Isolation.DEFAULT, propagation = Propagation.REQUIRED, timeout = 300,
      rollbackFor = RuntimeException.class)
  public DeviceOperationRequestDetails getOpRequestDetailsBySessionId(String sessionId)
      throws SessionManagerException {
    return tr069RequestProcessEngine.getOpRequestDetailsBySessionId(sessionId);
  }

  @Override
  @Transactional(isolation = Isolation.DEFAULT, propagation = Propagation.REQUIRED, timeout = 300,
      rollbackFor = RuntimeException.class)
  public TR069DeviceDetails getDeviceDetails(String deviceId) throws DeviceOperationException {
    return tr069RequestProcessEngine.getDeviceDetails(deviceId);
  }

  /**
   * @param deviceNotification
   * @return
   * @throws InterruptedException
   */
  private DeviceInformResponse processDeviceInformWithRetryOnConcurrentAccess(
      DeviceInform deviceNotification)
      throws SessionConcurrentAccessException, InterruptedException {
    logger.debug("Processing Device Inform Event");
    DeviceInformResponse deviceNotificationResponse = null;
    for (int i = 0; i < MAX_RETRY_LIMIT; i++) {
      try {
        deviceNotificationResponse =
            tr069RequestProcessEngine.processDeviceInform(deviceNotification);
        logger.debug("Successfully processed Device Inform Event");
        break;
      } catch (SessionConcurrentAccessException scae) {
        if ((i + 1) == MAX_RETRY_LIMIT) {
          logger.error(RETRY_LIMIT_REACHED_AND_FAILING_THE_DEVICE_UNREGISTER_REQUEST);
          throw scae;
        }
        Long delay = (i == 0) ? DELAY : DELAY * i;
        Thread.sleep(delay);
      }
    }
    return deviceNotificationResponse;
  }

  /**
   * @param operationResult
   * @return
   * @throws InterruptedException
   * @throws Exception
   */
  private DeviceRPCRequest processDeviceRPCResponseWithRetryOnConcurrentAccess(
      DeviceRPCResponse operationResult)
      throws SessionConcurrentAccessException, InterruptedException {
    DeviceRPCRequest deviceRPCRequest = null;
    logger.debug("Processing Device operation response");
    for (int i = 0; i < MAX_RETRY_LIMIT; i++) {
      try {
        deviceRPCRequest = tr069RequestProcessEngine.processDeviceRPCResponse(operationResult);
        logger.debug("Successfully processed Device operation response");
        break;
      } catch (SessionConcurrentAccessException scae) {
        if ((i + 1) == MAX_RETRY_LIMIT) {
          logger.error(RETRY_LIMIT_REACHED_AND_FAILING_THE_DEVICE_UNREGISTER_REQUEST);
          throw scae;
        }
        Long delay = (i == 0) ? DELAY : DELAY * i;
        Thread.sleep(delay);
      }
    }
    return deviceRPCRequest;
  }

  /**
   * @param deviceDetails
   * @return
   * @throws InterruptedException
   * @throws Exception
   */
  private DeviceRPCRequest processEmptyDeviceRequestWithRetryOnConcurrentAccess(
      TR069DeviceDetails deviceDetails)
      throws SessionConcurrentAccessException, InterruptedException {
    DeviceRPCRequest deviceRPCRequest = null;
    logger.debug("Processing Empty request");
    for (int i = 0; i < MAX_RETRY_LIMIT; i++) {
      try {
        deviceRPCRequest = tr069RequestProcessEngine.processEmptyDeviceRequest(deviceDetails);
        logger.debug("Successfully processed Empty request");
        break;
      } catch (SessionConcurrentAccessException scae) {
        if ((i + 1) == MAX_RETRY_LIMIT) {
          logger.error(RETRY_LIMIT_REACHED_AND_FAILING_THE_DEVICE_UNREGISTER_REQUEST);
          throw scae;
        }
        Long delay = (i == 0) ? DELAY : DELAY * i;
        Thread.sleep(delay);
      }
    }
    return deviceRPCRequest;
  }
}
