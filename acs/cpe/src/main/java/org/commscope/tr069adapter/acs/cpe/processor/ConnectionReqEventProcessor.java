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

package org.commscope.tr069adapter.acs.cpe.processor;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CONNECTION_REQUEST;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CR_TIMEOUT;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CR_TIMEOUT_CALLBACK;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.HTTP_OP_FAILED;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SEPERATOR;

import java.io.IOException;

import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.requestprocessor.service.TR069DeviceEventHandler;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.cpe.utils.DeviceConnector;
import org.commscope.tr069adapter.common.timer.TimerException;
import org.commscope.tr069adapter.common.timer.TimerServiceManagerAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConnectionReqEventProcessor {

  private static final Logger logger = LoggerFactory.getLogger(ConnectionReqEventProcessor.class);

  @Autowired
  private DeviceConnector deviceConnector;

  @Autowired
  private TimerServiceManagerAPI timerServiceManagerAPI;

  @Autowired
  TR069DeviceEventHandler tr069EventHandler;

  public void initiateConnectionRequest(TR069DeviceDetails tr069DeviceDetails, Boolean isRetry)
      throws SessionManagerException, IOException {
    DeviceRPCResponse deviceRPCResponse = null;

    logger.info("Initiating connection request on the device. Connection request URL is : {}",
        tr069DeviceDetails.getConnectionRequestURL());
    boolean isConnectionFailed = true;
    try {
      deviceRPCResponse = deviceConnector.requestConnectionHttp(tr069DeviceDetails);
    } catch (Exception e) {
      logger.error("Connection Failed with the Device: {}", e.getMessage());
      isConnectionFailed = false;
    }

    if (!isConnectionFailed
        || deviceRPCResponse.getOperationResponse().getStatus() == HTTP_OP_FAILED) {
      tr069DeviceDetails.setCrRetryCount(tr069DeviceDetails.getCrRetryCount() + 1);
      String faultStr = "";
      if (deviceRPCResponse != null) {
        faultStr = deviceRPCResponse.getFaultString();
      }
      logger.warn("Connection request failed with device, Error: {}, on attempt- {}", faultStr,
          tr069DeviceDetails.getCrRetryCount());
      if (tr069DeviceDetails.getCrRetryCount() == 1) {
        logger.error("CONNECT request is failed, not retrying the CR to device");
      } else {
        SessionManagerException e = new SessionManagerException(ErrorCode.SESSION_INITIATION_FAILED,
            tr069DeviceDetails.getDeviceId());
        logger.error(e.getMessage());
        throw e;
      }
    } else {
      onSuccessHTTPGetOperation(tr069DeviceDetails, isRetry);
    }
  }

  private void onSuccessHTTPGetOperation(TR069DeviceDetails tr069DeviceDetails, Boolean isRetry) {
    try {
      String timerId = tr069DeviceDetails.getDeviceId() + SEPERATOR + CONNECTION_REQUEST;
      if (isRetry.booleanValue()) {
        timerServiceManagerAPI.modifyTimer(timerId, CR_TIMEOUT, tr069DeviceDetails);
      } else {
        timerServiceManagerAPI.startTimer(timerId, CR_TIMEOUT_CALLBACK, CR_TIMEOUT,
            tr069DeviceDetails);
      }
      logger.debug(
          "Successfully started the timer task for connection request initiation on device : {}",
          tr069DeviceDetails.getDeviceId());
    } catch (TimerException e) {
      logger.error("Couldn't start the timer task for connection request initiation on device : {}",
          tr069DeviceDetails.getDeviceId());
    }
  }

  public DeviceConnector getDeviceConnector() {
    return deviceConnector;
  }

  public void setDeviceConnector(DeviceConnector deviceConnector) {
    this.deviceConnector = deviceConnector;
  }

  public TimerServiceManagerAPI getTimerServiceManagerAPI() {
    return timerServiceManagerAPI;
  }

  public void setTimerServiceManagerAPI(TimerServiceManagerAPI timerServiceManagerAPI) {
    this.timerServiceManagerAPI = timerServiceManagerAPI;
  }

}
