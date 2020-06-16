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

package org.commscope.tr069adapter.acs.requestprocessor.impl;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CONNECTION_REQUEST;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.DEVICE_RESPONSE_TIMEOUT;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SEPERATOR;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SESSION_TIMEOUT_CALLBACK_JNDI;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.WAITING_FOR_DEVICE_RESPONSE;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.WAITING_FOR_NBI_RESPONSE;

import java.io.Serializable;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.exception.DeviceOperationException;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepositoryHelper;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionState;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceRPCRequestEntity;
import org.commscope.tr069adapter.acs.requestprocessor.helper.TR069RequestProcessEngineUtility;
import org.commscope.tr069adapter.acs.requestprocessor.util.TR069RequestProcessorUtility;
import org.commscope.tr069adapter.common.timer.TimerException;
import org.commscope.tr069adapter.common.timer.TimerListener;
import org.commscope.tr069adapter.common.timer.TimerServiceManagerAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component(SESSION_TIMEOUT_CALLBACK_JNDI)
public class SessionTimeoutHandler implements TimerListener {



  private static final Logger logger = LoggerFactory.getLogger(SessionTimeoutHandler.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private SessionManager sessionManager;

  @Autowired
  TR069EventNotificationService eventNotificationService;

  @Autowired
  DeviceOperationInterface deviceOperationInterface;

  @Autowired
  private TimerServiceManagerAPI timerServiceManagerAPI;

  @Autowired
  private DeviceRPCRequestRepositoryHelper deviceRPCRequestRepositoryHelper;

  @Autowired
  private TR069RequestProcessEngineUtility tr069ProcessEngineUtility;

  @Transactional(isolation = Isolation.DEFAULT, timeout = 300,
      propagation = Propagation.REQUIRES_NEW, rollbackFor = {Exception.class})
  @Override
  public void notifyTimeout(String timerId, Serializable data) {

    String timeoutType = (String) data;

    logger.info("Timeout notification received for the type: {}", timeoutType);
    if (WAITING_FOR_NBI_RESPONSE.equals(timeoutType)) {
      handleNBIResponseTimeout(timerId);
    } else if (WAITING_FOR_DEVICE_RESPONSE.equals(timeoutType)) {
      handleDeviceResponseTimeout(timerId);
    }
  }

  /**
   * Handling the NBI response timeout
   * 
   * @param timerId
   */
  private void handleNBIResponseTimeout(String timerId) {
    String[] splitTimerId = timerId.split(SEPERATOR);
    String deviceId = splitTimerId[0];
    String operationId = splitTimerId[1];
    try {
      MDC.put(CLIENT_STR, deviceId);
      // Only session must be moved to terminated state in such case
      logger.debug("The NBI operation request {} has timeout", operationId);

      TR069DeviceDetails deviceDetails = deviceOperationInterface.getDeviceDetails(deviceId);
      List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntityList =
          deviceRPCRequestRepositoryHelper.findByDeviceIdAndOperationId(deviceId,
              Long.valueOf(operationId));

      if (deviceRPCRequestEntityList != null) {
        logger.debug(
            "Device details and the NBI Operation request are successfully fetched for operationId: {}",
            operationId);
        DeviceRPCRequest nbiDeviceOperationrequest =
            TR069RequestProcessorUtility.convertToDTO(deviceRPCRequestEntityList);
        DeviceRPCResponse deviceRPCResponse = tr069ProcessEngineUtility
            .buildTimedOutOperationResult(deviceDetails, nbiDeviceOperationrequest);

        eventNotificationService.sendOperationResultToNBI(deviceRPCResponse);
        logger.debug("Successfully notified timed out operation result to NBI");

        // Marking the NBI Request as processed
        for (TR069DeviceRPCRequestEntity deviceRPCRequestEntity : deviceRPCRequestEntityList) {
          deviceRPCRequestEntity.setIsProcessed(Integer.valueOf(1));
        }
        deviceRPCRequestRepositoryHelper.saveAll(deviceRPCRequestEntityList);

        logger.debug("Stopping the connection request timer if any running");
        String connectionRequestTimerId = deviceId + SEPERATOR + CONNECTION_REQUEST;
        timerServiceManagerAPI.stopTimer(connectionRequestTimerId);
      }
    } catch (DeviceOperationException e) {
      logger.error(
          "Fetching device details failed for timed out operation processing for operation id: {} Reason: {}",
          operationId, e.getMessage());
    } catch (TimerException e) {
      logger.debug(
          "Failed stopping the timer job for connection request retry timer for operation id: {} Reason: {}",
          operationId, e.getMessage());
    } catch (Exception e) {
      logger.error(
          "An unknown exception occurred while timed out operation processing for operation id: {} Reason: {}",
          operationId, e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }

  /**
   * Handling the device inform response timeout
   * 
   * @param sessionId
   */
  private void handleDeviceResponseTimeout(String sessionId) {
    SessionDTO session = null;
    try {
      session = sessionManager.getSessionBySessionId(sessionId);
    } catch (SessionManagerException e) {
      if (ErrorCode.SESSION_EXPIRED.equals(e.getErrorCode())) {
        logger.info(
            "The session {} does not exist or already in TERMINATED state, hence ignoring the timeout notification",
            sessionId);
        return;
      }

      logger.debug("Session fetching failed, Reason: {} ", e.getMessage());
    }

    try {
      // Only session must be moved to terminated state in such case
      if (session != null) {
        MDC.put(CLIENT_STR, session.getDeviceId());
        boolean isSessionReset = false;

        String deviceId = session.getDeviceId();
        Long operationId = session.getCurrentOperationId();
        if (operationId != null && operationId != 0l
            && !SessionState.TERMINATED.equals(session.getSessionState())) {
          List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntityList =
              deviceRPCRequestRepositoryHelper.findByDeviceIdAndOperationId(deviceId, operationId);
          Integer isProcessed = deviceRPCRequestEntityList.get(0).getIsProcessed();
          if (isProcessed != null && isProcessed == 0) {
            logger.debug(
                "The Device RPC request is still in processing state, hence resetting the session timer");
            timerServiceManagerAPI.modifyTimer(sessionId, DEVICE_RESPONSE_TIMEOUT,
                WAITING_FOR_DEVICE_RESPONSE);
            logger.debug("Successfully restarted the session timer for session: {} ", sessionId);
            isSessionReset = true;
          }
        }

        if (!isSessionReset) {
          logger.debug("Updating the session state to terminated");
          session.setSessionState(SessionState.TERMINATED);
          sessionManager.updateSession(session);
          logger.debug("Successfully handled timeout notification of Device Response");
        }
      }
    } catch (Exception e) {
      logger.error(
          "An error occurred while checking the NBI state to reset the session timer, Reason: {}",
          e.getMessage());
      logger.debug("Updating the session to TERMINATED failed during timeout, Reason: {}",
          e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }
}
