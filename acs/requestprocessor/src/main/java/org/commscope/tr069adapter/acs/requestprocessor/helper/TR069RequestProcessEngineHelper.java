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

package org.commscope.tr069adapter.acs.requestprocessor.helper;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CR_REQ_Q;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.DEVICE_RESPONSE_TIMEOUT;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.DEVICE_RPC_EXECUTION_TIMEOUT_SECONDS;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SEPERATOR;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SESSION_TIMEOUT_CALLBACK_JNDI;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.WAITING_FOR_DEVICE_RESPONSE;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.WAITING_FOR_NBI_RESPONSE;

import java.util.Date;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.acs.common.exception.DeviceOperationException;
import org.commscope.tr069adapter.acs.common.exception.SessionConcurrentAccessException;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.requestprocessor.custom.CustomOperation;
import org.commscope.tr069adapter.acs.requestprocessor.dto.CustomOperationData;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionState;
import org.commscope.tr069adapter.acs.requestprocessor.dto.TR069RequestProcessorData;
import org.commscope.tr069adapter.acs.requestprocessor.impl.SessionManager;
import org.commscope.tr069adapter.acs.requestprocessor.util.TR069RequestProcessorCacheUtil;
import org.commscope.tr069adapter.common.timer.TimerException;
import org.commscope.tr069adapter.common.timer.TimerServiceManagerAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.jms.core.JmsTemplate;

public class TR069RequestProcessEngineHelper {

  private static final Logger logger =
      LoggerFactory.getLogger(TR069RequestProcessEngineHelper.class);

  @Autowired
  private ApplicationContext context;

  @Autowired
  private SessionManager sessionManager;

  @Autowired
  private TimerServiceManagerAPI timerServiceManagerAPI;

  @Autowired
  TR069RequestProcessorCacheUtil requestCacheUtil;

  @Autowired
  protected TR069RequestProcessEngineUtility tr069RequestProcessEngineUtility;

  @Autowired
  private JmsTemplate jmsTemplate;

  /**
   * @param deviceId
   * @return
   * @throws SessionManagerException
   */
  protected SessionDTO getSession(String deviceId) {
    logger.debug("Getting the existing Session Object for the device");
    return sessionManager.getSession(deviceId);
  }

  /**
   * @param sessionId
   * @return
   * @throws SessionManagerException
   */
  public SessionDTO getSessionBySessionId(String sessionId) throws SessionManagerException {
    return sessionManager.getSessionBySessionId(sessionId);
  }

  /**
   * @param deviceDetails
   * @param session
   */
  protected void initThreadLocalCache(DeviceDetails deviceDetails, SessionDTO session) {
    TR069RequestProcessorData tr069RequestProcessorData = new TR069RequestProcessorData();
    tr069RequestProcessorData.setTr069DeviceDetails((TR069DeviceDetails) deviceDetails);
    tr069RequestProcessorData.setSessionDTO(session);

    requestCacheUtil.put(tr069RequestProcessorData);
  }

  /**
   * @return
   */
  protected TR069RequestProcessorData getTR069RequestProcessorData() {
    return requestCacheUtil.get();
  }

  /**
   * @return
   */
  protected String getUniqueSessionIdentifier() {
    return sessionManager.generateUniqueSessionID();
  }

  /**
   * @param deviceId
   * @param notificationType
   * @return
   * @throws SessionConcurrentAccessException
   * @throws DeviceOperationException
   */
  protected SessionDTO acquireSessionLock(String deviceId, TR069InformType notificationType,
      boolean generateNewSessionId)
      throws SessionConcurrentAccessException, DeviceOperationException {
    logger.debug("Acquiring the session lock for the device");
    SessionDTO session = null;

    String sessionID = null;
    session = sessionManager.getLockedSession(deviceId);

    if (session == null && TR069InformType.BOOTSTRAP.equals(notificationType)) {
      logger.info(
          "Device is contacting ACS for the first time, creating the new session ID for the device");
      sessionID = getUniqueSessionIdentifier();
      session = new SessionDTO(deviceId, sessionID, null);
      session.setSessionStartTime(new Date());
      try {
        sessionManager.createSession(session);
        generateNewSessionId = false;
      } catch (Exception ex) {
        SessionConcurrentAccessException scae =
            new SessionConcurrentAccessException(ErrorCode.SESSION_ALREADY_LOCKED, ex.getMessage());
        logger.error(scae.getMessage());
        throw scae;
      }

      session = sessionManager.getLockedSession(deviceId);
    } else if (session == null) {
      DeviceOperationException doe = new DeviceOperationException(ErrorCode.DEVICE_NOT_ACTIVATED);
      logger.error(doe.getMessage());
      throw doe;
    }

    if (generateNewSessionId) {
      sessionID = getUniqueSessionIdentifier();
      session.setSessionId(sessionID);
    }

    return session;
  }

  /**
   * @param session
   * @throws SessionManagerException
   */
  protected void updateSession(SessionDTO session) {
    logger.debug("Updating the session");
    sessionManager.updateSession(session);
  }

  /**
   * @param deviceId
   * @throws SessionConcurrentAccessException
   */
  protected void deleteSession(String deviceId) {
    sessionManager.deleteSession(deviceId);
  }

  /**
   * Utility method to post the connection request initiation asynchronously on the device
   * 
   * @param tr069DeviceDetails
   */
  protected void requestForConnectionRequestInform(TR069DeviceDetails tr069DeviceDetails) {
    logger.info("Initiating Connection request on device: {}", tr069DeviceDetails.getDeviceId());
    try {
      jmsTemplate.convertAndSend(CR_REQ_Q, tr069DeviceDetails);
    } catch (Exception e) {
      logger.error("Connection request initiation failed, Reason: {}", e.getMessage());
    }
  }

  /**
   * @param tr069RequestProcessorData
   * @param sessionID
   */
  protected void updateSessionOnDeviceNotification(
      TR069RequestProcessorData tr069RequestProcessorData, String sessionID) {
    SessionDTO session = tr069RequestProcessorData.getSessionDTO();
    session.setSessionId(sessionID);
    session.setSessionStartTime(new Date());
    session.setCurrentOperationId(null);
    session.setSessionState(SessionState.PROCESSING);
  }

  /**
   * @param tr069RequestProcessorData
   * @param sessionState
   */
  protected void changeSessionState(TR069RequestProcessorData tr069RequestProcessorData,
      SessionState sessionState) {
    SessionDTO session = tr069RequestProcessorData.getSessionDTO();
    String oldSessionState =
        (session.getSessionState() != null) ? session.getSessionState().name() : null;
    String newSessionState = sessionState.name();
    logger.debug("Changing the session state from {} to {}", oldSessionState, newSessionState);
    session.setSessionState(sessionState);
  }

  /**
   * @param tr069RequestProcessorData
   * @param operationId
   */
  protected void updateSessionCurOpId(TR069RequestProcessorData tr069RequestProcessorData,
      Long operationId) {
    SessionDTO session = tr069RequestProcessorData.getSessionDTO();
    session.setCurrentOperationId(operationId);
  }

  /**
   * @param tr069RequestProcessorData
   * @param deviceNotification
   * @return
   */
  protected Boolean isDeviceUpdateExists(TR069RequestProcessorData tr069RequestProcessorData,
      DeviceInform deviceNotification) {
    Boolean isDeviceDataUpdated = false;
    TR069DeviceDetails persistedDeviceDetails = tr069RequestProcessorData.getTr069DeviceDetails();
    TR069DeviceDetails notificationDeviceDetails =
        (TR069DeviceDetails) deviceNotification.getDeviceDetails();

    if (notificationDeviceDetails.getConnectionRequestURL() == null
        || notificationDeviceDetails.getSoftwareVersion() == null
        || notificationDeviceDetails.getHardwareVersion() == null) {
      isDeviceDataUpdated = true;
      logger.warn(
          "Notification do not contains either connection request URL or swVer or hwVer of the device");
      return isDeviceDataUpdated;
    }

    if (!notificationDeviceDetails.getConnectionRequestURL()
        .equals(persistedDeviceDetails.getConnectionRequestURL())
        || !notificationDeviceDetails.getSoftwareVersion()
            .equals(persistedDeviceDetails.getSoftwareVersion())
        || !notificationDeviceDetails.getHardwareVersion()
            .equals(persistedDeviceDetails.getHardwareVersion())) {
      logger.debug("Device details are changed");
      isDeviceDataUpdated = true;
    }
    return isDeviceDataUpdated;
  }

  protected void updateDeviceDetails(TR069RequestProcessorData tr069RequestProcessorData,
      DeviceInform deviceNotification) {
    TR069DeviceDetails persistedDeviceDetails = tr069RequestProcessorData.getTr069DeviceDetails();
    TR069DeviceDetails notificationDeviceDetails =
        (TR069DeviceDetails) deviceNotification.getDeviceDetails();

    persistedDeviceDetails
        .setConnectionRequestURL(notificationDeviceDetails.getConnectionRequestURL());
    persistedDeviceDetails.setSoftwareVersion(notificationDeviceDetails.getSoftwareVersion());
    persistedDeviceDetails.setHardwareVersion(notificationDeviceDetails.getHardwareVersion());
  }

  /**
   * @param customClassJNDI
   * @param customOperationData
   * @return
   */
  public CustomOperationData executeCustomOperation(String customClassJNDI,
      CustomOperationData customOperationData) {
    try {
      logger.debug("Executing the custom logic implemented for JNDI: {}", customClassJNDI);
      CustomOperation customOperation = (CustomOperation) context.getBean(customClassJNDI);
      customOperationData =
          (CustomOperationData) customOperation.executeCustomLogic(customOperationData);
    } catch (Exception e) {
      logger.error("Custom operation execution failed, Reason: {}", e.getMessage());
    }
    return customOperationData;
  }

  /**
   * returns the operation code for Custom function execution
   * 
   * @param operationDetails
   * @return
   */
  protected TR069OperationCode getCustomOperationCode(TR069OperationDetails operationDetails) {
    List<ParameterDTO> deleteList = operationDetails.getDeleteParamList();
    List<ParameterDTO> setList = operationDetails.getSetParamList();
    List<ParameterDTO> modifyList = operationDetails.getModifyParamList();

    for (ParameterDTO param : deleteList) {
      if (!param.isProcessed())
        return TR069OperationCode.DELETE_OBJECT;
    }

    for (ParameterDTO param : setList) {
      if (!param.isProcessed())
        return TR069OperationCode.ADD_OBJECT;
    }

    for (ParameterDTO param : modifyList) {
      if (!param.isProcessed())
        return TR069OperationCode.SET_PARAMETER_VALUES;
    }

    return TR069OperationCode.SET_PARAMETER_VALUES;
  }

  /**
   * @param deviceId
   * @param operationId
   * @param timeout
   */
  protected void startDeviceRPCRequestTimer(String deviceId, Long operationId, Long timeout) {
    String timerId = deviceId + SEPERATOR + operationId;
    if (null == timeout || timeout == 0L) {
      timeout = DEVICE_RPC_EXECUTION_TIMEOUT_SECONDS;
    }

    try {
      timerServiceManagerAPI.startTimer(timerId, SESSION_TIMEOUT_CALLBACK_JNDI, (timeout * 1000),
          WAITING_FOR_NBI_RESPONSE);
      logger.debug(
          "Successfully started the timer task for Device RPC request with operation ID : {}",
          operationId);
    } catch (TimerException e) {
      logger.error("Couldn't start the timer task for Device RPC Request with operation ID : {}",
          operationId);
    }
  }

  /**
   * @param deviceId
   * @param operationId
   */
  public void stopDeviceRPCRequestTimer(String deviceId, Long operationId) {
    String timerId = deviceId + SEPERATOR + operationId;
    try {
      timerServiceManagerAPI.stopTimer(timerId);
      logger.debug(
          "Successfully stopped the timer task for Device RPC request with operation ID : {}",
          operationId);
    } catch (TimerException e) {
      logger.error("Couldn't stop the timer task for Device RPC Request with operation ID : {}",
          operationId);
    }
  }

  /**
   * @param deviceId
   * @param operationId
   * @param timeout
   */
  protected void startSessionTimer(String sessionId) {
    try {
      timerServiceManagerAPI.startTimer(sessionId, SESSION_TIMEOUT_CALLBACK_JNDI,
          DEVICE_RESPONSE_TIMEOUT, WAITING_FOR_DEVICE_RESPONSE);
      logger.debug("Successfully started the timer task for Device request with session ID : {}",
          sessionId);
    } catch (TimerException e) {
      logger.error("Couldn't start the timer task for Device Request with session ID : {}",
          sessionId);
    }
  }

  /**
   * @param deviceId
   * @param operationId
   */
  protected void stopSessionTimer(String sessionId) {
    try {
      timerServiceManagerAPI.stopTimer(sessionId);
      logger.debug("Successfully stopped the timer task for Device request with session ID : {}",
          sessionId);
    } catch (TimerException e) {
      logger.error("Couldn't stop the timer task for Device Request with session ID : {}",
          sessionId);
    }
  }
}
