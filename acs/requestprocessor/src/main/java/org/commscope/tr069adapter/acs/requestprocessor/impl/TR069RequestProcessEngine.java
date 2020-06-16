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

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SESSION_ID;

import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.dto.CustomOperationCode;
import org.commscope.tr069adapter.acs.common.dto.DeviceOperationRequestDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.acs.common.exception.DeviceOperationException;
import org.commscope.tr069adapter.acs.common.exception.SessionConcurrentAccessException;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.common.faults.AcsFaultCode;
import org.commscope.tr069adapter.acs.common.inform.BootInform;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.acs.common.response.DeviceInformResponse;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepositoryHelper;
import org.commscope.tr069adapter.acs.requestprocessor.dto.CustomOperationData;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionState;
import org.commscope.tr069adapter.acs.requestprocessor.dto.TR069RequestProcessorData;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceRPCRequestEntity;
import org.commscope.tr069adapter.acs.requestprocessor.helper.TR069RequestProcessEngineHelper;
import org.commscope.tr069adapter.acs.requestprocessor.util.TR069RequestProcessorUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TR069RequestProcessEngine extends TR069RequestProcessEngineHelper {

  private static final String PENDING_RPC_CHECK =
      "Checking if any pending Device RPC requests exists for the device";

  private static final Logger logger = LoggerFactory.getLogger(TR069RequestProcessEngine.class);

  @Autowired
  TR069EventNotificationService tr069EventNotificationService;

  @Autowired
  DeviceOperationInterface deviceOperationInterface;

  @Autowired
  protected DeviceRPCRequestRepositoryHelper deviceRPCRequestRepositoryHelper;

  /**
   * @param deviceRPCRequest
   * @throws TR069EventProcessingException
   * @throws SessionConcurrentAccessException
   */
  public void processDeviceRPCRequest(DeviceRPCRequest deviceRPCRequest)
      throws TR069EventProcessingException, SessionConcurrentAccessException {

    DeviceRPCResponse deviceRPCResponse = null;
    String deviceId = null;

    try {
      if (deviceRPCRequest == null) {
        TR069EventProcessingException ex =
            new TR069EventProcessingException(ErrorCode.INVALID_NBI_REQUEST);
        logger.error(ex.getMessage());
        throw ex;
      }

      Long operationId = deviceRPCRequest.getOperationId();
      logger.info("A Mapper request is received with operationID: {}", operationId);
      TR069DeviceDetails tr069DeviceDetails = null;
      deviceId = deviceRPCRequest.getDeviceDetails().getDeviceId();
      try {
        tr069DeviceDetails = deviceOperationInterface.getDeviceDetails(deviceId);
      } catch (DeviceOperationException deo) {
        logger.error(deo.getMessage());
        deviceRPCResponse = tr069RequestProcessEngineUtility.buildAbortedOperationresult(
            tr069DeviceDetails, deviceRPCRequest, AcsFaultCode.FAULT_CODE_8000);
        return;
      }

      try {
        SessionDTO sessionDTO = acquireSessionLockWithRetryOnFailure(deviceId, operationId);

        logger.debug("Persisting the Device RPC request, with operation ID: {}", operationId);
        List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntities =
            TR069RequestProcessorUtility.convertToEntity(deviceRPCRequest);
        deviceRPCRequestRepositoryHelper.saveAll(tr069DeviceRPCRequestEntities);
        logger.info("Successfully persisted the Device RPC request");

        if (sessionDTO != null) {
          if (SessionState.TERMINATED.equals(sessionDTO.getSessionState())) {
            logger.debug("No active session exists, hence requesting for Connection request");
            requestForConnectionRequestInform(tr069DeviceDetails);

            // Start Request Timer
            startDeviceRPCRequestTimer(deviceId, deviceRPCRequest.getOperationId(),
                deviceRPCRequest.getOptions().getExecutionTimeout());
          } else {
            logger.debug(
                "Session is in processing state, Will be notified to session manager to pick the request on session availability");
          }
        } else {
          logger.warn(
              "The device is not activated yet, hence the NBI Operation result cannot be processed!");
          deviceRPCResponse = tr069RequestProcessEngineUtility.buildAbortedOperationresult(
              tr069DeviceDetails, deviceRPCRequest, AcsFaultCode.FAULT_CODE_8001);
        }
      } catch (SessionConcurrentAccessException scae) {
        throw scae;
      } catch (Exception e) {
        logger.error("An unknown exception occurred while processing the NBI request, Reason: {}",
            e.getMessage());
        deviceRPCResponse = tr069RequestProcessEngineUtility.buildAbortedOperationresult(
            tr069DeviceDetails, deviceRPCRequest, AcsFaultCode.FAULT_CODE_8004);
      }
    } finally {
      if (deviceRPCResponse != null) {
        logger.debug("Sending failed operation result for this NBI request");
        tr069EventNotificationService.sendOperationResultToNBI(deviceRPCResponse);
        // Marking the NBI Request as processed
        deviceRPCRequestRepositoryHelper.markDeviceRPCRequestAsProcessed(deviceId,
            deviceRPCRequest.getOperationId());
      }
    }
  }

  /**
   * Common Step 1. Since there can exist only one Inform from any device, which will be the
   * initiator of the session, following steps to be followed a. Stop the session timer for this
   * device if any running. b. Get the lock by reading the session record for this device from DB
   * and load the session object in Thread Local Cache c. Create a new session id from
   * SessionManager, and update the session object with new Session id and change the state to
   * PROCESSING. 2. Read the Device record from the DB, and load the device DTO also in the
   * ThreadLocalCache
   * 
   * Common Notification Specific Step 1. Take the connection request URL from the
   * deviceNotification object 2. Update the Device DTO object with connection request URL,
   * swVersion, hwVersion if there is a difference. Update the last updated time if updated 3. Send
   * this Inform to the NBI, by calling the ProcessDeviceInform method on TR069NBIService module. 4.
   * Create the Inform response Object and update the sessionID used for this transaction in the
   * response which will be used in the cookie of the HTTP response 5. Post the message into
   * Response Queue 6. Change the session state to LOCKED 7. Save the Session and device records
   * with the values in ThreadLocalCache 8. Start the session timer and default request timer (As
   * configured for all the requests from device) 8. All the above steps to be executed in a single
   * transaction.
   * 
   * @param deviceNotification
   * @return
   * @throws SessionConcurrentAccessException
   */
  public DeviceInformResponse processDeviceInform(DeviceInform deviceNotification)
      throws SessionConcurrentAccessException {

    String deviceId = deviceNotification.getDeviceDetails().getDeviceId();
    TR069InformType notificationType = (TR069InformType) deviceNotification.getInformType();
    logger.info("Processing the Device Inform Event: '{}'", notificationType.getNotificationCode());
    String newSessionId = null;
    SessionDTO session = null;
    TR069DeviceDetails deviceDetails = null;
    DeviceInformResponse informResponse = null;

    try {

      SessionDTO sessionDTO = getSession(deviceId);
      if (sessionDTO != null && !SessionState.TERMINATED.equals(sessionDTO.getSessionState())) {
        String sessionId = sessionDTO.getSessionId();
        logger.debug(
            "The session with session id {} is not terminated, hence stopping the associated timer",
            sessionId);
        stopSessionTimer(sessionDTO.getSessionId());
      }

      // To stop the request timer if any running for this device, and send failed operation
      // result for any such pending cases. Requests pending in DB should not be cleared

      /*
       * Read any pending records in TR069_NBI_REQUEST table for this device. Send abort operation
       * result for all the pending requests Delete the records from the TR069_NBI_Request table for
       * this device.
       */
      if (deviceNotification instanceof BootstrapInform
          || deviceNotification instanceof BootInform) {
        sendAbortedOperationResultForPendingRPCRequests(deviceNotification.getDeviceDetails(),
            notificationType);
      }

      session = acquireSessionLock(deviceId, notificationType, true);
      deviceDetails = getPersistedDeviceDetails(deviceId, deviceNotification);

      newSessionId = session.getSessionId();
      logger.debug("The session id generated to process the device notification request is: {} ",
          newSessionId);

      initThreadLocalCache(deviceDetails, session);
      TR069RequestProcessorData tr069RequestProcessorData = getTR069RequestProcessorData();
      updateSessionOnDeviceNotification(tr069RequestProcessorData, newSessionId);

      logger.debug("Sending the Device Inform Event to the Mapper");
      tr069EventNotificationService.sendDeviceInformToNBI(deviceNotification);

      updateDeviceDetailsFromInform(tr069RequestProcessorData, deviceNotification);

      logger.debug("Updating the session for the device with newly generated session id");
      changeSessionState(tr069RequestProcessorData, SessionState.LOCKED);
      updateSession(session);

      // Start session timer - Get a default timeout for session.
      startSessionTimer(session.getSessionId());

      informResponse =
          new DeviceInformResponse(newSessionId, deviceNotification.getDeviceDetails());
    } catch (

    DeviceOperationException doe) {
      logger.error(doe.getMessage());
    } catch (SessionConcurrentAccessException scae) {
      throw scae;
    } catch (Exception e) {
      throw new SessionConcurrentAccessException(ErrorCode.UNKNOWN_ERROR, e.getMessage());
    }
    return informResponse;
  }

  /**
   * 
   * 1. Stop the request timer for this device using the session ID received in the cookie of the
   * request 2. Read the session record from the session table into ThreadLocalCache 3. Move the
   * session state to PROCESSING and update the OPERATION_ID as null. 4a. Session Manager to check
   * if any pending request being notified for the device using in memory cache - Not planned for
   * this Drop 4. As an interim solution for drop1, Check the TR069_NBI_REQUEST table if any request
   * is pending.
   * 
   * if (anyPendingRequestExists) { 1. Pick the request with the least created_time 2. Create the
   * response object and update the sessionID used for this transaction in the response which will
   * be used in the cookie of the HTTP response 3. Post the response object into Response Queue 4.
   * Change the session state to LOCKED and update the OPERATION_ID in the session table with the
   * NBI requests OPERATION_ID. 5. Save the Session record with the values in ThreadLocalCache 6.
   * Start the session timer and request timer (available in the request) } else { 1. Move the
   * session state to PROCESSING and update the OPERATION_ID as null. 2. Save the Session record
   * with the values in ThreadLocalCache. 3. Start the session timer. }
   * 
   * 6. All the above steps to be executed in a single transaction
   * 
   * @param deviceRPCResponse
   * @return
   * @throws SessionConcurrentAccessException
   */
  public DeviceRPCRequest processDeviceRPCResponse(DeviceRPCResponse deviceRPCResponse)
      throws SessionConcurrentAccessException {
    TR069DeviceDetails deviceDetails = (TR069DeviceDetails) deviceRPCResponse.getDeviceDetails();
    String deviceId = deviceDetails.getDeviceId();
    logger.info("Processing the operation response from device");

    SessionDTO session;
    DeviceRPCRequest deviceRPCRequest = null;
    try {
      session = acquireSessionLock(deviceId, null, false);
      String newSessionId = session.getSessionId();
      logger.debug("The session id used to process the device RPC response is: {}", newSessionId);

      initThreadLocalCache(deviceDetails, session);
      TR069RequestProcessorData tr069RequestProcessorData = getTR069RequestProcessorData();
      updateSessionOnDeviceNotification(tr069RequestProcessorData, newSessionId);

      Long operationId = deviceRPCResponse.getOperationId();

      deviceRPCRequest = getNextRPCRequest(deviceId, operationId);

      if (deviceRPCRequest != null) {
        OperationDetails opDetails = deviceRPCRequest.getOpDetails();
        if (opDetails != null) {
          OperationCode opCode = opDetails.getOpCode();
          if (opCode instanceof CustomOperationCode) {
            CustomOperationCode customOperationCode = (CustomOperationCode) opCode;
            String customOperationCodeName = customOperationCode.name();
            logger.info(
                "The Device RPC request is of custom type, the custom operation to be performed is: {}",
                customOperationCodeName);
            String jndiName = customOperationCode.getJndiName();
            CustomOperationData customOperationData =
                new CustomOperationData(deviceDetails, deviceRPCResponse, deviceRPCRequest);
            customOperationData = executeCustomOperation(jndiName, customOperationData);

            DeviceRPCRequest operationRequest = customOperationData.getDeviceRPCRequest();
            deviceRPCResponse = customOperationData.getDeviceRPCResponse();
            if (operationRequest != null) {
              operationRequest.addContextParam(SESSION_ID, newSessionId);
              updateSessionCurOpId(tr069RequestProcessorData, deviceRPCRequest.getOperationId());
              changeSessionState(tr069RequestProcessorData, SessionState.LOCKED);
              updateSession(session);
              if (deviceRPCResponse != null && operationRequest.getOperationId() != null
                  && !operationRequest.getOperationId()
                      .equals(deviceRPCResponse.getOperationId())) {
                logger.debug(
                    "Sending the Device RPC response for a configure Multiple object prior operation");
                // Sending the operation response to NBI
                tr069EventNotificationService.sendOperationResultToNBI(deviceRPCResponse);
              }
              return operationRequest;
            } else {
              logger.debug(PENDING_RPC_CHECK);
              deviceRPCRequest =
                  deviceRPCRequestRepositoryHelper.findOldestDeviceRPCRequest(deviceId);
            }
          }
        }
      }

      if (deviceRPCRequest != null) {
        logger.info("A pending Device RPC request exists for the device with operation ID: {}",
            deviceRPCRequest.getOperationId());
        updateSessionCurOpId(tr069RequestProcessorData, deviceRPCRequest.getOperationId());
        changeSessionState(tr069RequestProcessorData, SessionState.LOCKED);

        deviceRPCRequest.addContextParam(SESSION_ID, newSessionId);

      } else {
        logger.info(
            "No pending Device RPC request exists for the device, hence empty response will be sent to the device");
        logger.debug("Updating the session to terminated state");
        // To stop the session timer if any running for this device.
        stopSessionTimer(newSessionId);
        changeSessionState(tr069RequestProcessorData, SessionState.TERMINATED);
      }

      // To stop the request timer if any running for this operation.
      stopDeviceRPCRequestTimer(deviceId, deviceRPCResponse.getOperationId());

      // Sending the operation response to NBI
      logger.debug("Sending the Device RPC Response to the Mapper");
      tr069EventNotificationService.sendOperationResultToNBI(deviceRPCResponse);

      updateSession(session);
    } catch (DeviceOperationException doe) {
      logger.error(doe.getMessage());
    } catch (SessionConcurrentAccessException scae) {
      throw scae;
    }

    return deviceRPCRequest;
  }

  /**
   * 
   * 1. Stop the request timer for this device using the session ID received in the cookie of the
   * request 2. Read the session record from the session table into ThreadLocalCache 3. Move the
   * session state to PROCESSING and update the OPERATION_ID as null. 4a. Session Manager to check
   * if any pending request being notified for the device using in memory cache - Not planned for
   * this Drop 4. As an interim solution for drop1, Check the TR069_NBI_REQUEST table if any request
   * is pending.
   * 
   * if (anyPendingRequestExists) { 1. Pick the request with the least created_time 2. Create the
   * response object and update the sessionID used for this transaction in the response which will
   * be used in the cookie of the HTTP response 3. Post the response object into Response Queue 4.
   * Change the session state to LOCKED and update the OPERATION_ID in the session table with the
   * NBI requests OPERATION_ID. 5. Save the Session record with the values in ThreadLocalCache 6.
   * Start the session timer and request timer (available in the request) } else { 1. Move the
   * session state to PROCESSING and update the OPERATION_ID as null. 2. Save the Session record
   * with the values in ThreadLocalCache. 3. Start the session timer. }
   * 
   * 6. All the above steps to be executed in a single transaction
   * 
   * @param deviceDetails
   * @return
   * @throws SessionConcurrentAccessException
   */
  public DeviceRPCRequest processEmptyDeviceRequest(TR069DeviceDetails deviceDetails)
      throws SessionConcurrentAccessException {
    String deviceId = deviceDetails.getDeviceId();
    logger.info("Processing the empty request from device");

    SessionDTO session;
    DeviceRPCRequest nbiDeviceOperationRequest = null;
    try {
      session = acquireSessionLock(deviceId, null, false);
      String newSessionId = session.getSessionId();
      logger.debug("The session id used to process the empty device request is: {}", newSessionId);

      initThreadLocalCache(deviceDetails, session);
      TR069RequestProcessorData tr069RequestProcessorData = getTR069RequestProcessorData();
      updateSessionOnDeviceNotification(tr069RequestProcessorData, newSessionId);

      logger.debug(PENDING_RPC_CHECK);
      nbiDeviceOperationRequest =
          deviceRPCRequestRepositoryHelper.findOldestDeviceRPCRequest(deviceId);
      if (nbiDeviceOperationRequest != null) {
        Long operationId = nbiDeviceOperationRequest.getOperationId();
        OperationDetails opDetails = nbiDeviceOperationRequest.getOpDetails();
        if (opDetails != null) {
          OperationCode opCode = opDetails.getOpCode();
          if (opCode instanceof CustomOperationCode) {
            CustomOperationCode customOperationCode = (CustomOperationCode) opCode;
            String customOperationCodeName = customOperationCode.name();
            logger.info(
                "The Device RPC operation request is of custom type, the custom operation to be performed is: {}",
                customOperationCodeName);
            String jndiName = customOperationCode.getJndiName();
            CustomOperationData customOperationData =
                new CustomOperationData(deviceDetails, null, nbiDeviceOperationRequest);
            customOperationData = executeCustomOperation(jndiName, customOperationData);

            DeviceRPCRequest operationRequest = customOperationData.getDeviceRPCRequest();
            if (operationRequest != null) {
              operationRequest.addContextParam(SESSION_ID, newSessionId);
              updateSessionCurOpId(tr069RequestProcessorData, operationId);
              changeSessionState(tr069RequestProcessorData, SessionState.LOCKED);
              updateSession(session);
              return operationRequest;
            } else {
              logger.debug(PENDING_RPC_CHECK);
              nbiDeviceOperationRequest =
                  deviceRPCRequestRepositoryHelper.findOldestDeviceRPCRequest(deviceId);
            }
          }
        }
      }

      if (nbiDeviceOperationRequest != null) {
        Long operationId = nbiDeviceOperationRequest.getOperationId();
        logger.info("A pending Device RPC request exists for the device with operation ID: {}",
            operationId);
        updateSessionCurOpId(tr069RequestProcessorData, operationId);
        changeSessionState(tr069RequestProcessorData, SessionState.LOCKED);

        nbiDeviceOperationRequest.addContextParam(SESSION_ID, newSessionId);

      } else {
        logger.info(
            "No pending Device RPC request exists for the device, hence empty device response will be sent to the device");
        logger.debug("Updating the session to terminated state");
        // To stop the session timer if any running for this device.
        stopSessionTimer(newSessionId);
        changeSessionState(tr069RequestProcessorData, SessionState.TERMINATED);
      }

      updateSession(session);
    } catch (DeviceOperationException doe) {
      logger.error(doe.getMessage());
    } catch (SessionConcurrentAccessException scae) {
      throw scae;
    }

    return nbiDeviceOperationRequest;
  }

  /**
   * @param sessionId
   * @return
   * @throws SessionManagerException
   */
  public DeviceOperationRequestDetails getOpRequestDetailsBySessionId(String sessionId)
      throws SessionManagerException {
    DeviceOperationRequestDetails deviceOperationRequestDetails =
        new DeviceOperationRequestDetails();

    logger.debug("Fetching Operation request details for session: {}", sessionId);
    SessionDTO session = getSessionBySessionId(sessionId);
    String deviceId = session.getDeviceId();
    TR069DeviceDetails deviceDetails = null;
    try {
      deviceDetails = deviceOperationInterface.getDeviceDetails(deviceId);
      if (session.getCurrentOperationId() == null) {
        logger.debug("There exists no pending operation request for the session: {}", sessionId);
      } else {
        logger.debug("There exists pending operation request for the session: {}", sessionId);
        List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntityList =
            deviceRPCRequestRepositoryHelper.findByDeviceIdAndOperationId(deviceId,
                session.getCurrentOperationId());
        if (tr069DeviceRPCRequestEntityList == null) {
          SessionManagerException ex =
              new SessionManagerException(ErrorCode.SESSION_EXPIRED, sessionId);
          logger.error(ex.getMessage());
          throw ex;
        }
        DeviceRPCRequest deviceRPCRequest =
            TR069RequestProcessorUtility.convertToDTO(tr069DeviceRPCRequestEntityList);
        OperationCode opCode = deviceRPCRequest.getOpDetails().getOpCode();

        String operationName = null;

        if (opCode instanceof TR069OperationCode) {
          operationName = ((TR069OperationCode) opCode).name();
        } else {
          operationName = ((CustomOperationCode) opCode).name();
          TR069OperationDetails tr069OperationDetails =
              (TR069OperationDetails) deviceRPCRequest.getOpDetails();
          opCode = getCustomOperationCode(tr069OperationDetails);
        }
        logger.info("The pending operation request for the session is of operation {}",
            operationName);
        deviceOperationRequestDetails.setOpCode(opCode);
        deviceOperationRequestDetails.setOperationId(deviceRPCRequest.getOperationId());
      }
      deviceOperationRequestDetails.setDeviceDetails(deviceDetails);
    } catch (DeviceOperationException e) {
      SessionManagerException ex =
          new SessionManagerException(ErrorCode.DEVICE_NOT_EXISTS, deviceId);
      logger.error(ex.getMessage());
      throw ex;
    } catch (Exception e) {
      logger.error(e.getMessage());
      SessionManagerException ex =
          new SessionManagerException(ErrorCode.SESSION_EXPIRED, sessionId);
      logger.error(ex.getMessage());
      throw ex;
    }
    return deviceOperationRequestDetails;
  }

  /**
   * @param deviceId
   * @return
   * @throws DeviceOperationException
   */
  public TR069DeviceDetails getDeviceDetails(String deviceId) throws DeviceOperationException {
    return deviceOperationInterface.getDeviceDetails(deviceId);
  }

  /**
   * @param deviceId
   * @param operationId
   * @return
   * @throws DeviceOperationException
   * @throws SessionConcurrentAccessException
   * @throws InterruptedException
   */
  private SessionDTO acquireSessionLockWithRetryOnFailure(String deviceId, Long operationId)
      throws DeviceOperationException, SessionConcurrentAccessException, InterruptedException {
    int sessionLockAcquireRetryCount = 0;
    SessionDTO sessionDTO = null;
    do {
      try {
        sessionDTO = acquireSessionLock(deviceId, null, false);
        logger.info(
            "Successfully acquired the session lock for processing NBI request with operation ID: {}",
            operationId);
        break;
      } catch (SessionConcurrentAccessException ex) {
        sessionLockAcquireRetryCount++;
        if (sessionLockAcquireRetryCount == 3) {
          logger.error("Failed acquiring the lock after retry, rolling back the transaction");
          throw ex;
        }
        logger.warn(
            "Session lock acquiring failed with SessionConcurrentAccessException, hence retrying");
        Thread.sleep(1000L);
      }
    } while (sessionLockAcquireRetryCount < 3);

    return sessionDTO;
  }

  /**
   * @param deviceId
   * @param deviceNotification
   * @return
   * @throws DeviceOperationException
   */
  private TR069DeviceDetails getPersistedDeviceDetails(String deviceId,
      DeviceInform deviceNotification) throws DeviceOperationException {
    TR069DeviceDetails deviceDetails = null;
    try {
      deviceDetails = deviceOperationInterface.getDeviceDetails(deviceId);
    } catch (DeviceOperationException doe) {
      if (ErrorCode.DEVICE_NOT_EXISTS.equals(doe.getErrorCode())) {
        logger.info(
            "Creating the device record in TR069_DEVICE_ table, as the device is authenticated successfully.");
        createDevice(deviceNotification.getDeviceDetails());
        deviceDetails = deviceOperationInterface.getDeviceDetails(deviceId);
      }
    }

    return deviceDetails;
  }

  /**
   * @param deviceDetails
   * @param notificationType
   * @throws TR069EventProcessingException
   */
  private void sendAbortedOperationResultForPendingRPCRequests(DeviceDetails deviceDetails,
      TR069InformType notificationType) throws TR069EventProcessingException {
    String deviceId = deviceDetails.getDeviceId();
    String notificationName = notificationType.name();
    logger.debug(
        "Device Inform event received is {}, hence aborting all the pending operations if any exists",
        notificationName);

    List<DeviceRPCRequest> deviceRPCRequestList =
        deviceRPCRequestRepositoryHelper.findAllDeviceRPCRequests(deviceId);

    for (DeviceRPCRequest pendingDeviceRPCRequest : deviceRPCRequestList) {
      DeviceRPCResponse deviceOpResult =
          tr069RequestProcessEngineUtility.buildAbortedOperationresult(deviceDetails,
              pendingDeviceRPCRequest, AcsFaultCode.FAULT_CODE_8002);
      String operationName = null;
      if (pendingDeviceRPCRequest.getOpDetails().getOpCode() instanceof CustomOperationCode) {
        CustomOperationCode operationCode =
            (CustomOperationCode) pendingDeviceRPCRequest.getOpDetails().getOpCode();
        operationName = operationCode.name();
      } else {
        TR069OperationCode operationCode =
            (TR069OperationCode) pendingDeviceRPCRequest.getOpDetails().getOpCode();
        operationName = operationCode.name();
      }
      Long operationId = pendingDeviceRPCRequest.getOperationId();
      logger.debug("Aborting the NBI Operation request with operation Id : {} for operation: {}",
          operationId, operationName);
      tr069EventNotificationService.sendOperationResultToNBI(deviceOpResult);
      // Marking the NBI Request as processed
      deviceRPCRequestRepositoryHelper.markDeviceRPCRequestAsProcessed(deviceId, operationId);
      stopDeviceRPCRequestTimer(deviceId, operationId);
    }
  }

  /**
   * @param tr069RequestProcessorData
   * @param deviceNotification
   */
  private void updateDeviceDetailsFromInform(TR069RequestProcessorData tr069RequestProcessorData,
      DeviceInform deviceNotification) {
    Boolean isDeviceDataChanged =
        isDeviceUpdateExists(tr069RequestProcessorData, deviceNotification);
    if (isDeviceDataChanged.booleanValue()) {
      updateDeviceDetails(tr069RequestProcessorData, deviceNotification);
      try {
        logger.info(
            "The device data like connection request URL/ Device SW/HW version has changed, hence updating the device details");
        deviceOperationInterface
            .updateDeviceDetails(tr069RequestProcessorData.getTr069DeviceDetails());
      } catch (DeviceOperationException e) {
        logger.error("Updating the device details with the notification details failed, Reason: {}",
            e.getMessage());
        logger.error(e.getMessage());
      }
    }
  }

  /**
   * @param deviceId
   * @param operationId
   * @return
   */
  private DeviceRPCRequest getNextRPCRequest(String deviceId, Long operationId) {
    DeviceRPCRequest deviceRPCRequest = null;
    try {
      List<TR069DeviceRPCRequestEntity> entityList =
          deviceRPCRequestRepositoryHelper.findByDeviceIdAndOperationId(deviceId, operationId);
      Integer operationCode = entityList.get(0).getOpCode();
      if (CustomOperationCode.getByOperationCode(operationCode) == null) {
        logger.info("Marking the Device RPC request with operation id - {} as processed.",
            operationId);
        deviceRPCRequestRepositoryHelper.markDeviceRPCRequestAsProcessed(deviceId, operationId);
      }
      logger.debug(PENDING_RPC_CHECK);
      deviceRPCRequest = deviceRPCRequestRepositoryHelper.findOldestDeviceRPCRequest(deviceId);
    } catch (TR069EventProcessingException e) {
      logger.error("An unknown exception occurred while fetching the NBI request, Reason: {}",
          e.getMessage());
    }

    return deviceRPCRequest;
  }

  /**
   * Creates the device in the DM module if factory imported already
   * 
   * @param deviceDetails
   * @throws DeviceOperationException
   */
  private void createDevice(DeviceDetails deviceDetails) throws DeviceOperationException {
    deviceOperationInterface.updateDeviceDetails(deviceDetails);
  }
}
