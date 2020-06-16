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

package org.commscope.tr069adapter.acs.cpe.handler;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CONNECTION_REQUEST;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SEPERATOR;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.DeviceOperationRequestDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.common.inform.AbstractDeviceInform;
import org.commscope.tr069adapter.acs.common.inform.TransferCompleteInform;
import org.commscope.tr069adapter.acs.common.requestprocessor.service.TR069DeviceEventHandler;
import org.commscope.tr069adapter.acs.common.response.DeviceInformResponse;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.cpe.TR069RPC;
import org.commscope.tr069adapter.acs.cpe.builder.DeviceInformBuilder;
import org.commscope.tr069adapter.acs.cpe.builder.DeviceRPCBuilder;
import org.commscope.tr069adapter.acs.cpe.builder.DeviceRPCResponseBuilder;
import org.commscope.tr069adapter.acs.cpe.rpc.Fault;
import org.commscope.tr069adapter.acs.cpe.rpc.Inform;
import org.commscope.tr069adapter.acs.cpe.rpc.TransferComplete;
import org.commscope.tr069adapter.common.timer.TimerException;
import org.commscope.tr069adapter.common.timer.TimerServiceManagerAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DeviceEventHandler {

  private static final Logger logger = LoggerFactory.getLogger(DeviceEventHandler.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private DeviceInformBuilder deviceInformBuilder;

  @Autowired
  private DeviceRPCBuilder deviceRPCBuilder;

  @Autowired
  private DeviceRPCResponseBuilder deviceRPCResponseBuilder;

  @Autowired
  private TR069DeviceEventHandler tr069DeviceEventHandler;

  @Autowired
  private TimerServiceManagerAPI timerServiceManagerAPI;

  @Autowired
  private DeviceValidator deviceValidtor;

  private static Map<String, List<String>> informParameter = null;

  static {
    informParameter = new HashMap<>();
    List<String> parameters = new ArrayList<>();
    parameters.add("InternetGatewayDevice.DeviceInfo.HardwareVersion");
    parameters.add("InternetGatewayDevice.DeviceInfo.SoftwareVersion");
    parameters.add("InternetGatewayDevice.DeviceInfo.ProvisioningCode");
    parameters.add("InternetGatewayDevice.ManagementServer.ConnectionRequestURL");
    parameters.add("InternetGatewayDevice.ManagementServer.ParameterKey");
    informParameter.put("InternetGatewayDevice", parameters);

    List<String> deviceParameters = new ArrayList<>();
    deviceParameters.add("Device.DeviceInfo.HardwareVersion");
    deviceParameters.add("Device.DeviceInfo.SoftwareVersion");
    deviceParameters.add("Device.DeviceInfo.ProvisioningCode");
    deviceParameters.add("Device.ManagementServer.ConnectionRequestURL");
    deviceParameters.add("Device.ManagementServer.ParameterKey");
    informParameter.put("Device", deviceParameters);
  }

  /**
   * @param inform
   * @param authorizationHeader
   * @return
   * @throws TR069EventProcessingException
   */
  public DeviceInformResponse processDeviceInform(Inform inform, String authorizationHeader)
      throws TR069EventProcessingException {

    DeviceInformResponse deviceInformResponse = null;
    try {
      String deviceId = inform.getSn();
      MDC.put(CLIENT_STR, deviceId);

      logger.info("Processing the device Inform event");

      logger.debug("Authorization header received in the request -> {}", authorizationHeader);
      Boolean isAuthorized = deviceValidtor.isDeviceAuthorized(inform, authorizationHeader);
      logger.info("Is device authentication successful: {}", isAuthorized);
      if (!isAuthorized.booleanValue()) {
        TR069EventProcessingException ex =
            new TR069EventProcessingException(ErrorCode.UNAUTHORIZED_EVENT, "Authorization failed");
        logger.error(ex.getMessage());
        throw ex;
      }

      logger.debug("The root element is: {}", inform.getRoot());
      // Canceling any connection initiator timer running, due to inform event
      stopConnectionInitiatorTimer(inform.getSn());

      if (!deviceValidtor.validateDevice(inform.getSn(), inform.getOui(), inform.getProductClass())
          .booleanValue()) {
        TR069EventProcessingException ex =
            new TR069EventProcessingException(ErrorCode.OUI_OR_PC_MISMATCH);
        logger.error(ex.getMessage());
        throw ex;
      }

      if (!validateInformParameters(inform)) {
        TR069EventProcessingException ex =
            new TR069EventProcessingException(ErrorCode.INVALID_PARAMS_IN_INFORM);
        logger.error(ex.getMessage());
        throw ex;
      }

      AbstractDeviceInform deviceInform = deviceInformBuilder.constructDeviceInform(inform);
      if (deviceInform == null) {
        TR069EventProcessingException ex =
            new TR069EventProcessingException(ErrorCode.INVALID_PARAMS_IN_INFORM);
        logger.error(ex.getMessage());
        throw ex;
      }

      logger.debug("Sending the device inform to TR069 Request Processor to process");
      deviceInformResponse = tr069DeviceEventHandler.processDeviceInform(deviceInform);

    } catch (TR069EventProcessingException tr069Ex) {
      throw tr069Ex;
    } catch (Exception e) {
      TR069EventProcessingException ex =
          new TR069EventProcessingException(ErrorCode.FAILED_PROCESSING_INFORM, e.getMessage());
      logger.error(ex.getMessage());
      throw ex;
    } finally {
      MDC.remove(CLIENT_STR);
    }

    return deviceInformResponse;

  }

  /**
   * @param tc
   * @return
   * @throws TR069EventProcessingException
   */
  public DeviceInformResponse processTransferComplete(TransferComplete tc)
      throws TR069EventProcessingException {

    logger.debug("Processing Transfer Complete");

    String startTime = tc.getStartTime();
    String completeTime = tc.getCompleteTime();
    int faultCode = tc.getFaultCode();
    String faultString = tc.getFaultString();
    String commandKey = tc.getCommandKey();

    DeviceInformResponse deviceInformResponse = null;

    try {
      MDC.put(CLIENT_STR, commandKey);
      TransferCompleteInform transferCompleteInform = new TransferCompleteInform();
      transferCompleteInform.setCommandKey(tc.getCommandKey());
      transferCompleteInform.setCompleteTime(completeTime);
      transferCompleteInform.setFaultCode(faultCode);
      transferCompleteInform.setFaultString(faultString);
      transferCompleteInform.setStartTime(startTime);

      logger.debug("TransferComplete inform received with Start time");

      transferCompleteInform.setDeviceDetails(tr069DeviceEventHandler.getDeviceDetails(commandKey));
      deviceInformResponse = tr069DeviceEventHandler.processDeviceInform(transferCompleteInform);
      logger.debug("Successfully processed the TRANSFER COMPLETE Inform");

    } catch (Exception e) {
      throw new TR069EventProcessingException(ErrorCode.FAILED_PROCESSING_INFORM, e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }

    return deviceInformResponse;
  }

  /**
   * @param msg
   * @param sessionId
   * @return
   * @throws TR069EventProcessingException
   */
  public TR069RPC processRPCResponse(TR069RPC msg, String sessionId)
      throws TR069EventProcessingException {
    DeviceOperationRequestDetails deviceOperationRequestDetails = null;
    try {
      deviceOperationRequestDetails =
          tr069DeviceEventHandler.getOpRequestDetailsBySessionId(sessionId);
      if (null == deviceOperationRequestDetails
          || null == deviceOperationRequestDetails.getDeviceDetails()) {
        logger.error("Response with invalid session ID: {}", sessionId);
        return null;
      }

      String deviceId = deviceOperationRequestDetails.getDeviceDetails().getDeviceId();
      MDC.put(CLIENT_STR, deviceId);
      DeviceRPCResponse deviceRPCResponse = new DeviceRPCResponse();
      deviceRPCResponse.setDeviceDetails(deviceOperationRequestDetails.getDeviceDetails());
      deviceRPCResponse.setOperationId(deviceOperationRequestDetails.getOperationId());
      OperationResponse operationResponse = null;
      if (msg instanceof Fault) {
        Fault values = (Fault) msg;
        logger.info("{} ID->{} faultCode->{} faultString->{}", values.getName(), values.getId(),
            values.getCwmpFaultCode(), values.getFaultStringCwmp());
        deviceRPCResponse.setFaultKey(values.getCwmpFaultCode());
        deviceRPCResponse.setFaultString(
            values.getFaultStringCwmp() + ": Error code: " + values.getCwmpFaultCode());

        TR069OperationCode operationCode =
            (TR069OperationCode) deviceOperationRequestDetails.getOpCode();
        operationResponse = constructResponseForFault(operationCode);
        if (operationResponse != null) {
          operationResponse.setStatus(1);
          operationResponse.setParameterDTOs(new ArrayList<ParameterDTO>());
        }
      } else {
        operationResponse = deviceRPCResponseBuilder.constructDeviceRPCResponse(msg);
      }
      deviceRPCResponse.setOperationResponse(operationResponse);

      DeviceRPCRequest deviceRPCRequest =
          tr069DeviceEventHandler.processDeviceRPCResponse(deviceRPCResponse);
      if (null != deviceRPCRequest) {
        return deviceRPCBuilder.constructDeviceRPC(deviceRPCRequest);
      }
    } catch (SessionManagerException e) {
      logger.error("Error while getting device detail for the session id: {}", sessionId);
    } catch (Exception e) {
      throw new TR069EventProcessingException(ErrorCode.FAILED_PROCESSING_RPC_RESPONSE,
          msg.getName(), e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }

    return null;
  }

  /**
   * @param sessionId
   * @return
   * @throws TR069EventProcessingException
   */
  public TR069RPC processEmptyRequest(String sessionId) throws TR069EventProcessingException {
    try {
      DeviceOperationRequestDetails deviceOperationRequestDetails =
          tr069DeviceEventHandler.getOpRequestDetailsBySessionId(sessionId);
      DeviceRPCRequest deviceRPCRequest = null;
      String deviceId = deviceOperationRequestDetails.getDeviceDetails().getDeviceId();
      MDC.put(CLIENT_STR, deviceId);
      deviceRPCRequest = tr069DeviceEventHandler
          .processEmptyDeviceRequest(deviceOperationRequestDetails.getDeviceDetails());
      if (null == deviceRPCRequest) {
        return null;
      } else {
        logger.debug("There exists a NBI request to process.");
        return deviceRPCBuilder.constructDeviceRPC(deviceRPCRequest);
      }
    } catch (SessionManagerException e) {
      logger.error("Error while processing empty request, reason: {}", e.getMessage());
    } catch (Exception e) {
      throw new TR069EventProcessingException(ErrorCode.EMPTY_REQUEST_PROCESSING_ERROR,
          e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
    return null;
  }

  /**
   * @param operationCode
   * @return
   */
  private OperationResponse constructResponseForFault(TR069OperationCode operationCode) {
    OperationResponse operationResponse = null;
    if (operationCode.equals(TR069OperationCode.ADD_OBJECT)) {
      operationResponse = new org.commscope.tr069adapter.acs.common.response.AddObjectResponse();
    } else if (operationCode.equals(TR069OperationCode.DELETE_OBJECT)) {
      operationResponse = new org.commscope.tr069adapter.acs.common.response.DeleteObjectResponse();
    } else if (operationCode.equals(TR069OperationCode.SET_PARAMETER_VALUES)) {
      operationResponse =
          new org.commscope.tr069adapter.acs.common.response.SetParameterValueResponse();
    } else if (operationCode.equals(TR069OperationCode.GET_PARAMETER_VALUES)) {
      operationResponse =
          new org.commscope.tr069adapter.acs.common.response.GetParameterValueResponse();
    } else if (operationCode.equals(TR069OperationCode.GET_PARAMETER_ATTRIBUTES)) {
      operationResponse =
          new org.commscope.tr069adapter.acs.common.response.GetParameterAttributeResponse();
    }
    return operationResponse;
  }

  /**
   * @param serialNumber
   */
  public void stopConnectionInitiatorTimer(String serialNumber) {
    String timerId = serialNumber + SEPERATOR + CONNECTION_REQUEST;
    try {
      logger
          .debug("Canceling the Connection initiation timer, as Inform is been sent by the device");
      timerServiceManagerAPI.stopTimer(timerId);
    } catch (TimerException e) {
      logger.error(
          "An exception occurred while stopping the connection initiator session timer, Reason: {}",
          e.getMessage());
    }
  }

  /**
   * @param lastInform
   * @return
   */
  private boolean validateInformParameters(Inform lastInform) {
    boolean validate = false;
    String root = lastInform.getRoot();
    if (!informParameter.containsKey(root))
      return validate;
    List<String> params = informParameter.get(root);
    Set<String> keySet = lastInform.getParams().keySet();
    validate = true;
    for (String param : params) {
      if (!keySet.contains(param)) {
        logger.warn("This param Not found in the inform {}", param);
        validate = false;
        break;
      }
    }
    return validate;
  }

  /**
   * @param tr069ex
   * @return
   */
  public int handleException(TR069EventProcessingException tr069ex) {

    int errorresponseCode = 0;

    ErrorCode errorCode = tr069ex.getErrorCode();
    switch (errorCode) {
      case UNSUPPORTED_CHARACTER_ENCODING:
      case INVALID_PARAMS_IN_INFORM:
      case FAILED_PROCESSING_INFORM:
        errorresponseCode = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
        break;
      case UNAUTHORIZED_EVENT:
        errorresponseCode = HttpServletResponse.SC_UNAUTHORIZED;
        break;
      default:
        break;
    }

    return errorresponseCode;
  }

  /***************************************************************************************************************************/

  public void setDeviceInformBuilder(DeviceInformBuilder deviceInformBuilder) {
    this.deviceInformBuilder = deviceInformBuilder;
  }

  public void setDeviceRPCBuilder(DeviceRPCBuilder deviceRPCBuilder) {
    this.deviceRPCBuilder = deviceRPCBuilder;
  }

  public void setDeviceRPCResponseBuilder(DeviceRPCResponseBuilder deviceRPCResponseBuilder) {
    this.deviceRPCResponseBuilder = deviceRPCResponseBuilder;
  }

  public void setTr069DeviceEventHandler(TR069DeviceEventHandler tr069DeviceEventHandler) {
    this.tr069DeviceEventHandler = tr069DeviceEventHandler;
  }

  public void setTimerServiceManagerAPI(TimerServiceManagerAPI timerServiceManagerAPI) {
    this.timerServiceManagerAPI = timerServiceManagerAPI;
  }

  public void setDeviceAuthenticator(DeviceValidator deviceAuthenticator) {
    this.deviceValidtor = deviceAuthenticator;
  }

}
