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

package org.commscope.tr069adapter.vesagent.util;

import java.util.Calendar;
import java.util.List;
import java.util.Map;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.vesagent.exception.VesAgentException;


public class VesAgentUtils {
  private static final Log logger = LogFactory.getLog(VesAgentUtils.class);

  private static String errorMsg = null;

  public static boolean isNullOrEmpty(String object) {
    return (null == object || object.isEmpty());
  }

  public static Boolean isNullOrEmpty(List list) {
    return (null == list || list.isEmpty());
  }

  public static Boolean isNullOrEmpty(Map map) {
    return (null == map || map.isEmpty());
  }

  public static void validateDeviceId(String deviceId) throws VesAgentException {
    if (null == deviceId || deviceId.isEmpty()) {
      errorMsg = "Error: deviceId in request is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(errorMsg);
    }
  }

  public static void validateHeartBeatPeriod(Integer heartBeatPeriod) throws VesAgentException {
    if (null == heartBeatPeriod) {
      errorMsg = "Error: heartBeatPeriod in request is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(errorMsg);
    }
  }


  public static void validateCountDownTimer(Integer countDownTimer) throws VesAgentException {
    if (null == countDownTimer) {
      errorMsg = "Error: countDownTimer in request is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(errorMsg);
    }
  }

  public static void validateDeviceRPCRequest(DeviceRPCRequest deviceRPCRequest)
      throws VesAgentException {
    if (null == deviceRPCRequest || null == deviceRPCRequest.getOpDetails()
        || null == deviceRPCRequest.getOpDetails().getParmeters()
        || deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
      errorMsg = "Error: Input parameter list is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(VesAgentConstants.INVALID_ARGUMENTS, errorMsg);
    }

    if (null == deviceRPCRequest.getDeviceDetails()
        || null == deviceRPCRequest.getDeviceDetails().getDeviceId()
        || deviceRPCRequest.getDeviceDetails().getDeviceId().isEmpty()) {
      errorMsg = "Error: Input deviceId is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(VesAgentConstants.INVALID_ARGUMENTS, errorMsg);
    }
  }

  public static void validateVESNotification(VESNotification notification)
      throws VesAgentException {
    if (null == notification || null == notification.getDevnotification()
        || null == notification.getDevnotification().getDeviceDetails()
        || null == notification.getDevnotification().getDeviceDetails().getDeviceId()
        || notification.getDevnotification().getDeviceDetails().getDeviceId().isEmpty()) {

      errorMsg = "Error: Input device details is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(VesAgentConstants.INVALID_ARGUMENTS, errorMsg);
    }
  }

  public static void validateDelVESNotification(VESNotification notification)
      throws VesAgentException {
    if (null == notification || null == notification.getOperationDetails()
        || null == notification.getOperationDetails().getParmeters()
        || notification.getOperationDetails().getParmeters().isEmpty()) {
      errorMsg = "Error: Input parameter list is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(VesAgentConstants.INVALID_ARGUMENTS, errorMsg);
    }

    if (null == notification.geteNodeBName() || notification.geteNodeBName().isEmpty()) {
      errorMsg = "Error: Input deviceId/enodeBName is null or empty";
      logger.error(errorMsg);
      throw new VesAgentException(VesAgentConstants.INVALID_ARGUMENTS, errorMsg);
    }
  }


  public static DeviceRPCResponse getErrorResponse(DeviceRPCRequest deviceRPCRequest,
      String faultCode, String faultMessage) {
    DeviceRPCResponse errorResponse = new DeviceRPCResponse();

    errorResponse.setDeviceDetails(deviceRPCRequest.getDeviceDetails());

    OperationResponse operationResponse = new OperationResponse();
    operationResponse.setStatus(VesAgentConstants.RPC_FAILED);// device reachable...change value 1
                                                              // to some constant or enum
    operationResponse.setOperationCode(deviceRPCRequest.getOpDetails().getOpCode());

    errorResponse.setOperationResponse(operationResponse);
    errorResponse.setFaultKey(faultCode);
    errorResponse.setFaultString(faultMessage);

    return errorResponse;
  }

  public static DeviceRPCResponse getSuccessResponse(DeviceRPCRequest deviceRPCRequest) {
    DeviceRPCResponse response = new DeviceRPCResponse();

    response.setDeviceDetails(deviceRPCRequest.getDeviceDetails());

    OperationResponse operationResponse = new OperationResponse();
    operationResponse.setStatus(VesAgentConstants.RPC_SUCCESS);
    operationResponse.setOperationCode(deviceRPCRequest.getOpDetails().getOpCode());
    operationResponse.setParameterDTOs(deviceRPCRequest.getOpDetails().getParmeters());

    response.setOperationResponse(operationResponse);
    return response;
  }

  public static boolean isDeviceReachable(DeviceRPCResponse deviceRPCResponse) {
    if (null == deviceRPCResponse || null == deviceRPCResponse.getOperationResponse()) {
      return false;
    }

    if (deviceRPCResponse.getOperationResponse()
        .getStatus() == VesAgentConstants.DEVICE_IS_REACHABLE) {
      return true;
    }

    return (null != deviceRPCResponse.getFaultKey() && deviceRPCResponse.getFaultKey()
        .equalsIgnoreCase(VesAgentConstants.ABORTED_BY_BOOT_BOOTSTRAP));
  }

  public static Boolean isVesNotificationRequest(ParameterDTO param) {
    if (null == param.getParamName() || param.getParamName().isEmpty()) {
      return false;
    }

    return param.getParamName().toLowerCase().contains(VesAgentConstants.HEART_BEAT.toLowerCase());
  }


  public static String getDeviceOperationKey(String deviceId, OperationCode opCode) {
    return deviceId + "-" + opCode;
  }

  public static long getStartEpochTime() {
    Calendar calendar = Calendar.getInstance();

    long minuteEquivalentWithoutSec = calendar.getTimeInMillis();
    minuteEquivalentWithoutSec = (minuteEquivalentWithoutSec / 60000);
    minuteEquivalentWithoutSec = minuteEquivalentWithoutSec * 60 * 1000;

    return minuteEquivalentWithoutSec;
  }

}
