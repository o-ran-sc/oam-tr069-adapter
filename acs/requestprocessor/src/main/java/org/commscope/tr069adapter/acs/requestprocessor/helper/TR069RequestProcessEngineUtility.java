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

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.OPERATION_EXPIRATION_TIMEOUT;

import java.util.ArrayList;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.dto.CustomOperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.faults.AcsFaultCode;
import org.commscope.tr069adapter.acs.common.response.AddObjectResponse;
import org.commscope.tr069adapter.acs.common.response.DeleteObjectResponse;
import org.commscope.tr069adapter.acs.common.response.DownloadResponse;
import org.commscope.tr069adapter.acs.common.response.FactoryResetResponse;
import org.commscope.tr069adapter.acs.common.response.GetParameterAttributeResponse;
import org.commscope.tr069adapter.acs.common.response.GetParameterValueResponse;
import org.commscope.tr069adapter.acs.common.response.RebootResponse;
import org.commscope.tr069adapter.acs.common.response.SetParameterAttributeResponse;
import org.commscope.tr069adapter.acs.common.response.SetParameterValueResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class TR069RequestProcessEngineUtility {

  private static final Logger logger =
      LoggerFactory.getLogger(TR069RequestProcessEngineUtility.class);

  /**
   * Builds a operation aborted operation request
   * 
   * @param deviceDetails
   * @param nbiDeviceOperationrequest
   * @param acsFaultCode
   * @return
   */
  public DeviceRPCResponse buildAbortedOperationresult(DeviceDetails deviceDetails,
      DeviceRPCRequest nbiDeviceOperationrequest, AcsFaultCode acsFaultCode) {
    Long operationId = nbiDeviceOperationrequest.getOperationId();

    TR069OperationCode operationCode = null;
    if (nbiDeviceOperationrequest.getOpDetails().getOpCode() instanceof CustomOperationCode) {
      operationCode = TR069OperationCode.GET_PARAMETER_VALUES;
    } else {
      operationCode = (TR069OperationCode) nbiDeviceOperationrequest.getOpDetails().getOpCode();
    }

    return buildOperationresult(deviceDetails, operationId, operationCode, acsFaultCode);

  }

  /**
   * @param deviceDetails
   * @param operationId
   * @param operationCode
   * @return
   */
  public DeviceRPCResponse buildTimedOutOperationResult(DeviceDetails deviceDetails,
      DeviceRPCRequest deviceRPCRequest) {
    Long operationId = deviceRPCRequest.getOperationId();

    TR069OperationCode operationCode = null;
    if (deviceRPCRequest.getOpDetails().getOpCode() instanceof CustomOperationCode) {
      CustomOperationCode customOperationCode =
          (CustomOperationCode) deviceRPCRequest.getOpDetails().getOpCode();
      if (CustomOperationCode.CONFIGURE_MULTIPLE_OBJECTS.equals(customOperationCode)) {
        operationCode = TR069OperationCode.GET_PARAMETER_VALUES;
      } else if (CustomOperationCode.CONNECT.equals(customOperationCode)) {
        operationCode = TR069OperationCode.INITIATE_CR;
      }
    } else {
      operationCode = (TR069OperationCode) deviceRPCRequest.getOpDetails().getOpCode();
    }
    DeviceRPCResponse deviceRPCResponse = buildOperationresult(deviceDetails, operationId,
        operationCode, AcsFaultCode.FAULT_CODE_8003);
    OperationResponse operationResponse = deviceRPCResponse.getOperationResponse();
    operationResponse.setStatus(OPERATION_EXPIRATION_TIMEOUT);
    return deviceRPCResponse;
  }

  /**
   * Builds a operation aborted operation request
   * 
   * @param deviceDetails
   * @param nbiDeviceOperationrequest
   * @param acsFaultCode
   * @return
   */
  private DeviceRPCResponse buildOperationresult(DeviceDetails deviceDetails, Long operationId,
      TR069OperationCode operationCode, AcsFaultCode acsFaultCode) {
    DeviceRPCResponse deviceRPCResponse = new DeviceRPCResponse();
    deviceRPCResponse.setDeviceDetails(deviceDetails);
    deviceRPCResponse.setOperationId(operationId);

    deviceRPCResponse.setFaultKey(acsFaultCode.getFaultKey());
    deviceRPCResponse.setFaultString(acsFaultCode.getFaultString());

    OperationResponse opResponse = null;
    switch (operationCode) {
      case GET_RPC_METHODS:
      case GET_PARAMETER_NAMES:
        break;
      case SET_PARAMETER_VALUES:
        opResponse = new SetParameterValueResponse();
        opResponse.setParameterDTOs(new ArrayList<>());
        break;
      case GET_PARAMETER_VALUES:
      case INITIATE_CR:
        opResponse = new GetParameterValueResponse();
        opResponse.setParameterDTOs(new ArrayList<>());
        break;
      case SET_PARAMETER_ATTRIBUTES:
        opResponse = new SetParameterAttributeResponse();
        opResponse.setParameterDTOs(new ArrayList<>());
        break;
      case GET_PARAMETER_ATTRIBUTES:
        opResponse = new GetParameterAttributeResponse();
        opResponse.setParameterDTOs(new ArrayList<>());
        break;
      case ADD_OBJECT:
        opResponse = new AddObjectResponse();
        opResponse.setParameterDTOs(new ArrayList<>());
        break;
      case DELETE_OBJECT:
        opResponse = new DeleteObjectResponse();
        opResponse.setParameterDTOs(new ArrayList<>());
        break;
      case REBOOT:
        opResponse = new RebootResponse();
        break;
      case DOWNLOAD:
        opResponse = new DownloadResponse();
        break;
      case SCHEDULE_DOWNLOAD:
      case UPLOAD:
      case FACTORY_RESET:
        opResponse = new FactoryResetResponse();
        break;
      default:
        String operationCodeName = operationCode.name();
        logger.debug("An unsupported operation: {}, hence returning without response object",
            operationCodeName);
        break;
    }
    deviceRPCResponse.setOperationResponse(opResponse);

    return deviceRPCResponse;
  }

}
