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

package org.commscope.tr069adapter.acs.cpe.builder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.ParameterAttributeDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.cpe.TR069RPC;
import org.commscope.tr069adapter.acs.cpe.rpc.AddObjectResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.DeleteObjectResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.DownloadResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.FactoryResetResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.GetParameterAttributesResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.GetParameterAttributesResponse.ParameterAttributeStruct;
import org.commscope.tr069adapter.acs.cpe.rpc.GetParameterValuesResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.RebootResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.SetParameterAttributesResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.SetParameterValuesResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class DeviceRPCResponseBuilder {

  private static final Logger logger = LoggerFactory.getLogger(DeviceRPCResponseBuilder.class);

  /**
   * @param message
   * @return
   */
  public OperationResponse constructDeviceRPCResponse(TR069RPC message) {
    OperationResponse operationResponse = null;
    if (message instanceof SetParameterValuesResponse) {
      logger.debug("Response Msg is SPVResponse");
      SetParameterValuesResponse values = (SetParameterValuesResponse) message;
      operationResponse = buildSetParameterValuesResponse(values.getStatus());
    } else if (message instanceof GetParameterValuesResponse) {
      logger.debug("Response Msg is GPVResponse");
      GetParameterValuesResponse values = (GetParameterValuesResponse) message;
      operationResponse = buildGetParameterValuesResponse(values.getValues());
    } else if (message instanceof SetParameterAttributesResponse) {
      logger.debug("Response Msg is SPAResponse");
      operationResponse = buildSetParameterAttributesResponse();
    } else if (message instanceof GetParameterAttributesResponse) {
      logger.debug("Response Msg is GPAResponse");
      GetParameterAttributesResponse values = (GetParameterAttributesResponse) message;
      operationResponse = buildGetParameterAttributeResponse(values.getAttributes());
    } else if (message instanceof AddObjectResponse) {
      logger.debug("Response Msg is AOResponse");
      AddObjectResponse values = (AddObjectResponse) message;
      operationResponse = buildAddObjectResponse(values.getInstanceNumber(), values.getStatus());
    } else if (message instanceof DeleteObjectResponse) {
      logger.debug("Response Msg is DOResponse");
      DeleteObjectResponse values = (DeleteObjectResponse) message;
      operationResponse = buildDeleteObjectResponse(values.getStatus());
    } else if (message instanceof FactoryResetResponse) {
      logger.debug("Response Msg is FRResponse");
      operationResponse = buildFactoryResetResponse();
    } else if (message instanceof RebootResponse) {
      logger.debug("Response Msg is RebootResponse");
      operationResponse = buildRebootResponse();
    } else if (message instanceof DownloadResponse) {
      logger.debug("Response Msg is DownloadResponse");
      DownloadResponse values = (DownloadResponse) message;
      operationResponse = buildDownloadResponse(values.getStatus());
    }

    return operationResponse;
  }

  /**
   * @param status
   * @return
   */
  private OperationResponse buildSetParameterValuesResponse(int status) {
    org.commscope.tr069adapter.acs.common.response.SetParameterValueResponse setParameterValueResponse =
        new org.commscope.tr069adapter.acs.common.response.SetParameterValueResponse();
    setParameterValueResponse.setStatus(status);
    setParameterValueResponse.setOperationCode(TR069OperationCode.SET_PARAMETER_VALUES);

    return setParameterValueResponse;
  }

  /**
   * @param parameterList
   * @return
   */
  private OperationResponse buildGetParameterValuesResponse(Map<String, String> parameterList) {
    org.commscope.tr069adapter.acs.common.response.GetParameterValueResponse getParameterValuesResponse =
        new org.commscope.tr069adapter.acs.common.response.GetParameterValueResponse();

    List<ParameterDTO> parameterDTOs = new ArrayList<>();
    for (Entry<String, String> entry : parameterList.entrySet()) {
      ParameterDTO parameterDTO = new ParameterDTO();
      parameterDTO.setParamName(entry.getKey());
      parameterDTO.setParamValue(entry.getValue());
      parameterDTOs.add(parameterDTO);
    }

    getParameterValuesResponse.setParameterDTOs(parameterDTOs);
    getParameterValuesResponse.setOperationCode(TR069OperationCode.GET_PARAMETER_VALUES);

    return getParameterValuesResponse;
  }

  /**
   * @return
   */
  private OperationResponse buildSetParameterAttributesResponse() {
    org.commscope.tr069adapter.acs.common.response.SetParameterAttributeResponse setParameterAttributeResponse =
        new org.commscope.tr069adapter.acs.common.response.SetParameterAttributeResponse();
    setParameterAttributeResponse.setOperationCode(TR069OperationCode.SET_PARAMETER_ATTRIBUTES);
    return setParameterAttributeResponse;
  }

  /**
   * @param attributes
   * @return
   */
  private OperationResponse buildGetParameterAttributeResponse(
      ParameterAttributeStruct[] attributes) {
    org.commscope.tr069adapter.acs.common.response.GetParameterAttributeResponse getParameterAttributeResponse =
        new org.commscope.tr069adapter.acs.common.response.GetParameterAttributeResponse();

    List<ParameterDTO> parameterAttributeList = new ArrayList<>();
    for (int i = 0; i < attributes.length; i++) {
      ParameterAttributeStruct parameterAttributeStruct = attributes[i];
      ParameterAttributeDTO parameterAttributeDTO = new ParameterAttributeDTO();
      parameterAttributeDTO.setParamName(parameterAttributeStruct.getName());
      parameterAttributeDTO.setNotification(parameterAttributeStruct.getNotification());
      parameterAttributeDTO.setAccessList(parameterAttributeStruct.getAccessList());
      parameterAttributeList.add(parameterAttributeDTO);
    }

    getParameterAttributeResponse.setParameterDTOs(parameterAttributeList);
    getParameterAttributeResponse.setOperationCode(TR069OperationCode.GET_PARAMETER_ATTRIBUTES);

    return getParameterAttributeResponse;
  }

  /**
   * @param instanceNumber
   * @param status
   * @return
   */
  private OperationResponse buildAddObjectResponse(long instanceNumber, int status) {
    org.commscope.tr069adapter.acs.common.response.AddObjectResponse addObjectResponse =
        new org.commscope.tr069adapter.acs.common.response.AddObjectResponse();
    addObjectResponse.setInstanceNumber(instanceNumber);
    addObjectResponse.setStatus(status);
    addObjectResponse.setOperationCode(TR069OperationCode.ADD_OBJECT);

    return addObjectResponse;
  }

  /**
   * @param status
   * @return
   */
  private OperationResponse buildDeleteObjectResponse(int status) {
    org.commscope.tr069adapter.acs.common.response.DeleteObjectResponse deleteObjectResponse =
        new org.commscope.tr069adapter.acs.common.response.DeleteObjectResponse();
    deleteObjectResponse.setStatus(status);
    deleteObjectResponse.setOperationCode(TR069OperationCode.DELETE_OBJECT);

    return deleteObjectResponse;
  }

  /**
   * @return
   */
  private OperationResponse buildFactoryResetResponse() {
    org.commscope.tr069adapter.acs.common.response.FactoryResetResponse factoryResetResponse =
        new org.commscope.tr069adapter.acs.common.response.FactoryResetResponse();
    factoryResetResponse.setOperationCode(TR069OperationCode.FACTORY_RESET);

    return factoryResetResponse;
  }

  /**
   * @return
   */
  private OperationResponse buildRebootResponse() {
    org.commscope.tr069adapter.acs.common.response.RebootResponse rebootResponse =
        new org.commscope.tr069adapter.acs.common.response.RebootResponse();
    rebootResponse.setOperationCode(TR069OperationCode.REBOOT);

    return rebootResponse;
  }

  /**
   * @param status
   * @return
   */
  private OperationResponse buildDownloadResponse(int status) {
    org.commscope.tr069adapter.acs.common.response.DownloadResponse downloadResponse =
        new org.commscope.tr069adapter.acs.common.response.DownloadResponse();
    downloadResponse.setStatus(status);
    downloadResponse.setOperationCode(TR069OperationCode.DOWNLOAD);

    return downloadResponse;
  }

}
