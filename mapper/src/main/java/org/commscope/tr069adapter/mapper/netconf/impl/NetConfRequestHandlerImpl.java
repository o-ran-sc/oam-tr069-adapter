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

package org.commscope.tr069adapter.mapper.netconf.impl;

import java.util.ArrayList;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.mapper.ErrorCodeMetaData;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.model.ErrorCodeDetails;
import org.commscope.tr069adapter.mapper.model.NetConfRequest;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.netconf.NetConfRequestHandler;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.commscope.tr069adapter.mapper.util.ErrorCodeUtil;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.commscope.tr069adapter.mapper.util.NetconfToTr069MapperUtil;
import org.commscope.tr069adapter.mapper.ves.VESNotificationSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

@Component
public class NetConfRequestHandlerImpl implements NetConfRequestHandler {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfRequestHandlerImpl.class);
  private static final String BOOLEAN_TRUE_VALUE = "1";
  private static final String BOOLEAN_FALSE_VALUE = "0";
  private static final String BOOLEAN_DATA_TYPE = "boolean";

  @Autowired
  SynchronizedRequestHandler syncHandler;

  @Autowired
  NetconfToTr069MapperUtil mapperUtil;

  @Autowired
  MOMetaDataUtil metaDataUtil;

  @Autowired
  private ErrorCodeUtil errorCodeUtil;

  @Autowired
  VESNotificationSender vesnotiSender;

  @Override
  public NetConfResponse handleSetConfigRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    DeviceRPCRequest deviceRPCRequest = NetconfToTr069MapperUtil.prepareTR069Request(
        netConfRequest.getDeviceId(), el, "config", TR069OperationCode.SET_PARAMETER_VALUES);

    if (deviceRPCRequest == null) {
      LOG.debug("There are no supported device parameters found for edit-config.");
      return getEmptyResponse();
    }
    List<ParameterDTO> requestParams = deviceRPCRequest.getOpDetails().getParmeters();
    List<ParameterDTO> filteredParams = new ArrayList<>();
    if (null != requestParams) {
      for (ParameterDTO param : requestParams) {
        if (null == param.getParamValue() || StringUtils.isEmpty(param.getParamValue())) {
          continue;
        }
        filteredParams.add(param);
      }
      deviceRPCRequest.getOpDetails().setParmeters(filteredParams);
    }

    if (deviceRPCRequest.getOpDetails() == null
        || deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug("There are no device parameters found for edit-config.");
      return getEmptyResponse();
    } else if (deviceRPCRequest.getOpDetails() != null) {
      deviceRPCRequest.getOpDetails()
          .setParmeters(filteredSetParameters(deviceRPCRequest.getOpDetails().getParmeters()));
      if (deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
        LOG.debug("There are no supported device parameters found for edit-config.");
        return getEmptyResponse();
      }
    }

    handleBooleanParametersReverse(deviceRPCRequest.getOpDetails().getParmeters());
    LOG.debug("Prepared NBI request for edit-config {}", deviceRPCRequest);

    DeviceRPCResponse deviceRPCResponse;
    deviceRPCResponse = syncHandler.performDeviceOperation(deviceRPCRequest);
    if (null == deviceRPCResponse) {
      return getTimeOutResponse();
    }
    response = mapperUtil.getNetconfResponse(deviceRPCResponse);
    return response;
  }

  @Override
  public NetConfResponse handleDelConfigRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    DeviceRPCRequest request = NetconfToTr069MapperUtil.prepareTR069Request(
        netConfRequest.getDeviceId(), el, "config", TR069OperationCode.DELETE_OBJECT);

    if (request == null) {
      LOG.debug("There are no supported device parameters found for delete-config.");
      return getEmptyResponse();
    }

    List<ParameterDTO> requestParams = request.getOpDetails().getParmeters();
    List<ParameterDTO> filteredParams = new ArrayList<>();
    if (null != requestParams) {
      for (ParameterDTO param : requestParams) {
        filteredParams.add(param);
      }
      request.getOpDetails().setParmeters(filteredParams);
    }

    if (request.getOpDetails() == null || request.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug("There are no device parameters found for delete-config.");
      return getEmptyResponse();
    } else if (request.getOpDetails() != null) {
      request.getOpDetails()
          .setParmeters(filteredSetParameters(request.getOpDetails().getParmeters()));
      if (request.getOpDetails().getParmeters().isEmpty()) {
        LOG.debug("There are no supported device parameters found for delete-config.");
        return getEmptyResponse();
      }
    }
    LOG.debug("Prepared NBI request for delete-config {}", request);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(request);
    if (null == opResult) {
      return getTimeOutResponse();
    }
    response = mapperUtil.getNetconfResponse(opResult);
    return response;
  }

  @Override
  public NetConfResponse handleGetRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    DeviceRPCRequest request = NetconfToTr069MapperUtil.prepareTR069Request(
        netConfRequest.getDeviceId(), el, "filter", TR069OperationCode.GET_PARAMETER_VALUES);

    if (request == null || request.getOpDetails() == null
        || request.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug("There are no device parameters found for get.");
      return getEmptyResponse();
    } else if (request.getOpDetails() != null) {

      request.getOpDetails()
          .setParmeters(filteredGetParameters(request.getOpDetails().getParmeters()));

      if (request.getOpDetails().getParmeters().isEmpty()) {
        LOG.debug("There are no supported device parameters found for get.");
        return getEmptyResponse();
      }
    }
    LOG.debug("Prepared NBI request for get {}", request);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(request);
    if (null == opResult) {
      return getTimeOutResponse();
    }
    response = mapperUtil.getNetconfResponse(opResult);
    return response;
  }

  @Override
  public NetConfResponse handleGetConfigRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    DeviceRPCRequest request = NetconfToTr069MapperUtil.prepareTR069Request(
        netConfRequest.getDeviceId(), el, "filter", TR069OperationCode.GET_PARAMETER_VALUES);

    if (request == null || request.getOpDetails() == null
        || request.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug("There are no device parameters found for get-config.");
      return getEmptyResponse();
    } else if (request.getOpDetails() != null) {
      request.getOpDetails()
          .setParmeters(filteredGetParameters(request.getOpDetails().getParmeters()));

      if (request.getOpDetails().getParmeters().isEmpty()) {
        LOG.debug("There are no supported device parameters found for get-config.");
        return getEmptyResponse();
      }
    }
    LOG.debug("Prepared NBI request for get-config {}", request);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(request);
    if (null == opResult) {
      return getTimeOutResponse();
    }
    LOG.debug("Received GPV response : FaultKey = {} FaultString = {} Parameters : {}",
        opResult.getFaultKey(), opResult.getFaultString(),
        opResult.getOperationResponse().getParameterDTOs());
    if (null != opResult.getOperationResponse().getParameterDTOs())
      handleBooleanParameters(opResult.getOperationResponse().getParameterDTOs());
    response = mapperUtil.getNetconfResponse(opResult);

    if (opResult.getFaultKey() != null && opResult.getFaultKey().equalsIgnoreCase("9005")) {
      // check for tabular
      LOG.debug("Tabular Entry not exist in the device; we need to add it now");
      MOMetaData data = metaDataUtil
          .getMetaDataByTR69Name(request.getOpDetails().getParmeters().get(0).getParamName());
      if (data.isTabluarObj()) {
        return getEmptyResponse();
      }
    }

    return response;
  }

  private void handleBooleanParameters(List<ParameterDTO> parameterDTOs) {

    for (ParameterDTO param : parameterDTOs) {
      MOMetaData metaData = metaDataUtil.getMetaDataByTR69Name(param.getParamName());
      if (null != metaData && BOOLEAN_DATA_TYPE.equalsIgnoreCase(metaData.getDataType())) {
        if (BOOLEAN_TRUE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(Boolean.TRUE.toString());
        } else if (BOOLEAN_FALSE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(Boolean.FALSE.toString());
        }
      }
    }
  }

  private void handleBooleanParametersReverse(List<ParameterDTO> parameterDTOs) {

    for (ParameterDTO param : parameterDTOs) {
      MOMetaData metaData = metaDataUtil.getMetaDataByTR69Name(param.getParamName());
      if (null != metaData && BOOLEAN_DATA_TYPE.equalsIgnoreCase(metaData.getDataType())) {
        if (Boolean.TRUE.toString().equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(BOOLEAN_TRUE_VALUE);
        } else if (Boolean.FALSE.toString().equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(BOOLEAN_FALSE_VALUE);
        }
      }
    }
  }

  private NetConfResponse getEmptyResponse() {
    NetConfResponse response = new NetConfResponse();
    ErrorCodeMetaData errorCodeMetaData = errorCodeUtil.getErrorCodeMetaData("0");
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    errorCode.setFaultCode("0");
    errorCode.setErrorMessage(errorCodeMetaData.getErrorMessage());
    errorCode.setErrorType(errorCodeMetaData.getErrorType());
    errorCode.setErrorTag(errorCodeMetaData.getErrorTag());
    errorCode.setErrorSeverity(errorCodeMetaData.getErrorSeverity());
    response.setErrorCode(errorCode);
    return response;
  }

  private NetConfResponse getTimeOutResponse() {
    // prepare timeout error response
    NetConfResponse timeOutErrorResponse = new NetConfResponse();
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    ErrorCodeMetaData errorCodeMetaData = errorCodeUtil.getErrorCodeMetaData("8006");

    errorCode.setFaultCode("8006");
    errorCode.setErrorMessage(errorCodeMetaData.getErrorMessage());
    errorCode.setErrorType(errorCodeMetaData.getErrorType());
    errorCode.setErrorTag(errorCodeMetaData.getErrorTag());
    errorCode.setErrorSeverity(errorCodeMetaData.getErrorSeverity());
    timeOutErrorResponse.setErrorCode(errorCode);
    timeOutErrorResponse.setErrorMessage("TR069 device request has been timed out.");
    return timeOutErrorResponse;
  }

  public List<ParameterDTO> filteredSetParameters(List<ParameterDTO> parameters) {
    List<ParameterDTO> result = new ArrayList<>();
    for (ParameterDTO param : parameters) {
      MOMetaData metaData = metaDataUtil.getMetaDataByNetConfName(param.getParamName());
      if (null != metaData && !metaData.isReadOnly()) {
        String tr069MoName =
            MOMetaDataUtil.getTR69MOByReplacingIndexes(param.getParamName(), metaData.getMoName());
        param.setDataType(metaData.getDataType());
        param.setParamName(tr069MoName);
        result.add(param);
      }
    }
    return result;
  }

  private List<ParameterDTO> filteredGetParameters(List<ParameterDTO> parameters) {

    return metaDataUtil.getSupportedChildParameters(parameters);
  }

  @Override
  public boolean handelRegisterEvent(NetConfServerDetails request) {
    LOG.debug("processing the handelRegisterEvent started");
    boolean result = false;
    try {
      vesnotiSender.sendNotification(null, request);
    } catch (Exception e) {
      LOG.error("processing the handelRegisterEvent exception occurred");
      result = false;
    }
    LOG.debug("processing the handelRegisterEvent completed");
    return result;
  }
}
