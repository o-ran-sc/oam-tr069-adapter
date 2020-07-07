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
import java.util.Map;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationOptions;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.dao.DeviceOperationsDAO;
import org.commscope.tr069adapter.mapper.entity.DeviceOperationDetails;
import org.commscope.tr069adapter.mapper.model.ErrorCodeDetails;
import org.commscope.tr069adapter.mapper.model.NetConfRequest;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.netconf.NetConfRequestHandler;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.commscope.tr069adapter.mapper.util.ErrorCodeUtil;
import org.commscope.tr069adapter.mapper.util.FirwareUpgradeStatus;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.commscope.tr069adapter.mapper.util.MapperConstants;
import org.commscope.tr069adapter.mapper.util.MapperValidator;
import org.commscope.tr069adapter.mapper.util.NetconfToTr069MapperUtil;
import org.commscope.tr069adapter.mapper.ves.VESNotificationSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.w3c.dom.Document;
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

  @Autowired
  DeviceOperationsDAO deviceOperDAO;

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
    List<ParameterDTO> vesRequestParams = new ArrayList<>();

    if (null != requestParams) {
      for (ParameterDTO param : requestParams) {
        if (null == param.getParamValue() || StringUtils.isEmpty(param.getParamValue())) {
          continue;
        }
        if (isVesNotificationRequest(param)) {
          vesRequestParams.add(param);
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

    DeviceRPCResponse deviceRPCResponseVes = null;
    List<ParameterDTO> allParamList = deviceRPCRequest.getOpDetails().getParmeters();

    if (!vesRequestParams.isEmpty()) {
      if (!MapperValidator.isCountDownTimerValid(vesRequestParams)) {
        return getErrorResponse(MapperConstants.INVALID_PARAM_VAL_ERROR_CODE,
            MapperConstants.INVALID_COUNT_DOWN_TIMER_MSG);
      }

      deviceRPCRequest.getOpDetails().setParmeters(vesRequestParams);
      deviceRPCResponseVes = vesnotiSender.sendEditConfigNotification(deviceRPCRequest);

      if (null == deviceRPCResponseVes) {
        return getTimeOutResponse();
      }
    }

    allParamList.removeAll(vesRequestParams);

    DeviceRPCResponse deviceRPCResponseDevice = null;
    if (null != allParamList && !allParamList.isEmpty()) {
      deviceRPCRequest.getOpDetails().setParmeters(allParamList);
      deviceRPCResponseDevice = syncHandler.performDeviceOperation(deviceRPCRequest);

      if (null == deviceRPCResponseDevice) {
        return getTimeOutResponse();
      }
    }

    DeviceRPCResponse deviceRPCResponse =
        mergeSetConfigDeviceRPCResponse(deviceRPCResponseVes, deviceRPCResponseDevice);
    if (null == deviceRPCResponse) {
      return getTimeOutResponse();
    }
    response = mapperUtil.getNetconfResponse(deviceRPCResponse, false);
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
    response = mapperUtil.getNetconfResponse(opResult, false);
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
    response = mapperUtil.getNetconfResponse(opResult, false);
    return response;
  }

  @Override
  public NetConfResponse handleGetConfigRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    List<ParameterDTO> vesRequestParams = new ArrayList<>();

    boolean isSoftwareInventory = false;
    if (netConfRequest.getRequestXml().contains("software-inventory")) {
      LOG.info("XML Contains software-inventory");
      isSoftwareInventory = true;
    }

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
      for (ParameterDTO param : request.getOpDetails().getParmeters()) {
        if (isVesNotificationRequest(param)) {
          vesRequestParams.add(param);
        }
      }
    }
    LOG.debug("Prepared NBI request for get-config {}", request);

    DeviceRPCResponse opResultVes = null;
    List<ParameterDTO> allParamList = request.getOpDetails().getParmeters();

    if (!vesRequestParams.isEmpty()) {
      request.getOpDetails().setParmeters(vesRequestParams);
      opResultVes = vesnotiSender.sendGetConfigNotification(request);

      if (null == opResultVes) {
        return getTimeOutResponse();
      }
    }


    allParamList.removeAll(vesRequestParams);

    DeviceRPCResponse opResultDevice = null;
    if (!allParamList.isEmpty()) {
      request.getOpDetails().setParmeters(allParamList);
      opResultDevice = syncHandler.performDeviceOperation(request);

      if (null == opResultDevice) {
        return getTimeOutResponse();
      }
    }

    DeviceRPCResponse opResult = mergeGetConfigDeviceRPCResponse(opResultVes, opResultDevice);

    if (null == opResult) {
      return getTimeOutResponse();
    }
    LOG.debug("Received GPV response : FaultKey = " + opResult.getFaultKey() + ", FaultString = "
        + opResult.getFaultString() + ", Parameters :"
        + opResult.getOperationResponse().getParameterDTOs());
    if (null != opResult.getOperationResponse().getParameterDTOs())
      handleBooleanParameters(opResult.getOperationResponse().getParameterDTOs());

    if (isSoftwareInventory) {
      response = mapperUtil.getNetconfResponseForSoftwareInventory(opResult);
    } else {
      response = mapperUtil.getNetconfResponse(opResult, false);
    }

    if (opResult.getFaultKey() != null && opResult.getFaultKey().equalsIgnoreCase("9005")) {
      // check for tabular
      LOG.debug("Tabualr Entry not exist in the device; we need to add it now");
      MOMetaData data = metaDataUtil
          .getMetaDataByTR69Name(request.getOpDetails().getParmeters().get(0).getParamName());
      if (data.isTabluarObj()) {
        return getEmptyResponse();
      }
    }

    return response;
  }

  @Override
  public NetConfResponse handleSWDownloadRequest(NetConfRequest request) {
    LOG.debug("request received fro sw-download");
    Document d1 = NetconfToTr069MapperUtil.convertStringToDocumentXml(request.getRequestXml());
    NetConfResponse response = null;
    Map<String, String> map =
        NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "software-download");
    if (map == null || map.size() <= 0) {
      LOG.debug("There are no device parameters found for get.");
      return getEmptyResponse();
    }

    TR069OperationDetails opDetails = new TR069OperationDetails();
    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
    tr069DeviceDetails.setDeviceId(request.getDeviceId());
    deviceRPCRequest.setOpDetails(opDetails);
    deviceRPCRequest.setDeviceDetails(tr069DeviceDetails);
    OperationOptions options = new OperationOptions();
    options.setExecutionTimeout(60l);
    deviceRPCRequest.setOptions(options);
    String fileName = map.get("rpc.software-download.remote-file-path");
    String password = map.get("rpc.software-download.password.password");

    if (fileName == null || password == null || getDownloadFileURI(fileName) == null
        || getDownloadUserName(fileName) == null) {
      LOG.error(
          "remote-file-path value is not as per yang model reference. Allowed pattern sftp://<username>@<host>[:<port>]/path");
      return getOperationAbortedResponse(
          "remote-file-path value is not as per yang model reference. Allowed pattern sftp://<username>@<host>[:<port>]/path");
    }

    List<ParameterDTO> paramDTOList = new ArrayList<>();
    paramDTOList.add(new ParameterDTO("FileType", "1 Firmware Upgrade Image"));
    paramDTOList.add(new ParameterDTO("URL", getDownloadFileURI(fileName)));
    paramDTOList.add(new ParameterDTO("Username", getDownloadUserName(fileName)));
    paramDTOList.add(new ParameterDTO("Password", password));
    paramDTOList.add(new ParameterDTO("FileSize", "0"));
    paramDTOList.add(new ParameterDTO("TargetFileName", ""));
    paramDTOList.add(new ParameterDTO("DelaySeconds", "1"));
    paramDTOList.add(new ParameterDTO("SuccessURL", ""));
    paramDTOList.add(new ParameterDTO("FailureURL", ""));

    deviceRPCRequest.getOpDetails().setParmeters(paramDTOList);
    deviceRPCRequest.getOpDetails().setOpCode(TR069OperationCode.DOWNLOAD);

    LOG.debug("Prepared NBI request for download " + deviceRPCRequest);

    DeviceOperationDetails fwDetails = deviceOperDAO.findByDeviceId(request.getDeviceId());
    if (fwDetails == null) {
      String errorMsg = "TR069 device request has been aborted,due to device not identified";
      return getOperationAbortedResponse(errorMsg);
    }

    if (fwDetails.getDownLoadStatus() != FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus()
        && fwDetails.getDownLoadStatus() != FirwareUpgradeStatus.DOWNLOAD_COMPLETED.getStatus()) {

      LOG.debug("persisting the fw details " + fwDetails.toString());

      DeviceRPCResponse opResult;
      opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
      if (null == opResult) {
        return getTimeOutResponse();
      }
      fwDetails.setFileName(fileName);
      fwDetails.setDownLoadStatus(FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus());
      deviceOperDAO.save(fwDetails);
      ArrayList<ParameterDTO> responseParamDTOList = new ArrayList<>();

      if (opResult.getOperationResponse().getStatus() == 1) {
        responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:status", "STARTED"));
      } else {
        responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:status", "FAILED"));
        responseParamDTOList
            .add(new ParameterDTO("rpc-reply.ns1:error-message", opResult.getFaultString()));
      }
      responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:notification-timeout", "1200"));

      opResult.getOperationResponse().setParameterDTOs(responseParamDTOList);
      response = mapperUtil.getNetconfResponse(opResult, true);

      LOG.debug("update the status for fw details " + fwDetails.toString());
    } else {
      LOG.debug("FirmWare Upgrade is in progress");
      String errorMsg = "TR069 device request has been aborted as Firmware Upgrade is inProgress";
      return getOperationAbortedResponse(errorMsg);
    }

    return response;
  }

  private DeviceRPCResponse mergeGetConfigDeviceRPCResponse(DeviceRPCResponse opResultVes,
      DeviceRPCResponse opResultDevice) {
    if (null == opResultVes) {
      return opResultDevice;
    }

    if (null == opResultDevice) {
      return opResultVes;
    }

    if (null != opResultVes.getFaultKey()
        && !opResultVes.getFaultKey().equals(MapperConstants.RPC_SUCCESS)) {
      return opResultVes;
    } else if (null != opResultDevice.getFaultKey()
        && !opResultDevice.getFaultKey().equals(MapperConstants.RPC_SUCCESS)) {
      return opResultDevice;
    }

    opResultDevice.getOperationResponse().getParameterDTOs()
        .addAll(opResultVes.getOperationResponse().getParameterDTOs());
    return opResultDevice;
  }

  private DeviceRPCResponse mergeSetConfigDeviceRPCResponse(DeviceRPCResponse opResultVes,
      DeviceRPCResponse opResultDevice) {
    if (null == opResultVes) {
      return opResultDevice;
    }

    if (null == opResultDevice) {
      return opResultVes;
    }

    return opResultDevice;
  }

  private boolean isVesNotificationRequest(ParameterDTO param) {
    if (null == param.getParamName() || param.getParamName().isEmpty()) {
      return false;
    }

    if (param.getParamName().toLowerCase().contains(MapperConstants.HEART_BEAT_PERIOD.toLowerCase())
        || param.getParamName().toLowerCase()
            .contains(MapperConstants.COUNT_DOWN_TIMER.toLowerCase())
        || param.getParamName().toLowerCase().contains(MapperConstants.HEART_BEAT.toLowerCase())) {
      return true;
    }

    return false;
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
    ErrorCodeDetails errorCodeMetaData = errorCodeUtil.getErrorCodeMetaData("0");
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
    ErrorCodeDetails errorCodeMetaData = errorCodeUtil.getErrorCodeMetaData("8006");

    errorCode.setFaultCode("8006");
    errorCode.setErrorMessage(errorCodeMetaData.getErrorMessage());
    errorCode.setErrorType(errorCodeMetaData.getErrorType());
    errorCode.setErrorTag(errorCodeMetaData.getErrorTag());
    errorCode.setErrorSeverity(errorCodeMetaData.getErrorSeverity());
    timeOutErrorResponse.setErrorCode(errorCode);
    timeOutErrorResponse.setErrorMessage("TR069 device request has been timed out.");
    return timeOutErrorResponse;
  }

  private NetConfResponse getErrorResponse(String errCode, String errorMsg) {
    NetConfResponse errorResponse = new NetConfResponse();
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    ErrorCodeDetails errorCodeMetaData = errorCodeUtil.getErrorCodeMetaData("8006");
    if (errorCodeMetaData != null) {
      errorCode.setFaultCode(errCode);
      errorCode.setErrorMessage(errorCodeMetaData.getErrorMessage());
      errorCode.setErrorType(errorCodeMetaData.getErrorType());
      errorCode.setErrorTag(errorCodeMetaData.getErrorTag());
      errorCode.setErrorSeverity(errorCodeMetaData.getErrorSeverity());
    }
    errorResponse.setErrorCode(errorCode);
    errorResponse.setErrorMessage(errorMsg);
    return errorResponse;
  }

  private NetConfResponse getOperationAbortedResponse(String errorMessage) {
    // prepare timeout error response
    NetConfResponse timeOutErrorResponse = new NetConfResponse();
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    ErrorCodeDetails errorCodeMetaData = errorCodeUtil.getErrorCodeMetaData("8006");
    if (errorCode != null) {
      errorCode.setFaultCode("8002");
      errorCode.setErrorMessage(errorCodeMetaData.getErrorMessage());
      errorCode.setErrorType(errorCodeMetaData.getErrorType());
      errorCode.setErrorTag(errorCodeMetaData.getErrorTag());
      errorCode.setErrorSeverity(errorCodeMetaData.getErrorSeverity());
    }
    timeOutErrorResponse.setErrorCode(errorCode);
    timeOutErrorResponse.setErrorMessage(errorMessage);
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

  private static String getDownloadFileURI(String filepath) {

    if (filepath.contains("@") && filepath.contains("//")) {
      String[] str = filepath.split("@");
      String[] strForUserName = str[0].split("//");
      if (str.length > 1) {
        String Url = strForUserName[0] + "//" + str[1];
        return Url;
      }
    }
    return null;
  }

  private static String getDownloadUserName(String filepath) {

    if (filepath.contains("@") && filepath.contains("//")) {
      String[] str = filepath.split("@");
      String[] strForUserName = str[0].split("//");
      if (strForUserName.length > 1)
        return strForUserName[1];
    }
    return null;
  }
}
