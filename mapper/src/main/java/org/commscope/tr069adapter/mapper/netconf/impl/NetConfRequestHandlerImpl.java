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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationOptions;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.CustomOperationCode;
import org.commscope.tr069adapter.acs.common.dto.ParameterAttributeDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.acs.common.response.AddObjectResponse;
import org.commscope.tr069adapter.acs.common.response.SetParameterValueResponse;
import org.commscope.tr069adapter.acs.common.utils.ConnectionStatusPOJO;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.acs.ACSRequestSender;
import org.commscope.tr069adapter.mapper.dao.DeviceOperationsDAO;
import org.commscope.tr069adapter.mapper.entity.DeviceOperationDetails;
import org.commscope.tr069adapter.mapper.model.ErrorCodeDetails;
import org.commscope.tr069adapter.mapper.model.NetConfRequest;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.mapper.netconf.NetConfRequestHandler;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.commscope.tr069adapter.mapper.util.ErrorCodeUtil;
import org.commscope.tr069adapter.mapper.util.FirwareUpgradeStatus;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.commscope.tr069adapter.mapper.util.MapperConstants;
import org.commscope.tr069adapter.mapper.util.NetconfToTr069MapperUtil;
import org.commscope.tr069adapter.mapper.ves.VESNotificationSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

@Component
public class NetConfRequestHandlerImpl implements NetConfRequestHandler {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfRequestHandlerImpl.class);
  private static final String BOOLEAN_TRUE_VALUE = "1";
  private static final String BOOLEAN_FALSE_VALUE = "0";
  private static final String BOOLEAN_DATA_TYPE = "boolean";
  private static final String CONFIG = "config";
  private static final String FILTER = "filter";
  private static final String NO_DEVICE_PARAM_FOUND =
      "There are no device parameters found for get.";
  private static final String RPC_REPLY_STATUS = "rpc-reply.ns1:status";
  private static final String RPC_REPLY_ERROR = "rpc-reply.ns1:error-message";
  private static final String FAILED = "FAILED";
  private static final String PARAMETER = "parameter";

  @Autowired
  SynchronizedRequestHandler syncHandler;

  @Autowired
  NetconfToTr069MapperUtil mapperUtil;

  @Autowired
  MOMetaDataUtil metaDataUtil;

  @Autowired
  ErrorCodeUtil errorCodeUtil;

  @Autowired
  VESNotificationSender vesnotiSender;

  @Autowired
  DeviceOperationsDAO deviceOperDAO;

  @Autowired
  ACSRequestSender tr069ReqSender;

  @Override
  public NetConfResponse handleSetConfigRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    DeviceRPCRequest deviceRPCRequest = NetconfToTr069MapperUtil.prepareTR069Request(
        netConfRequest.getDeviceId(), el, CONFIG, TR069OperationCode.SET_PARAMETER_VALUES);

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
          .setParmeters(filteredSetParameters(deviceRPCRequest.getOpDetails().getParmeters(),
              netConfRequest.getSwVersion(), netConfRequest.getHwVersion()));
      if (deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
        LOG.debug("There are no supported device parameters found for edit-config.");
        return getEmptyResponse();
      }
    }

    handleBooleanParametersReverse(deviceRPCRequest.getOpDetails().getParmeters(),
        netConfRequest.getSwVersion(), netConfRequest.getHwVersion());
    LOG.debug("Prepared NBI request for edit-config {}", deviceRPCRequest);

    DeviceRPCResponse deviceRPCResponseVes = null;
    List<ParameterDTO> allParamList = deviceRPCRequest.getOpDetails().getParmeters();

    if (!vesRequestParams.isEmpty()) {
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
      if (isAdminStateOverriden(allParamList)) {
        deviceRPCRequest.getOpDetails().setOpCode(CustomOperationCode.CONFIGURE_MULTIPLE_OBJECTS);
        TR069OperationDetails tr069OperationDetails =
            (TR069OperationDetails) deviceRPCRequest.getOpDetails();
        tr069OperationDetails.setModifyParamList(allParamList);
        tr069OperationDetails.setSetParamList(null);

        deviceRPCResponseDevice = syncHandler.performDeviceOperation(deviceRPCRequest);
        convertResposeToSPVResponse(deviceRPCResponseDevice);
      } else {
        deviceRPCResponseDevice = syncHandler.performDeviceOperation(deviceRPCRequest);
      }

      if (null == deviceRPCResponseDevice) {
        return getTimeOutResponse();
      }
    }

    DeviceRPCResponse deviceRPCResponse =
        mergeSetConfigDeviceRPCResponse(deviceRPCResponseVes, deviceRPCResponseDevice);
    if (null == deviceRPCResponse) {
      return getTimeOutResponse();
    }
    response = mapperUtil.getNetconfResponse(deviceRPCResponse, netConfRequest.getSwVersion(),
        netConfRequest.getHwVersion(), false);
    return response;
  }

  @Override
  public NetConfResponse handleDelConfigRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    DeviceRPCRequest request = NetconfToTr069MapperUtil.prepareTR069Request(
        netConfRequest.getDeviceId(), el, CONFIG, TR069OperationCode.DELETE_OBJECT);

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
          .setParmeters(filteredSetParameters(request.getOpDetails().getParmeters(),
              netConfRequest.getSwVersion(), netConfRequest.getHwVersion()));
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
    response = mapperUtil.getNetconfResponse(opResult, netConfRequest.getSwVersion(),
        netConfRequest.getHwVersion(), false);
    return response;
  }

  @Override
  public NetConfResponse handleGetRequest(NetConfRequest netConfRequest) {
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    DeviceRPCRequest request = NetconfToTr069MapperUtil.prepareTR069Request(
        netConfRequest.getDeviceId(), el, FILTER, TR069OperationCode.GET_PARAMETER_VALUES);

    if (request == null || request.getOpDetails() == null
        || request.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug(NO_DEVICE_PARAM_FOUND);
      return getEmptyResponse();
    } else if (request.getOpDetails() != null) {

      request.getOpDetails()
          .setParmeters(filteredGetParameters(request.getOpDetails().getParmeters(),
              netConfRequest.getSwVersion(), netConfRequest.getHwVersion()));

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
    response = mapperUtil.getNetconfResponse(opResult, netConfRequest.getSwVersion(),
        netConfRequest.getHwVersion(), false);
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
        netConfRequest.getDeviceId(), el, FILTER, TR069OperationCode.GET_PARAMETER_VALUES);

    if (request == null || request.getOpDetails() == null
        || request.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug("There are no device parameters found for get-config.");
      return getEmptyResponse();
    } else if (request.getOpDetails() != null) {
      request.getOpDetails()
          .setParmeters(filteredGetParameters(request.getOpDetails().getParmeters(),
              netConfRequest.getSwVersion(), netConfRequest.getHwVersion()));

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
    LOG.debug("Received GPV response : FaultKey = {}, FaultString = {}, Parameters : {}",
        opResult.getFaultKey(), opResult.getFaultString(),
        opResult.getOperationResponse().getParameterDTOs());
    if (null != opResult.getOperationResponse().getParameterDTOs())
      handleBooleanParameters(opResult.getOperationResponse().getParameterDTOs(),
          netConfRequest.getSwVersion(), netConfRequest.getHwVersion());

    if (isSoftwareInventory) {
      response = mapperUtil.getNetconfResponseForSoftwareInventory(opResult,
          netConfRequest.getSwVersion(), netConfRequest.getHwVersion());
    } else {
      response = mapperUtil.getNetconfResponse(opResult, netConfRequest.getSwVersion(),
          netConfRequest.getHwVersion(), false);
    }

    if (opResult.getFaultKey() != null && opResult.getFaultKey().equalsIgnoreCase("9005")) {
      // check for tabular
      LOG.debug("Tabualr Entry not exist in the device; we need to add it now");
      MOMetaData data = metaDataUtil.getMetaDataByTR69Name(
          request.getOpDetails().getParmeters().get(0).getParamName(),
          netConfRequest.getSwVersion(), netConfRequest.getHwVersion());
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
      LOG.debug(NO_DEVICE_PARAM_FOUND);
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

    LOG.debug("Prepared NBI request for download {}", deviceRPCRequest);

    DeviceOperationDetails fwDetails = deviceOperDAO.findByDeviceId(request.getDeviceId());
    if (fwDetails == null) {
      String errorMsg = "TR069 device request has been aborted,due to device not identified";
      return getOperationAbortedResponse(errorMsg);
    }

    if (fwDetails.getDownLoadStatus() != FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus()
        && fwDetails.getDownLoadStatus() != FirwareUpgradeStatus.DOWNLOAD_COMPLETED.getStatus()) {

      LOG.debug("persisting the fw details {}", fwDetails);

      DeviceRPCResponse opResult;
      opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
      if (null == opResult) {
        return getTimeOutResponse();
      }
      fwDetails.setFileName(fileName);
      fwDetails.setDownLoadStatus(FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus());
      fwDetails.setOrigin("sdnr");
      deviceOperDAO.save(fwDetails);
      ArrayList<ParameterDTO> responseParamDTOList = new ArrayList<>();

      if (opResult.getOperationResponse().getStatus() == 1) {
        responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, "STARTED"));
      } else {
        responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, FAILED));
        responseParamDTOList.add(new ParameterDTO(RPC_REPLY_ERROR, opResult.getFaultString()));
      }
      responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:notification-timeout", "1200"));

      opResult.getOperationResponse().setParameterDTOs(responseParamDTOList);
      response = mapperUtil.getNetconfResponse(opResult, request.getSwVersion(),
          request.getHwVersion(), true);

      LOG.debug("update the status for fw details {}", fwDetails);
    } else {
      LOG.debug("FirmWare Upgrade is in progress");
      String errorMsg = "TR069 device request has been aborted as Firmware Upgrade is inProgress";
      return getOperationAbortedResponse(errorMsg);
    }

    return response;
  }

  @Override
  public NetConfResponse handleAddObjectRequest(NetConfRequest request) {
    LOG.debug("request received for addObject");
    Document d1 = NetconfToTr069MapperUtil.convertStringToDocumentXml(request.getRequestXml());
    NetConfResponse response = null;
    Map<String, String> map =
        NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "add-object");
    if (map == null || map.size() <= 0) {
      LOG.debug(NO_DEVICE_PARAM_FOUND);
      return getEmptyResponse();
    }

    Element el = NetconfToTr069MapperUtil.convertStringToDocument(request.getRequestXml());
    DeviceRPCRequest deviceRPCRequest = NetconfToTr069MapperUtil
        .prepareTR069Request(request.getDeviceId(), el, PARAMETER, TR069OperationCode.ADD_OBJECT);

    if (deviceRPCRequest == null || deviceRPCRequest.getOpDetails() == null
        || deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug("There are no device parameters found for addobject.");
      return getEmptyResponse();
    } else if (deviceRPCRequest.getOpDetails() != null) {
      deviceRPCRequest.getOpDetails()
          .setParmeters(filteredGetParameters(deviceRPCRequest.getOpDetails().getParmeters(),
              request.getSwVersion(), request.getHwVersion()));
      if (deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
        LOG.debug("There are no supported device parameters found for addobject.");
        return getEmptyResponse();
      }
    }

    LOG.debug("Prepared NBI request for addobject {}", deviceRPCRequest);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
    if (null == opResult) {
      return getTimeOutResponse();
    }
    if (null == opResult.getOperationResponse()) {
      return getTimeOutResponse();
    }
    AddObjectResponse addOpresult = (AddObjectResponse) opResult.getOperationResponse();

    ArrayList<ParameterDTO> responseParamDTOList = new ArrayList<>();

    if (opResult.getFaultKey() == null) {
      String status = String.valueOf(opResult.getOperationResponse().getStatus());
      responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, status));
      String instanceNumber = String.valueOf(addOpresult.getInstanceNumber());
      LOG.info("AddObject Passed : Instance Number: {}", instanceNumber);
      responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:instance-number", instanceNumber));
    }
    LOG.info("AddObject Label value: {}", addOpresult.getLabel());
    if (null == addOpresult.getLabel()) {
      responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:label", ""));
    } else {
      responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:label", addOpresult.getLabel()));
    }

    opResult.getOperationResponse().setParameterDTOs(responseParamDTOList);
    response = mapperUtil.getNetconfResponse(opResult, request.getSwVersion(),
        request.getHwVersion(), true);

    return response;
  }

  @Override
  public NetConfResponse handleDeleteObjectRequest(NetConfRequest request) {
    LOG.debug("request received for deleteObject");
    Document d1 = NetconfToTr069MapperUtil.convertStringToDocumentXml(request.getRequestXml());
    NetConfResponse response = null;
    Map<String, String> map =
        NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "delete-object");
    if (map == null || map.size() <= 0) {
      LOG.debug(NO_DEVICE_PARAM_FOUND);
      return getEmptyResponse();
    }

    Element el = NetconfToTr069MapperUtil.convertStringToDocument(request.getRequestXml());
    DeviceRPCRequest deviceRPCRequest = NetconfToTr069MapperUtil.prepareTR069Request(
        request.getDeviceId(), el, PARAMETER, TR069OperationCode.DELETE_OBJECT);

    if (deviceRPCRequest == null || deviceRPCRequest.getOpDetails() == null
        || deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
      LOG.debug("There are no device parameters found for deleteobject.");
      return getEmptyResponse();
    } else if (deviceRPCRequest.getOpDetails() != null) {
      deviceRPCRequest.getOpDetails()
          .setParmeters(filteredGetParameters(deviceRPCRequest.getOpDetails().getParmeters(),
              request.getSwVersion(), request.getHwVersion()));

      if (deviceRPCRequest.getOpDetails().getParmeters().isEmpty()) {
        LOG.debug("There are no supported device parameters found for deleteobject.");
        return getEmptyResponse();
      }
    }

    LOG.debug("Prepared NBI request for addobject: {}", deviceRPCRequest);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
    if (null == opResult) {
      return getTimeOutResponse();
    }
    if (null == opResult.getOperationResponse()) {
      return getTimeOutResponse();
    }

    ArrayList<ParameterDTO> responseParamDTOList = new ArrayList<>();

    if (opResult.getFaultKey() == null) {
      String status = String.valueOf(opResult.getOperationResponse().getStatus());
      responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, status));
    }
    opResult.getOperationResponse().setParameterDTOs(responseParamDTOList);
    response = mapperUtil.getNetconfResponse(opResult, request.getSwVersion(),
        request.getHwVersion(), true);

    return response;
  }

  @Override
  public NetConfResponse handleRequestWithoutInputParams(NetConfRequest request) {
    Document d1 = NetconfToTr069MapperUtil.convertStringToDocumentXml(request.getRequestXml());
    NetConfResponse response = null;
    TR069OperationDetails opDetails = new TR069OperationDetails();
    Map<String, String> map = null;

    if (request.getRequestXml().contains("reboot")) {
      LOG.info("Request Contains Reboot");
      map = NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "reboot");
      opDetails.setOpCode(TR069OperationCode.REBOOT);
    } else if (request.getRequestXml().contains("reset")) {
      LOG.info("Request Contains Reset");
      map = NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "reset");
      opDetails.setOpCode(TR069OperationCode.FACTORY_RESET);
    }

    if (map == null || map.size() <= 0) {
      LOG.debug(NO_DEVICE_PARAM_FOUND);
      return getEmptyResponse();
    }

    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
    tr069DeviceDetails.setDeviceId(request.getDeviceId());
    deviceRPCRequest.setOpDetails(opDetails);
    deviceRPCRequest.setDeviceDetails(tr069DeviceDetails);
    OperationOptions options = new OperationOptions();
    options.setExecutionTimeout(60l);
    deviceRPCRequest.setOptions(options);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
    if (null == opResult) {
      return getTimeOutResponse();
    }
    LOG.debug("Received response for request without input params : FaultKey = {}",
        opResult.getFaultKey());

    response = mapperUtil.getNetconfResponseForRequestWithoutInputParams(opResult);

    return response;
  }

  @Override
  public NetConfResponse handleSPAObjectRequest(NetConfRequest request) {
    LOG.debug("request received for spaObject");
    Document d1 = NetconfToTr069MapperUtil.convertStringToDocumentXml(request.getRequestXml());
    NetConfResponse response = null;
    Map<String, String> map =
        NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "set-parameter-attributes");
    if (map == null || map.size() <= 0) {
      LOG.debug(NO_DEVICE_PARAM_FOUND);
      return getEmptyResponse();
    }
    List<ParameterDTO> params = new ArrayList<>();
    NodeList nl = d1.getElementsByTagName(CONFIG);
    int len = nl.getLength();
    for (int i = 0; i < len; i++) {
      Element elm = (Element) nl.item(i);
      ParameterAttributeDTO param = mapperUtil.getParamNameAndValueForSPA(elm, CONFIG,
          request.getSwVersion(), request.getHwVersion());

      if (param == null) {
        LOG.debug("There are no device parameters found for spaconfig.");
        return getEmptyResponse();
      }

      params.add(param);
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

    deviceRPCRequest.getOpDetails().setParmeters(params);
    deviceRPCRequest.getOpDetails().setOpCode(TR069OperationCode.SET_PARAMETER_ATTRIBUTES);

    LOG.debug("Prepared NBI request for spaobject {} ", deviceRPCRequest);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
    if (null == opResult) {
      return getTimeOutResponse();
    }

    ArrayList<ParameterDTO> responseParamDTOList = new ArrayList<>();

    if (opResult.getFaultKey() == null) {
      responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, "SUCCESS"));
    } else {
      responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, FAILED));
      responseParamDTOList.add(new ParameterDTO(RPC_REPLY_ERROR, opResult.getFaultString()));
    }

    opResult.getOperationResponse().setParameterDTOs(responseParamDTOList);
    response = mapperUtil.getNetconfResponse(opResult, request.getSwVersion(),
        request.getHwVersion(), true);

    return response;
  }

  @Override
  public NetConfResponse handleGPAObjectRequest(NetConfRequest netConfRequest) {
    LOG.debug("request received for gpaObject");
    Document d1 =
        NetconfToTr069MapperUtil.convertStringToDocumentXml(netConfRequest.getRequestXml());
    NetConfResponse response = null;
    Map<String, String> map =
        NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "get-parameter-attributes");
    if (map == null || map.size() <= 0) {
      LOG.debug(NO_DEVICE_PARAM_FOUND);
      return getEmptyResponse();
    }
    List<ParameterDTO> params = new ArrayList<>();
    NodeList nl = d1.getElementsByTagName(FILTER);
    int len = nl.getLength();
    for (int i = 0; i < len; i++) {
      Element elm = (Element) nl.item(i);
      ParameterDTO param = mapperUtil.getParamNameAndValueForGPA(elm, PARAMETER,
          netConfRequest.getSwVersion(), netConfRequest.getHwVersion());

      if (param == null) {
        LOG.debug("There are no device parameters found for GPA.");
        return getEmptyResponse();
      }

      params.add(param);
    }

    TR069OperationDetails opDetails = new TR069OperationDetails();
    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
    tr069DeviceDetails.setDeviceId(netConfRequest.getDeviceId());
    deviceRPCRequest.setOpDetails(opDetails);
    deviceRPCRequest.setDeviceDetails(tr069DeviceDetails);
    OperationOptions options = new OperationOptions();
    options.setExecutionTimeout(60l);
    deviceRPCRequest.setOptions(options);

    deviceRPCRequest.getOpDetails().setParmeters(params);
    deviceRPCRequest.getOpDetails().setOpCode(TR069OperationCode.GET_PARAMETER_ATTRIBUTES);

    LOG.debug("Prepared NBI request for gpaobject {}", deviceRPCRequest);

    DeviceRPCResponse opResult;
    opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
    if (null == opResult) {
      return getTimeOutResponse();
    }

    response = mapperUtil.getNetconfResponseForGPA(opResult, netConfRequest.getSwVersion(),
        netConfRequest.getHwVersion());

    return response;
  }

  @Override
  public NetConfResponse handleConnectionStatusRequest(NetConfRequest request) {
    NetConfResponse netConfResponse = new NetConfResponse();
    ConnectionStatusPOJO connStatusPOJO =
        tr069ReqSender.sendConnectionStatusReq(request.getDeviceId());

    List<ParameterDTO> paramDTOList = new ArrayList<>();
    DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
    if (connStatusPOJO.isStatus()) {
      paramDTOList.add(new ParameterDTO("rpc-reply.ns1:last-access-status", "SUCCESS"));
    } else {
      paramDTOList.add(new ParameterDTO("rpc-reply.ns1:last-access-status", FAILED));
      paramDTOList.add(new ParameterDTO(RPC_REPLY_ERROR, connStatusPOJO.getErrorMessage()));
    }
    String lastContactDate = dateFormat.format(connStatusPOJO.getLastContactTime());
    paramDTOList.add(new ParameterDTO("rpc-reply.ns1:last-contact-time", lastContactDate));

    String lastFailedAttemptDate = dateFormat.format(connStatusPOJO.getLastFailedAttemptTime());
    paramDTOList
        .add(new ParameterDTO("rpc-reply.ns1:last-failure-attempt-time", lastFailedAttemptDate));

    String xml = mapperUtil.getNetconfResponseXML(paramDTOList, request.getSwVersion(),
        request.getHwVersion(), true);
    LOG.debug("handleConnectionStatusRequest XML String: {}", xml);
    netConfResponse.setNetconfResponseXml(xml);
    return netConfResponse;
  }

  @Override
  public NetConfResponse handleDownloadRequest(NetConfRequest request) {
    LOG.debug("request received for download");
    Document d1 = NetconfToTr069MapperUtil.convertStringToDocumentXml(request.getRequestXml());
    NetConfResponse response = null;
    Map<String, String> map =
        NetconfToTr069MapperUtil.extractRequestParamters(d1, "rpc", "download");
    if (map == null || map.size() <= 0) {
      LOG.debug(NO_DEVICE_PARAM_FOUND);
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
    String fileName = map.get("rpc.download.target-file-name");
    String password = map.get("rpc.download.password");
    String url = map.get("rpc.download.url");
    String userName = map.get("rpc.download.username");

    if (fileName == null || password == null || url == null || userName == null) {
      LOG.error("fileName or password or url or userName is not as per yang model");
      return getOperationAbortedResponse(
          "fileName or password or url or userName is not as per yang model");
    }

    List<ParameterDTO> paramDTOList = new ArrayList<>();
    paramDTOList.add(new ParameterDTO("FileType", map.get("rpc.download.file-type")));
    paramDTOList.add(new ParameterDTO("URL", url));
    paramDTOList.add(new ParameterDTO("Username", userName));
    paramDTOList.add(new ParameterDTO("Password", password));
    paramDTOList.add(new ParameterDTO("FileSize", map.get("rpc.download.file-size")));
    paramDTOList.add(new ParameterDTO("TargetFileName", fileName));
    paramDTOList.add(new ParameterDTO("DelaySeconds", map.get("rpc.download.delay-in-seconds")));
    paramDTOList.add(new ParameterDTO("SuccessURL", map.get("rpc.download.success-url")));
    paramDTOList.add(new ParameterDTO("FailureURL", map.get("rpc.download.failure-url")));
    paramDTOList.add(new ParameterDTO("CommandKey", map.get("rpc.download.command-key")));

    deviceRPCRequest.getOpDetails().setParmeters(paramDTOList);
    deviceRPCRequest.getOpDetails().setOpCode(TR069OperationCode.DOWNLOAD);

    LOG.debug("Prepared NBI request for download {} ", deviceRPCRequest);

    DeviceOperationDetails fwDetails = deviceOperDAO.findByDeviceId(request.getDeviceId());
    if (fwDetails == null) {
      String errorMsg = "TR069 device request has been aborted,due to device not identified";
      return getOperationAbortedResponse(errorMsg);
    }

    if (fwDetails.getDownLoadStatus() != FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus()
        && fwDetails.getDownLoadStatus() != FirwareUpgradeStatus.DOWNLOAD_COMPLETED.getStatus()) {

      LOG.debug("persisting the fw details {}", fwDetails);

      DeviceRPCResponse opResult;
      opResult = syncHandler.performDeviceOperation(deviceRPCRequest);
      if (null == opResult) {
        return getTimeOutResponse();
      }
      fwDetails.setFileName(fileName);
      fwDetails.setDownLoadStatus(FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus());
      fwDetails.setOrigin("csem");
      deviceOperDAO.save(fwDetails);
      ArrayList<ParameterDTO> responseParamDTOList = new ArrayList<>();

      if (opResult.getOperationResponse().getStatus() == 1) {
        responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, "STARTED"));
      } else {
        responseParamDTOList.add(new ParameterDTO(RPC_REPLY_STATUS, FAILED));
        responseParamDTOList.add(new ParameterDTO(RPC_REPLY_ERROR, opResult.getFaultString()));
      }
      responseParamDTOList.add(new ParameterDTO("rpc-reply.ns1:notification-timeout", "1200"));

      opResult.getOperationResponse().setParameterDTOs(responseParamDTOList);
      response = mapperUtil.getNetconfResponse(opResult, request.getSwVersion(),
          request.getHwVersion(), true);

      LOG.debug("update the status for fw details {} ", fwDetails);
    } else {
      LOG.debug("FirmWare Upgrade is in progress");
      String errorMsg = "TR069 device request has been aborted as Firmware Upgrade is inProgress";
      return getOperationAbortedResponse(errorMsg);
    }

    return response;
  }

  protected DeviceRPCResponse mergeGetConfigDeviceRPCResponse(DeviceRPCResponse opResultVes,
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

  protected DeviceRPCResponse mergeSetConfigDeviceRPCResponse(DeviceRPCResponse opResultVes,
      DeviceRPCResponse opResultDevice) {
    if (null == opResultVes) {
      return opResultDevice;
    }

    if (null == opResultDevice) {
      return opResultVes;
    }

    return opResultDevice;
  }

  protected boolean isVesNotificationRequest(ParameterDTO param) {
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

  protected void handleBooleanParameters(List<ParameterDTO> parameterDTOs, String swVersion,
      String hwVersion) {

    for (ParameterDTO param : parameterDTOs) {
      MOMetaData metaData =
          metaDataUtil.getMetaDataByTR69Name(param.getParamName(), swVersion, hwVersion);
      if (null != metaData && BOOLEAN_DATA_TYPE.equalsIgnoreCase(metaData.getDataType())) {
        if (BOOLEAN_TRUE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(Boolean.TRUE.toString());
        } else if (BOOLEAN_FALSE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(Boolean.FALSE.toString());
        }
      }
    }
  }

  protected void handleBooleanParametersReverse(List<ParameterDTO> parameterDTOs, String swVersion,
      String hwVersion) {

    for (ParameterDTO param : parameterDTOs) {
      MOMetaData metaData =
          metaDataUtil.getMetaDataByTR69Name(param.getParamName(), swVersion, hwVersion);
      if (null != metaData && BOOLEAN_DATA_TYPE.equalsIgnoreCase(metaData.getDataType())) {
        if (Boolean.TRUE.toString().equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(BOOLEAN_TRUE_VALUE);
        } else if (Boolean.FALSE.toString().equalsIgnoreCase(param.getParamValue().trim())) {
          param.setParamValue(BOOLEAN_FALSE_VALUE);
        }
      }
    }
  }

  protected NetConfResponse getEmptyResponse() {
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

  protected NetConfResponse getTimeOutResponse() {
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

  protected NetConfResponse getErrorResponse(String errCode, String errorMsg) {
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

  protected NetConfResponse getOperationAbortedResponse(String errorMessage) {
    // prepare timeout error response
    NetConfResponse timeOutErrorResponse = new NetConfResponse();
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    ErrorCodeDetails errorCodeMetaData = errorCodeUtil.getErrorCodeMetaData("8006");
    if (errorCodeMetaData != null) {
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

  public List<ParameterDTO> filteredSetParameters(List<ParameterDTO> parameters, String swVersion,
      String hwVersion) {
    List<ParameterDTO> result = new ArrayList<>();
    for (ParameterDTO param : parameters) {
      MOMetaData metaData =
          metaDataUtil.getMetaDataByNetConfName(param.getParamName(), swVersion, hwVersion);
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

  protected List<ParameterDTO> filteredGetParameters(List<ParameterDTO> parameters,
      String swVersion, String hwVersion) {
    return metaDataUtil.getSupportedChildParameters(parameters, swVersion, hwVersion);
  }

  @Override
  public boolean handelRegisterEvent(NetConfServerDetails request) {
    LOG.debug("processing the handelRegisterEvent started");
    try {
      VESNotificationResponse vesRsponse = vesnotiSender.sendNotification(null, request);
      if (HttpStatus.valueOf(vesRsponse.getStatusCode()).is2xxSuccessful()) {
        LOG.debug("processing the handelRegisterEvent completed");
        return true;
      } else {
        LOG.error("processing the handelRegisterEvent error code recevived: {}",
            vesRsponse.getStatusCode());
        return false;
      }
    } catch (Exception e) {
      LOG.error("processing the handelRegisterEvent exception occurred");
      return false;
    }
  }

  protected static String getDownloadFileURI(String filepath) {

    if (filepath.contains("@") && filepath.contains("//")) {
      String[] str = filepath.split("@");
      String[] strForUserName = str[0].split("//");
      if (str.length > 1) {
        return strForUserName[0] + "//" + str[1];
      }
    }
    return null;
  }

  protected static String getDownloadUserName(String filepath) {

    if (filepath.contains("@") && filepath.contains("//")) {
      String[] str = filepath.split("@");
      String[] strForUserName = str[0].split("//");
      if (strForUserName.length > 1)
        return strForUserName[1];
    }
    return null;
  }

  protected boolean isAdminStateOverriden(List<ParameterDTO> paramList) {
    for (ParameterDTO paramDTO : paramList) {
      if (paramDTO.getParamName().contains(MapperConstants.ADMIN_STATE)
          || paramDTO.getParamName().contains(MapperConstants.ADMIN_STATUS)) {
        return true;
      }
    }
    return false;
  }

  protected void convertResposeToSPVResponse(DeviceRPCResponse deviceRPCResponse) {
    if (null == deviceRPCResponse) {
      return;
    }

    OperationResponse operationResponse = new SetParameterValueResponse();
    operationResponse.setParameterDTOs(new ArrayList<>());

    if (null == deviceRPCResponse.getFaultKey()) {
      operationResponse.setStatus(MapperConstants.RPC_SUCCESS_CODE);
    } else {
      operationResponse.setStatus(MapperConstants.RPC_FAILED_CODE);
    }

    deviceRPCResponse.setOperationResponse(operationResponse);
  }
}
