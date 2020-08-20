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
package org.commscope.tr069adapter.vesagent.controller;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.ArrayList;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.entity.DeviceDataEntity;
import org.commscope.tr069adapter.vesagent.exception.VesAgentException;
import org.commscope.tr069adapter.vesagent.http.HttpRequestSender;
import org.commscope.tr069adapter.vesagent.model.CommonEventHeader;
import org.commscope.tr069adapter.vesagent.model.Event;
import org.commscope.tr069adapter.vesagent.model.EventMessage;
import org.commscope.tr069adapter.vesagent.model.HeartbeatFields;
import org.commscope.tr069adapter.vesagent.service.VesAgentServiceHelper;
import org.commscope.tr069adapter.vesagent.util.VesAgentConstants;
import org.commscope.tr069adapter.vesagent.util.VesAgentUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HeartBeatMessageHandler {

  private static final Logger logger = LoggerFactory.getLogger(HeartBeatMessageHandler.class);

  @Autowired
  VesAgentServiceHelper vesAgentServiceHelper;

  @Autowired
  VesConfiguration config;

  @Autowired
  HttpRequestSender sender;

  public VESNotificationResponse sendHeartBeatEvent(DeviceDataEntity deviceDataEntity,
      int heartBeatPeriod) throws Exception {
    Event event = convertNotificationToVESEvent(deviceDataEntity, heartBeatPeriod);

    EventMessage evMsg = new EventMessage();
    evMsg.setEvent(event);

    ObjectMapper mapper = new ObjectMapper();

    String requestBody = mapper.writeValueAsString(evMsg);
    if (requestBody.isEmpty()) {
      logger.debug("VES Event body is empty");
    }

    String url = config.getPnfRegVesUrl();
    return sender.postRequest(url, requestBody);
  }

  public void handleRegisterRequest(VESNotification vesNotification) {
    try {
      VesAgentUtils.validateVESNotification(vesNotification);
    } catch (VesAgentException e) {
      logger.error(
          "Failed to create heartbeat timers for device on recieving bootstrap. ErrorMsg: {}",
          e.getMessage());
      return;
    }

    List<DeviceDataEntity> deviceDataEntityList = vesAgentServiceHelper.findByDeviceIdAndGroup(
        vesNotification.getDevnotification().getDeviceDetails().getDeviceId(),
        VesAgentConstants.HEART_BEAT);

    if (Boolean.FALSE.equals(VesAgentUtils.isNullOrEmpty(deviceDataEntityList))) {
      DeviceDataEntity deviceDataEntity = deviceDataEntityList.get(0);
      String heartbeatPeriod =
          deviceDataEntity.getAttributesMap().get(VesAgentConstants.HEART_BEAT_PERIOD);

      if (!VesAgentUtils.isNullOrEmpty(heartbeatPeriod)
          && !heartbeatPeriod.equalsIgnoreCase(VesAgentConstants.REMOVE_HEART_BEAT_TIMER_VAL)) {
        return;
      }
    }

    logger.info("Creating heartbeat records and timer using default heartbeatPeriod {} minutes.",
        VesAgentConstants.HEART_BEAT_PERIOD_DEFAULT_VAL);

    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    deviceRPCRequest.setDeviceDetails(vesNotification.getDevnotification().getDeviceDetails());

    OperationDetails opDetails = new OperationDetails();

    ParameterDTO heartBeatPeriodParam = new ParameterDTO();
    heartBeatPeriodParam.setParamName(VesAgentConstants.HEART_BEAT_PERIOD);
    heartBeatPeriodParam.setParamValue(VesAgentConstants.HEART_BEAT_PERIOD_DEFAULT_VAL);

    List<ParameterDTO> paramList = new ArrayList<>();
    paramList.add(heartBeatPeriodParam);

    opDetails.setParmeters(paramList);
    deviceRPCRequest.setOpDetails(opDetails);

    deviceRPCRequest.addContextParam(VesAgentConstants.ENODEB_NAME,
        vesNotification.geteNodeBName());

    handleSetConfigRequest(deviceRPCRequest);

  }

  public DeviceRPCResponse handleSetConfigRequest(DeviceRPCRequest deviceRPCRequest) {
    try {
      VesAgentUtils.validateDeviceRPCRequest(deviceRPCRequest);
    } catch (VesAgentException e) {
      return VesAgentUtils.getErrorResponse(deviceRPCRequest, e.getErrorCode(), e.getMessage());
    }

    OperationDetails operationDetails = deviceRPCRequest.getOpDetails();

    String heartBeatPeriod = null;
    String countDownTimer = null;

    ParameterDTO countDownTimerDTO = null;

    List<ParameterDTO> paramDTOList = operationDetails.getParmeters();
    for (ParameterDTO paramDTO : paramDTOList) {
      if (paramDTO.getParamName().equalsIgnoreCase(VesAgentConstants.HEART_BEAT_PERIOD)) {
        heartBeatPeriod = paramDTO.getParamValue();
      }

      if (paramDTO.getParamName().equalsIgnoreCase(VesAgentConstants.COUNT_DOWN_TIMER)) {
        countDownTimer = paramDTO.getParamValue();
        countDownTimerDTO = paramDTO;
      }
    }

    try {
      vesAgentServiceHelper.processHeartBeatSetRequest(deviceRPCRequest, heartBeatPeriod,
          countDownTimer);
    } catch (VesAgentException e) {
      return VesAgentUtils.getErrorResponse(deviceRPCRequest, e.getErrorCode(), e.getMessage());
    }

    copyHeartBeatPeriodToTimerForResponse(countDownTimerDTO, heartBeatPeriod);
    return VesAgentUtils.getSuccessResponse(deviceRPCRequest);
  }

  private void copyHeartBeatPeriodToTimerForResponse(ParameterDTO countDownParam,
      String heartBeatPeriod) {
    if (null != countDownParam && !VesAgentUtils.isNullOrEmpty(heartBeatPeriod)) {
      countDownParam.setParamValue(heartBeatPeriod);
    }
  }

  public DeviceRPCResponse handleGetConfigRequest(DeviceRPCRequest deviceRPCRequest) {
    try {
      VesAgentUtils.validateDeviceRPCRequest(deviceRPCRequest);
    } catch (VesAgentException e) {
      return VesAgentUtils.getErrorResponse(deviceRPCRequest, e.getErrorCode(), e.getMessage());
    }

    vesAgentServiceHelper.processHeartBeatGetRequest(deviceRPCRequest);
    return VesAgentUtils.getSuccessResponse(deviceRPCRequest);
  }

  public VESNotificationResponse handleDeleteConfigRequest(VESNotification vesNotification) {
    try {
      VesAgentUtils.validateDelVESNotification(vesNotification);
      vesAgentServiceHelper.processHeartBeatDeleteRequest(vesNotification);
    } catch (VesAgentException e) {
      return new VESNotificationResponse(Integer.parseInt(e.getErrorCode()), e.getMessage());
    } catch (Exception e) {
      return new VESNotificationResponse(VesAgentConstants.RPC_FAILED, e.getMessage());
    }

    return new VESNotificationResponse(VesAgentConstants.RPC_SUCCESS, "success");
  }

  Event convertNotificationToVESEvent(DeviceDataEntity deviceDataEntity, int heartBeatPeriod) {
    Event hbEvent = new Event();
    CommonEventHeader eventHeader = new CommonEventHeader();

    eventHeader.setDomain("heartbeat");
    eventHeader
        .setEventId("heartbeat" + deviceDataEntity.getDeviceId() + System.currentTimeMillis());
    eventHeader.setEventName(
        "heartbeat_" + deviceDataEntity.getProductClass() + "-" + config.getVendorName());
    eventHeader.setEventType("CommScope_RAN_heartbeat");
    eventHeader.setLastEpochMicrosec(System.currentTimeMillis() * 1000);

    eventHeader.setPriority("High");
    eventHeader.setSequence(0);

    if (deviceDataEntity.geteNodeBName() == null) {
      eventHeader.setReportingEntityName(deviceDataEntity.getDeviceId());
      eventHeader.setReportingEntityId(deviceDataEntity.getDeviceId());
      eventHeader.setSourceId(deviceDataEntity.getDeviceId());
      eventHeader.setSourceName(deviceDataEntity.getDeviceId());
    } else {
      eventHeader.setReportingEntityName(deviceDataEntity.geteNodeBName());
      eventHeader.setSourceName(deviceDataEntity.geteNodeBName());
      eventHeader.setReportingEntityId(deviceDataEntity.getDeviceId());
      eventHeader.setSourceId(deviceDataEntity.getDeviceId());
    }

    eventHeader.setStartEpochMicrosec(deviceDataEntity.getStartEpochMicrosec());
    eventHeader.setVersion(config.getEventVersion());
    eventHeader.setNfNamingCode("");
    eventHeader.setNfcNamingCode("");
    eventHeader.setNfVendorName(config.getVendorName());
    eventHeader.setVesEventListenerVersion(config.getVesVersion());
    hbEvent.setCommonEventHeader(eventHeader);

    HeartbeatFields heartbeatFields = new HeartbeatFields();
    heartbeatFields.setHeartbeatFieldsVersion("3.0");
    heartbeatFields.setHeartbeatInterval(heartBeatPeriod * 60);
    hbEvent.setHeartbeatFields(heartbeatFields);

    return hbEvent;
  }
}
