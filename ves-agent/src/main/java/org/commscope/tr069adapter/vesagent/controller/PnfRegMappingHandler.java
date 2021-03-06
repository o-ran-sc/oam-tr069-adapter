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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.fault.Parser;
import org.commscope.tr069adapter.vesagent.http.HttpRequestSender;
import org.commscope.tr069adapter.vesagent.model.CommonEventHeader;
import org.commscope.tr069adapter.vesagent.model.Event;
import org.commscope.tr069adapter.vesagent.model.EventMessage;
import org.commscope.tr069adapter.vesagent.model.PnfRegEventAdditionalFeilds;
import org.commscope.tr069adapter.vesagent.model.PnfRegEventFields;
import org.commscope.tr069adapter.vesagent.util.EventUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PnfRegMappingHandler {
  private static final Logger logger = LoggerFactory.getLogger(PnfRegMappingHandler.class);
  public static final String SSH_USERNAME = "netconf";
  public static final String SSH_PSSWORD = "netconf";

  @Autowired
  VesConfiguration config;

  @Autowired
  HttpRequestSender sender;

  public VESNotificationResponse handlePnfRegNotification(VESNotification vesNoti)
      throws JsonProcessingException {
    Event event = convertNotificationToVESEvent(vesNoti.getDevnotification(),
        vesNoti.getNetconfDetails(), vesNoti.geteNodeBName(), vesNoti.getNetconfParameters());
    if (null != event) {
      EventMessage evMsg = new EventMessage();
      evMsg.setEvent(event);

      ObjectMapper mapper = new ObjectMapper();

      String requestBody = mapper.writeValueAsString(evMsg);
      if (requestBody.isEmpty()) {
        logger.debug("VES Event body is empty");
      }
      logger.debug("VES message for PnfRegistration : {}", requestBody);
      String url = config.getPnfRegVesUrl();
      return sender.postRequest(url, requestBody);
    } else {
      return new VESNotificationResponse(-1,
          "unable to prepare ves event due to insufficient data");
    }
  }

  public VESNotificationResponse handlePnfRegNotificationOnRestart(VESNotification vesNoti)
      throws JsonProcessingException {
    Event event = convertNotificationToVESEventOnRestart(vesNoti.getNetconfDetails());

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

  Event convertNotificationToVESEvent(DeviceInform notification,
      NetConfServerDetails netconfServerDetails, String eNodeBName,
      List<ParameterDTO> netconfParameters) {
    logger.debug("Converting Notification to VES pnfevent started");
    Parser parser = new Parser();


    Event regEvent = new Event();
    CommonEventHeader eventHeader = new CommonEventHeader();

    eventHeader.setDomain("pnfRegistration");
    eventHeader.setEventId(
        "PnfReg" + notification.getDeviceDetails().getDeviceId() + System.currentTimeMillis());
    eventHeader.setEventName("pnfReg_" + notification.getDeviceDetails().getProductClass() + "-"
        + config.getVendorName());
    eventHeader.setEventType(config.getPnfRegEventType());
    eventHeader.setPriority("High");
    EventUtil.populateEventHeaderFields(eventHeader, notification, eNodeBName, config);
    regEvent.setCommonEventHeader(eventHeader);

    PnfRegEventFields pnfRegistrationFields =
        parser.parseNotificationParams(notification.getParameters());
    populatePnfRegFeilds(netconfServerDetails, pnfRegistrationFields, netconfParameters);
    pnfRegistrationFields.setModelNumber(notification.getDeviceDetails().getProductClass());

    regEvent.setPnfRegistrationFields(pnfRegistrationFields);
    logger.debug("Converting Notification to VES pnfevent completed");
    return regEvent;
  }

  Event convertNotificationToVESEventOnRestart(NetConfServerDetails netconfServerDetails) {
    logger.debug("Converting Notification to VES pnfevent started");

    Event regEvent = new Event();
    CommonEventHeader eventHeader = new CommonEventHeader();

    if (null == netconfServerDetails) {
      logger.error("netconf server details as received as null");
      return null;
    }
    eventHeader.setDomain("pnfRegistration");
    eventHeader
        .setEventId("PnfReg" + netconfServerDetails.getDeviceId() + System.currentTimeMillis());
    eventHeader.setEventName(
        "pnfReg_" + netconfServerDetails.getDeviceId() + "-" + config.getVendorName());
    eventHeader.setEventType(config.getPnfRegEventType());
    eventHeader.setLastEpochMicrosec(System.currentTimeMillis());

    eventHeader.setPriority("High");
    eventHeader.setSequence(0);

    if (netconfServerDetails.getEnodeBName() == null) {
      eventHeader.setReportingEntityName(netconfServerDetails.getDeviceId());
      eventHeader.setReportingEntityId(netconfServerDetails.getDeviceId());
      eventHeader.setSourceId(netconfServerDetails.getDeviceId());
      eventHeader.setSourceName(netconfServerDetails.getDeviceId());
    } else {
      eventHeader.setReportingEntityName(netconfServerDetails.getEnodeBName());
      eventHeader.setSourceName(netconfServerDetails.getEnodeBName());

      eventHeader.setReportingEntityId(netconfServerDetails.getDeviceId());
      eventHeader.setSourceId(netconfServerDetails.getDeviceId());
    }

    eventHeader.setStartEpochMicrosec(System.currentTimeMillis() * 1000);
    eventHeader.setVersion(config.getEventVersion());
    eventHeader.setNfNamingCode("");
    eventHeader.setNfcNamingCode("");
    eventHeader.setNfVendorName(config.getVendorName());
    eventHeader.setVesEventListenerVersion(config.getVesVersion());
    regEvent.setCommonEventHeader(eventHeader);
    PnfRegEventFields pnfRegistrationFields = new PnfRegEventFields();
    regEvent.setPnfRegistrationFields(
        populatePnfRegFeilds(netconfServerDetails, pnfRegistrationFields, new ArrayList<>()));
    logger.debug("Converting Notification to VES pnfevent completed");
    return regEvent;
  }

  private PnfRegEventFields populatePnfRegFeilds(NetConfServerDetails netconfServerDetails,
      PnfRegEventFields pnfRegistrationFields, List<ParameterDTO> netconfParameters) {

    pnfRegistrationFields.setSerialNumber(netconfServerDetails.getDeviceId());
    pnfRegistrationFields.setPnfRegistrationFieldsVersion(config.getPnfFeildVersion());
    pnfRegistrationFields.setMacAddress(netconfServerDetails.getDeviceId());
    pnfRegistrationFields.setVendorName(config.getVendorName());
    pnfRegistrationFields.setSoftwareVersion(netconfServerDetails.getSwVersion());

    pnfRegistrationFields.setOamV4IpAddress(netconfServerDetails.getListenAddress());
    // TODO: since not supporting 1pv6 we are configuring empty value
    pnfRegistrationFields.setOamV6IpAddress("");

    pnfRegistrationFields.setManufactureDate("");
    pnfRegistrationFields.setUnitType(config.getUnitType());
    pnfRegistrationFields.setUnitFamily(config.getUnitFamily());
    pnfRegistrationFields.setLastServiceDate(new SimpleDateFormat("ddMMyyyy").format(new Date()));

    // Add additional attributes and device parameters to the VES message in additionalFields
    HashMap<String, String> additionalFieldsMap = new HashMap<>();
    PnfRegEventAdditionalFeilds additionalFields = new PnfRegEventAdditionalFeilds();
    additionalFields.setOamPort(netconfServerDetails.getListenPort());
    additionalFields.setProtocol("SSH");
    additionalFields.setUsername(SSH_USERNAME);
    additionalFields.setPassword(SSH_PSSWORD);
    additionalFields.setReconnectOnChangedSchema(Boolean.FALSE.toString());
    additionalFields.setSleepfactor("1.5");
    additionalFields.setTcpOnly(Boolean.FALSE.toString());
    additionalFields.setConnectionTimeout("20000");
    additionalFields.setMaxConnectionAttempts("100");
    additionalFields.setBetweenAttemptsTimeout("2000");
    additionalFields.setKeepaliveDelay("120");

    ObjectMapper mapper = new ObjectMapper();
    String additionalFieldsString;
    try {
      additionalFieldsString = mapper.writeValueAsString(additionalFields);
      HashMap<String, String> pnfAdditionalFieldsMap = new HashMap<>();
      pnfAdditionalFieldsMap =
          mapper.readValue(additionalFieldsString, pnfAdditionalFieldsMap.getClass());
      additionalFieldsMap.putAll(pnfAdditionalFieldsMap);
    } catch (JsonProcessingException e) {
      logger.error("Failed to add pnf Reg Additional fields {}", e.getMessage());
    }

    if (null != netconfParameters) {
      for (ParameterDTO param : netconfParameters) {
        additionalFieldsMap.put(param.getParamName(), param.getParamValue());
      }
    }

    pnfRegistrationFields.setAdditionalFields(additionalFieldsMap);
    return pnfRegistrationFields;

  }

}
