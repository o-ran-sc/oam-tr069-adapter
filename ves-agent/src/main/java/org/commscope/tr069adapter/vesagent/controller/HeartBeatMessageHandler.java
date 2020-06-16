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

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.http.HttpRequestSender;
import org.commscope.tr069adapter.vesagent.model.CommonEventHeader;
import org.commscope.tr069adapter.vesagent.model.Event;
import org.commscope.tr069adapter.vesagent.model.EventMessage;
import org.commscope.tr069adapter.vesagent.model.HeartbeatFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HeartBeatMessageHandler {

  private static final Logger logger = LoggerFactory.getLogger(HeartBeatMessageHandler.class);

  @Autowired
  VesConfiguration config;

  @Autowired
  HttpRequestSender sender;

  public VESNotificationResponse handlePINotification(VESNotification vesNoti)
      throws JsonProcessingException {
    Event event =
        convertNotificationToVESEvent(vesNoti.getDevnotification(), vesNoti.geteNodeBName());

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

  Event convertNotificationToVESEvent(DeviceInform notification, String eNodeBName) {
    Event hbEvent = new Event();
    CommonEventHeader eventHeader = new CommonEventHeader();

    eventHeader.setDomain("heartbeat");
    eventHeader.setEventId(
        "Heartbeat_" + notification.getDeviceDetails().getDeviceId() + System.currentTimeMillis());
    eventHeader.setEventName("Heartbeat_" + notification.getDeviceDetails().getProductClass() + "-"
        + config.getVendorName());
    eventHeader.setEventType("CommScope_RAN_Vnf");
    eventHeader.setLastEpochMicrosec(System.currentTimeMillis());

    eventHeader.setPriority("Normal");
    eventHeader.setSequence(0);

    if (eNodeBName == null) {
      eventHeader.setReportingEntityName(notification.getDeviceDetails().getDeviceId());
      eventHeader.setReportingEntityId(notification.getDeviceDetails().getDeviceId());
      eventHeader.setSourceId(notification.getDeviceDetails().getDeviceId());
      eventHeader.setSourceName(notification.getDeviceDetails().getDeviceId());
    } else {
      eventHeader.setReportingEntityName(eNodeBName);
      eventHeader.setSourceName(eNodeBName);
      eventHeader.setReportingEntityId(notification.getDeviceDetails().getDeviceId());
      eventHeader.setSourceId(notification.getDeviceDetails().getDeviceId());
    }

    eventHeader.setStartEpochMicrosec(System.currentTimeMillis());
    eventHeader.setVersion(config.getEventVersion());
    eventHeader.setNfNamingCode("");
    eventHeader.setNfcNamingCode("");
    eventHeader.setNfVendorName(config.getVendorName());
    eventHeader.setVesEventListenerVersion(config.getVesVersion());
    hbEvent.setCommonEventHeader(eventHeader);

    HeartbeatFields heartbeatFields = new HeartbeatFields();
    heartbeatFields.setHeartbeatFieldsVersion("3.0");
    heartbeatFields.setHeartbeatInterval(60);
    hbEvent.setHeartbeatFields(heartbeatFields);

    return hbEvent;
  }
}
