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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.exception.InvalidFaultOperationException;
import org.commscope.tr069adapter.vesagent.fault.AlarmFrameWorkSeverityEnum;
import org.commscope.tr069adapter.vesagent.fault.ExpeditedEvent;
import org.commscope.tr069adapter.vesagent.fault.Parser;
import org.commscope.tr069adapter.vesagent.http.HttpRequestSender;
import org.commscope.tr069adapter.vesagent.model.CommonEventHeader;
import org.commscope.tr069adapter.vesagent.model.Event;
import org.commscope.tr069adapter.vesagent.model.EventMessage;
import org.commscope.tr069adapter.vesagent.model.FaultFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

@Component
public class AlarmMappingHandler {

  private static final Logger logger = LoggerFactory.getLogger(AlarmMappingHandler.class);

  @Autowired
  VesConfiguration config;

  @Autowired
  HttpRequestSender sender;

  public VESNotificationResponse handleAlarmNotification(VESNotification notification)
      throws InvalidFaultOperationException, JsonProcessingException {
    List<Event> events = convertNotificationToVESEvent(notification.getDevnotification(),
        notification.geteNodeBName());

    for (Iterator<Event> iterator = events.iterator(); iterator.hasNext();) {
      Event event = iterator.next();
      EventMessage evMsg = new EventMessage();
      evMsg.setEvent(event);

      ObjectMapper mapper = new ObjectMapper();

      String requestBody = mapper.writeValueAsString(evMsg);
      if (requestBody.isEmpty()) {
        logger.debug("Alarm Event body is empty");
      }

      String url = config.getFaultVesUrl();
      VESNotificationResponse response = sender.postRequest(url, requestBody);
      if (response.getStatusCode() == HttpStatus.INTERNAL_SERVER_ERROR.value()) {
        logger.debug(
            "Error received while posting alarms; skiiping this alarm in the fault event and continue for remaining");
      }
    }

    return new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "No Alarms in the request");
  }

  List<Event> convertNotificationToVESEvent(DeviceInform notification, String eNodeBName)
      throws InvalidFaultOperationException {
    logger.debug("Converting Notification to VES fault event started");
    ArrayList<Event> mlist = new ArrayList<>();

    List<ParameterDTO> parameter = notification.getParameters();
    Map<String, ExpeditedEvent> hmAlarmParameters = null;
    Parser parser = new Parser();
    hmAlarmParameters = parser.parseFaultParams(parameter);

    for (Iterator<String> iterator = hmAlarmParameters.keySet().iterator(); iterator.hasNext();) {
      ExpeditedEvent event = hmAlarmParameters.get(iterator.next());

      if (event.getAdditionalInformation() != null
          && event.getAdditionalInformation().contains("-")) {
        String eNBName = event.getAdditionalInformation().split("-")[0].trim();
        eNodeBName = eNBName;
      }

      Event faultEvent = new Event();
      CommonEventHeader eventHeader = new CommonEventHeader();
      eventHeader.setDomain("fault");
      eventHeader.setEventId(event.getAlarmIdentifier());
      eventHeader.setEventName("Fault_" + notification.getDeviceDetails().getProductClass() + "-"
          + config.getVendorName() + "_" + event.getProbableCause().replace(" ", ""));
      eventHeader.setEventType(config.getFaultEventType());
      eventHeader.setLastEpochMicrosec(System.currentTimeMillis());
      eventHeader.setTimeZoneOffset(extractTimeZoneOffSet(event.getAdditionalInformation()));
      eventHeader.setPriority(extractPriority(event.getPerceivedSeverity()));

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

      eventHeader.setSequence(1);
      eventHeader.setStartEpochMicrosec(System.currentTimeMillis());
      eventHeader.setVersion(config.getEventVersion());
      if (eNodeBName != null && eNodeBName.length() > 3)
        eventHeader.setNfNamingCode(eNodeBName.substring(0, 3));
      else
        eventHeader.setNfNamingCode("");
      eventHeader.setNfcNamingCode("");
      eventHeader.setNfVendorName(config.getVendorName());
      eventHeader.setVesEventListenerVersion(config.getVesVersion());
      faultEvent.setCommonEventHeader(eventHeader);

      FaultFields faultfields = new FaultFields();
      faultfields.setAlarmCondition(event.getProbableCause().replace(" ", ""));

      faultfields.setEventSeverity(event.getPerceivedSeverity().toString());
      if (event.getPerceivedSeverity() == AlarmFrameWorkSeverityEnum.CLEAR
          || event.getPerceivedSeverity() == AlarmFrameWorkSeverityEnum.CLEARED) {
        faultfields.setEventSeverity("NORMAL");
      }

      faultfields.setEventSourceType(config.getFaultEventSourcePrefix() + "_"
          + notification.getDeviceDetails().getProductClass());
      faultfields.setFaultFieldsVersion(config.getFaultFeildVersion());
      faultfields.setSpecificProblem(event.getSpecificProblem());
      faultfields.setVfStatus("Active");
      faultfields.setEventCategory(event.getEventType());
      faultfields.setAlarmInterfaceA(event.getManagedObjectInstance());
      Map<String, String> addition = new HashMap<>();
      addition.put("eventTime",
          new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.s'Z'").format(new Date()));
      addition.put("AdditionalText", event.getAdditionalText());
      addition.put("AdditionalInformation", event.getAdditionalInformation());
      faultfields.setAlarmAdditionalInformation(addition);

      faultEvent.setFaultFields(faultfields);
      mlist.add(faultEvent);

    }
    logger.debug("Converting Notification to VES fault event completed :{}", mlist);
    return mlist;
  }

  private String extractTimeZoneOffSet(String additionalInformation) {
    String[] additionalInfoArray = null;
    String[] timeZoneOffset = null;
    String timeOffset = "UTC+00.00";

    if (additionalInformation != null && additionalInformation.contains("|")) {
      additionalInfoArray = additionalInformation.split("\\|");

      int index = -1;
      for (int i = 0; i < additionalInfoArray.length; i++) {
        if (additionalInfoArray[i].contains("TZD=")) {
          index = i;
        }
      }
      if (index >= 0) {
        timeZoneOffset = additionalInfoArray[index].split("=");
        timeOffset = timeZoneOffset[timeZoneOffset.length - 1];
      }
    } else if (additionalInformation != null && additionalInformation.contains("TZD=")) {
      timeZoneOffset = additionalInformation.split("=");
      timeOffset = timeZoneOffset[timeZoneOffset.length - 1];
    }

    return timeOffset;
  }

  private String extractPriority(AlarmFrameWorkSeverityEnum severity) {
    String priority;
    if (severity == AlarmFrameWorkSeverityEnum.CRITICAL
        || severity == AlarmFrameWorkSeverityEnum.MAJOR) {
      priority = "High";
    } else if (severity == AlarmFrameWorkSeverityEnum.MINOR) {
      priority = "Medium";
    } else {
      priority = "Low";
    }

    return priority;
  }
}
