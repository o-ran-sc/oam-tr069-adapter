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

package org.commscope.tr069adapter.vesagent.fault;


import java.io.Serializable;
import java.util.Date;

import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.vesagent.exception.InvalidFaultOperationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExpeditedEvent implements Serializable {
  private static final long serialVersionUID = -6862753992876923284L;

  private Date eventTime;

  private String alarmIdentifier;

  private NotificationTypeEnum notificationType;

  private String managedObjectInstance;

  private String eventType;

  private String probableCause;

  private String specificProblem;

  private AlarmFrameWorkSeverityEnum perceivedSeverity;

  private String additionalText;

  private String additionalInformation;
  private static final Logger logger = LoggerFactory.getLogger(ExpeditedEvent.class);

  public Date getEventTime() {
    return eventTime;
  }

  public void setEventTime(Date eventTime) {
    this.eventTime = eventTime;
  }

  public String getAlarmIdentifier() {
    return alarmIdentifier;
  }

  public void setAlarmIdentifier(String alarmIdentifier) {
    this.alarmIdentifier = alarmIdentifier;
  }

  public NotificationTypeEnum getNotificationType() {
    return notificationType;
  }

  public void setNotificationType(NotificationTypeEnum notificationType) {
    this.notificationType = notificationType;
  }

  public String getManagedObjectInstance() {
    return managedObjectInstance;
  }

  public void setManagedObjectInstance(String managedObjectInstance) {
    this.managedObjectInstance = managedObjectInstance;
  }

  public String getEventType() {
    return eventType;
  }

  public void setEventType(String eventType) {
    this.eventType = eventType;
  }

  public String getProbableCause() {
    return probableCause;
  }

  public void setProbableCause(String probableCause) {
    this.probableCause = probableCause;
  }

  public String getSpecificProblem() {
    return specificProblem;
  }

  public void setSpecificProblem(String specificProblem) {
    this.specificProblem = specificProblem;
  }

  public AlarmFrameWorkSeverityEnum getPerceivedSeverity() {
    return perceivedSeverity;
  }

  public void setPerceivedSeverity(AlarmFrameWorkSeverityEnum perceivedSeverity) {
    this.perceivedSeverity = perceivedSeverity;
  }

  public String getAdditionalText() {
    return additionalText;
  }

  public void setAdditionalText(String additionalText) {
    this.additionalText = additionalText;
  }

  public String getAdditionalInformation() {
    return additionalInformation;
  }

  public void setAdditionalInformation(String additionalInformation) {
    this.additionalInformation = additionalInformation;
  }

  public void parse(ParameterDTO parameter, String tempEEParam)
      throws InvalidFaultOperationException {
    if (null == parameter.getParamValue()) {
      return;
    }

    if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_ALARMIDENTIFIER)) {
      setAlarmIdentifier(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_NOTIFICATIONTYPE)) {
      validateNotificationType(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_MANAGEDOBJECTINSTANCE)
        || tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_OBJECTINSTANCE)) {
      setManagedObjectInstance(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_EVENTTYPE)) {
      setEventType(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_FAULTCODE)) {
      setEventType(parameter.getParamValue());
      setAlarmIdentifier(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_PROBABLECAUSE)) {
      setProbableCause(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_SPECIFICPROBLEM)
        || tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_ALARMMESSAGE)) {
      setSpecificProblem(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_PERCEIVEDSEVERITY)
        && !parameter.getParamValue().equals("")) {
      validateSeverity(parameter.getParamValue());
      setPerceivedSeverity(
          AlarmFrameWorkSeverityEnum.valueOf(((String) parameter.getParamValue()).toUpperCase()));
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_ADDITIONALTEXT)
        || tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_FAULTCOMPONENT)) {
      setAdditionalText(parameter.getParamValue());
    } else if (tempEEParam.equals(DeviceAlarmConstant.EXPEDITED_EVENT_ADDITIONALINFORMATION)) {
      setAdditionalInformation(parameter.getParamValue());
    }
  }

  private void validateSeverity(String severity) throws InvalidFaultOperationException {
    try {
      AlarmFrameWorkSeverityEnum.valueOf(severity.toUpperCase());
      logger.debug("Severity is validated successfully : {}", severity);
    } catch (Exception e) {
      logger.error("Invalid Severity: Severity : {}", severity);
      throw new InvalidFaultOperationException(
          "Invalid Severity: Severity " + severity + " is not supported");
    }
  }

  private void validateNotificationType(String notificationType)
      throws InvalidFaultOperationException {
    try {
      AlarmNotificationTypeEnum.valueOf(notificationType.toUpperCase());
      logger.debug("NotificationType is {} validated successfully", notificationType);
    } catch (Exception e) {
      logger.error("Invalid device alarm notification type: NotificationType : {}",
          notificationType);
      throw new InvalidFaultOperationException(
          "Invalid NotificationType: notificationType <" + notificationType + "> is not supported");
    }
  }

}
