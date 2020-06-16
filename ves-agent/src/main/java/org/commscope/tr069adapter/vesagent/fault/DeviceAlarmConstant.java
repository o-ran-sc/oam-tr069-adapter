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

public class DeviceAlarmConstant {

  private DeviceAlarmConstant() {}

  public static final String DEVICE_ALARM_PURGE_JOB_NAME = "deviceAlarmPurgeJob";
  public static final String DEVICE_ALARM_PURGE_JOB_SUB_SYSTEM = "np";

  public static final String EXPEDITED_EVENT_EVENTTIME = "EventTime";
  public static final String EXPEDITED_EVENT_ALARMIDENTIFIER = "AlarmIdentifier";
  public static final String EXPEDITED_EVENT_NOTIFICATIONTYPE = "NotificationType";
  public static final String EXPEDITED_EVENT_MANAGEDOBJECTINSTANCE = "ManagedObjectInstance";
  public static final String EXPEDITED_EVENT_OBJECTINSTANCE = "ObjectInstance";
  public static final String EXPEDITED_EVENT_EVENTTYPE = "EventType";
  public static final String EXPEDITED_EVENT_FAULTCODE = "FaultCode";
  public static final String EXPEDITED_EVENT_PROBABLECAUSE = "ProbableCause";
  public static final String EXPEDITED_EVENT_SPECIFICPROBLEM = "SpecificProblem";
  public static final String EXPEDITED_EVENT_ALARMMESSAGE = "AlarmMessage";
  public static final String EXPEDITED_EVENT_PERCEIVEDSEVERITY = "PerceivedSeverity";
  public static final String EXPEDITED_EVENT_ADDITIONALTEXT = "AdditionalText";
  public static final String EXPEDITED_EVENT_ADDITIONALINFORMATION = "AdditionalInformation";
  public static final String EXPEDITED_EVENT_FAULTCOMPONENT = "FaultComponent";

}
