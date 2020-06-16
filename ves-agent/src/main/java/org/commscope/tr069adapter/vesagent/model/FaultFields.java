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

package org.commscope.tr069adapter.vesagent.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;
import java.util.Map;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class FaultFields implements Serializable {
  @JsonProperty("faultFieldsVersion")
  private String faultFieldsVersion;

  @JsonProperty("alarmCondition")
  private String alarmCondition;
  @JsonProperty("eventSourceType")
  private String eventSourceType;
  @JsonProperty("specificProblem")
  private String specificProblem;
  @JsonProperty("eventSeverity")
  private String eventSeverity;
  @JsonProperty("vfStatus")
  private String vfStatus;
  @JsonProperty("alarmInterfaceA")
  private String alarmInterfaceA;
  @JsonProperty("eventCategory")
  private String eventCategory;

  @JsonProperty("alarmAdditionalInformation")
  private Map<String, String> alarmAdditionalInformation = null;

  @JsonProperty("alarmCondition")
  public String getAlarmCondition() {
    return alarmCondition;
  }

  @JsonProperty("alarmCondition")
  public void setAlarmCondition(String alarmCondition) {
    this.alarmCondition = alarmCondition;
  }

  @JsonProperty("eventSeverity")
  public String getEventSeverity() {
    return eventSeverity;
  }

  @JsonProperty("eventSeverity")
  public void setEventSeverity(String eventSeverity) {
    this.eventSeverity = eventSeverity;
  }

  @JsonProperty("eventSourceType")
  public String getEventSourceType() {
    return eventSourceType;
  }

  @JsonProperty("eventSourceType")
  public void setEventSourceType(String eventSourceType) {
    this.eventSourceType = eventSourceType;
  }

  @JsonProperty("faultFieldsVersion")
  public String getFaultFieldsVersion() {
    return faultFieldsVersion;
  }

  @JsonProperty("faultFieldsVersion")
  public void setFaultFieldsVersion(String faultFieldsVersion) {
    this.faultFieldsVersion = faultFieldsVersion;
  }

  @JsonProperty("specificProblem")
  public String getSpecificProblem() {
    return specificProblem;
  }

  @JsonProperty("specificProblem")
  public void setSpecificProblem(String specificProblem) {
    this.specificProblem = specificProblem;
  }

  @JsonProperty("vfStatus")
  public String getVfStatus() {
    return vfStatus;
  }

  @JsonProperty("vfStatus")
  public void setVfStatus(String vfStatus) {
    this.vfStatus = vfStatus;
  }

  @JsonProperty("alarmAdditionalInformation")
  public Map<String, String> getAlarmAdditionalInformation() {
    return alarmAdditionalInformation;
  }

  @JsonProperty("alarmAdditionalInformation")
  public void setAlarmAdditionalInformation(Map<String, String> alarmAdditionalInformation) {
    this.alarmAdditionalInformation = alarmAdditionalInformation;
  }

  public String getAlarmInterfaceA() {
    return alarmInterfaceA;
  }

  public void setAlarmInterfaceA(String alarmInterfaceA) {
    this.alarmInterfaceA = alarmInterfaceA;
  }

  public String getEventCategory() {
    return eventCategory;
  }

  public void setEventCategory(String eventCategory) {
    this.eventCategory = eventCategory;
  }
}
