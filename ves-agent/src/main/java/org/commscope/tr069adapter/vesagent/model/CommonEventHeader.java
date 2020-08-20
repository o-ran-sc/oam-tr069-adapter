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

@JsonInclude(JsonInclude.Include.NON_NULL)
public class CommonEventHeader implements Serializable {
  @JsonProperty("version")
  private String version;
  @JsonProperty("vesEventListenerVersion")
  private String vesEventListenerVersion;
  @JsonProperty("domain")
  private String domain;
  @JsonProperty("eventName")
  private String eventName;
  @JsonProperty("eventType")
  private String eventType;
  @JsonProperty("eventId")
  private String eventId;
  @JsonProperty("sequence")
  private Integer sequence;
  @JsonProperty("priority")
  private String priority;
  @JsonProperty("reportingEntityId")
  private String reportingEntityId;
  @JsonProperty("reportingEntityName")
  private String reportingEntityName;
  @JsonProperty("sourceId")
  private String sourceId;
  @JsonProperty("sourceName")
  private String sourceName;
  @JsonProperty("nfVendorName")
  private String nfVendorName;
  @JsonProperty("nfNamingCode")
  private String nfNamingCode;
  @JsonProperty("nfcNamingCode")
  private String nfcNamingCode;
  @JsonProperty("startEpochMicrosec")
  private long startEpochMicrosec;
  @JsonProperty("lastEpochMicrosec")
  private long lastEpochMicrosec;
  @JsonProperty("timeZoneOffset")
  private String timeZoneOffset;

  public String getEventId() {
    return eventId;
  }

  public void setEventId(String eventId) {
    this.eventId = eventId;
  }

  public String getEventName() {
    return eventName;
  }

  public void setEventName(String eventName) {
    this.eventName = eventName;
  }

  public String getDomain() {
    return domain;
  }

  public void setDomain(String domain) {
    this.domain = domain;
  }

  public String getSourceName() {
    return sourceName;
  }

  public void setSourceName(String sourceName) {
    this.sourceName = sourceName;
  }

  public String getPriority() {
    return priority;
  }

  public void setPriority(String priority) {
    this.priority = priority;
  }

  public String getVersion() {
    return version;
  }

  public void setVersion(String version) {
    this.version = version;
  }

  public int getSequence() {
    return sequence;
  }

  public void setSequence(int sequence) {
    this.sequence = sequence;
  }

  public String getVesEventListenerVersion() {
    return vesEventListenerVersion;
  }

  public void setVesEventListenerVersion(String vesEventListenerVersion) {
    this.vesEventListenerVersion = vesEventListenerVersion;
  }

  public long getStartEpochMicrosec() {
    return startEpochMicrosec;
  }

  public void setStartEpochMicrosec(long startEpochMicrosec) {
    this.startEpochMicrosec = startEpochMicrosec;
  }

  public long getLastEpochMicrosec() {
    return lastEpochMicrosec;
  }

  public void setLastEpochMicrosec(long lastEpochMicrosec) {
    this.lastEpochMicrosec = lastEpochMicrosec;
  }

  public String getReportingEntityName() {
    return reportingEntityName;
  }

  public void setReportingEntityName(String reportingEntityName) {
    this.reportingEntityName = reportingEntityName;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public String getEventType() {
    return eventType;
  }

  public void setEventType(String eventType) {
    this.eventType = eventType;
  }

  public String getNfNamingCode() {
    return nfNamingCode;
  }

  public void setNfNamingCode(String nfNamingCode) {
    this.nfNamingCode = nfNamingCode;
  }

  public String getReportingEntityId() {
    return reportingEntityId;
  }

  public void setReportingEntityId(String reportingEntityId) {
    this.reportingEntityId = reportingEntityId;
  }

  public String getSourceId() {
    return sourceId;
  }

  public void setSourceId(String sourceId) {
    this.sourceId = sourceId;
  }

  public String getNfVendorName() {
    return nfVendorName;
  }

  public void setNfVendorName(String nfVendorName) {
    this.nfVendorName = nfVendorName;
  }

  public String getTimeZoneOffset() {
    return timeZoneOffset;
  }

  public void setTimeZoneOffset(String timeZoneOffset) {
    this.timeZoneOffset = timeZoneOffset;
  }

  public String getNfcNamingCode() {
    return nfcNamingCode;
  }

  public void setNfcNamingCode(String nfcNamingCode) {
    this.nfcNamingCode = nfcNamingCode;
  }

  @Override
  public String toString() {
    return "CommonEventHeader [version=" + version + ", vesEventListenerVersion="
        + vesEventListenerVersion + ", domain=" + domain + ", eventName=" + eventName
        + ", eventType=" + eventType + ", eventId=" + eventId + ", sequence=" + sequence
        + ", priority=" + priority + ", reportingEntityId=" + reportingEntityId
        + ", reportingEntityName=" + reportingEntityName + ", sourceId=" + sourceId
        + ", sourceName=" + sourceName + ", nfVendorName=" + nfVendorName + ", nfNamingCode="
        + nfNamingCode + ", nfcNamingCode=" + nfcNamingCode + ", startEpochMicrosec="
        + startEpochMicrosec + ", lastEpochMicrosec=" + lastEpochMicrosec + ", timeZoneOffset="
        + timeZoneOffset + "]";
  }

}
