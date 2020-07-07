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

package org.commscope.tr069adapter.vesagent;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "config")
public class VesConfiguration {

  private String faultvesCollectorHost;
  private String faultvesCollectorPort;
  private String pnfregvesCollectorHost;
  private String pnfregvesCollectorPort;
  private String vendorName;
  private String vesVersion;
  private String eventVersion;
  private String faultEventType;
  private String pnfRegEventType;
  private String faultFeildVersion;
  private String pnfFeildVersion;
  private String unitFamily;
  private String unitType;
  private String faultEventSourcePrefix;

  private String requestTimeout;
  private String mapperPath;

  public String getFaultvesCollectorHost() {
    return faultvesCollectorHost;
  }

  public void setFaultvesCollectorHost(String faultvesCollectorHost) {
    this.faultvesCollectorHost = faultvesCollectorHost;
  }

  public String getFaultvesCollectorPort() {
    return faultvesCollectorPort;
  }

  public void setFaultvesCollectorPort(String faultvesCollectorPort) {
    this.faultvesCollectorPort = faultvesCollectorPort;
  }

  public String getPnfregvesCollectorHost() {
    return pnfregvesCollectorHost;
  }

  public void setPnfregvesCollectorHost(String pnfregvesCollectorHost) {
    this.pnfregvesCollectorHost = pnfregvesCollectorHost;
  }

  public String getPnfregvesCollectorPort() {
    return pnfregvesCollectorPort;
  }

  public void setPnfregvesCollectorPort(String pnfregvesCollectorPort) {
    this.pnfregvesCollectorPort = pnfregvesCollectorPort;
  }

  public String getVendorName() {
    return vendorName;
  }

  public void setVendorName(String vendorName) {
    this.vendorName = vendorName;
  }

  public String getVesVersion() {
    return vesVersion;
  }

  public void setVesVersion(String vesVersion) {
    this.vesVersion = vesVersion;
  }

  public String getEventVersion() {
    return eventVersion;
  }

  public void setEventVersion(String eventVersion) {
    this.eventVersion = eventVersion;
  }

  public String getFaultEventType() {
    return faultEventType;
  }

  public void setFaultEventType(String faultEventType) {
    this.faultEventType = faultEventType;
  }

  public String getFaultFeildVersion() {
    return faultFeildVersion;
  }

  public void setFaultFeildVersion(String faultFeildVersion) {
    this.faultFeildVersion = faultFeildVersion;
  }

  public String getPnfFeildVersion() {
    return pnfFeildVersion;
  }

  public void setPnfFeildVersion(String pnfFeildVersion) {
    this.pnfFeildVersion = pnfFeildVersion;
  }

  public String getFaultVesUrl() {
    return "http://" + getFaultvesCollectorHost() + ":" + getFaultvesCollectorPort()
        + "/eventListener/v7";
  }

  public String getPnfRegVesUrl() {
    return "http://" + getPnfregvesCollectorHost() + ":" + getPnfregvesCollectorPort()
        + "/eventListener/v7";
  }

  public String getUnitFamily() {
    return unitFamily;
  }

  public void setUnitFamily(String unitFamily) {
    this.unitFamily = unitFamily;
  }

  public String getUnitType() {
    return unitType;
  }

  public void setUnitType(String unitType) {
    this.unitType = unitType;
  }

  public String getFaultEventSourcePrefix() {
    return faultEventSourcePrefix;
  }

  public void setFaultEventSourcePrefix(String faultEventSourcePrefix) {
    this.faultEventSourcePrefix = faultEventSourcePrefix;
  }

  public String getPnfRegEventType() {
    return pnfRegEventType;
  }

  public void setPnfRegEventType(String pnfRegEventType) {
    this.pnfRegEventType = pnfRegEventType;
  }

  public String getRequestTimeout() {
    return requestTimeout;
  }

  public void setRequestTimeout(String requestTimeout) {
    this.requestTimeout = requestTimeout;
  }

  public String getMapperPath() {
    return mapperPath;
  }

  public void setMapperPath(String mapperPath) {
    this.mapperPath = mapperPath;
  }

}
