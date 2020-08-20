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

package org.commscope.tr069adapter.mapper;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "config")
public class MapperConfigProperties {

  private String requesTimeout;

  private String preConfigureOnPNP;

  private String sbiUri;

  private String connStatusUri;

  private String configDBUri;

  private String nbiNotificationUri;
  private String nbiServerManagerUri;
  private String vesUri;

  private String netConfServerIP;
  private String netConfServerPort;

  private String alarmMORegex;

  private String verConfigUri;
  private String vendorName;

  public String getRequesTimeout() {
    return requesTimeout;
  }

  public void setRequesTimeout(String requesTimeout) {
    this.requesTimeout = requesTimeout;
  }

  public String getPreConfigureOnPNP() {
    return preConfigureOnPNP;
  }

  public void setPreConfigureOnPNP(String preConfigureOnPNP) {
    this.preConfigureOnPNP = preConfigureOnPNP;
  }

  public String getSbiUri() {
    return sbiUri;
  }

  public void setSbiUri(String sbiUri) {
    this.sbiUri = sbiUri;
  }

  public String getConfigDBUri() {
    return configDBUri;
  }

  public void setConfigDBUri(String configDBUri) {
    this.configDBUri = configDBUri;
  }

  public String getNbiNotificationUri() {
    return nbiNotificationUri;
  }

  public void setNbiNotificationUri(String nbiNotificationUri) {
    this.nbiNotificationUri = nbiNotificationUri;
  }

  public String getNbiServerManagerUri() {
    return nbiServerManagerUri;
  }

  public void setNbiServerManagerUri(String nbiServerManagerUri) {
    this.nbiServerManagerUri = nbiServerManagerUri;
  }

  public String getVesUri() {
    return vesUri;
  }

  public void setVesUri(String vesUri) {
    this.vesUri = vesUri;
  }

  public String getNetConfServerIP() {
    return netConfServerIP;
  }

  public void setNetConfServerIP(String netConfServerIP) {
    this.netConfServerIP = netConfServerIP;
  }

  public String getNetConfServerPort() {
    return netConfServerPort;
  }

  public void setNetConfServerPort(String netConfServerPort) {
    this.netConfServerPort = netConfServerPort;
  }

  public String getAlarmMORegex() {
    return alarmMORegex;
  }

  public void setAlarmMORegex(String alarmMORegex) {
    this.alarmMORegex = alarmMORegex;
  }

  public String getVerConfigUri() {
    return verConfigUri;
  }

  public void setVerConfigUri(String verConfigUri) {
    this.verConfigUri = verConfigUri;
  }

  public String getConnStatusUri() {
    return connStatusUri;
  }

  public void setConnStatusUri(String connStatusUri) {
    this.connStatusUri = connStatusUri;
  }

  public String getVendorName() {
    return vendorName;
  }

  public void setVendorName(String vendorName) {
    this.vendorName = vendorName;
  }

}
