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

package org.commscope.tr069adapter.acs.cpe.rpc;

import java.util.Date;

public class DeviceInfo {

  Date lastcontact;
  Date confupTime;
  String hardware;
  String serialno;
  String url;
  String username;
  String password;
  Integer authtype;
  byte[] props;
  Integer hwid;
  String hardwareModelName;
  String oui;
  String pClass;
  String conrequser;
  String conreqpass;
  String profileName;
  String softVersion;
  String configVersion;
  Boolean forcePasswords;
  Boolean reboot;
  String autoConfigurationFilePath;

  public DeviceInfo(String serialno, String username, String password, String m,
      Boolean forcePasswords, Integer authType) {
    this.serialno = serialno;
    this.username = username;
    this.password = password;
    this.hardwareModelName = m;
    this.forcePasswords = forcePasswords;
    this.authtype = authType;
  }

  /**
   * @param authtype the authtype to set
   */
  public void setAuthtype(Integer authtype) {
    this.authtype = authtype;
  }

  /**
   * @param conrequser the conrequser to set
   */
  public void setConrequser(String conrequser) {
    this.conrequser = conrequser;
  }

  /**
   * @param conreqpass the conreqpass to set
   */
  public void setConreqpass(String conreqpass) {
    this.conreqpass = conreqpass;
  }

  /**
   * @param confupTime the confupTime to set
   */
  public void setConfupTime(Date confupTime) {
    this.confupTime = confupTime;
  }

  /**
   * @param url the url to set
   */
  public void setUrl(String url) {
    this.url = url;
  }

  /**
   * @return the username
   */
  public String getUsername() {
    return username;
  }

  /**
   * @return the password
   */
  public String getPassword() {
    return password;
  }

  /**
   * @return the authtype
   */
  public Integer getAuthtype() {
    return authtype;
  }

  /**
   * @return the props
   */
  public byte[] getProps() {
    return props;
  }

  /**
   * @param props the props to set
   */
  public void setProps(byte[] props) {
    this.props = props;
  }

  /**
   * @return the hwid
   */
  public Integer getHwid() {
    return hwid;
  }

  /**
   * @param hwid the hwid to set
   */
  public void setHwid(Integer hwid) {
    this.hwid = hwid;
  }

  /**
   * @return the profileName
   */
  public String getProfileName() {
    return profileName;
  }

  /**
   * @param profileName the profileName to set
   */
  public void setProfileName(String profileName) {
    this.profileName = profileName;
  }

  /**
   * @return the forcePasswords
   */
  public Boolean getForcePasswords() {
    return forcePasswords;
  }

  /**
   * @param forcePasswords the forcePasswords to set
   */
  public void setForcePasswords(Boolean forcePasswords) {
    this.forcePasswords = forcePasswords;
  }

  /**
   * @return the reboot
   */
  public Boolean getReboot() {
    return reboot;
  }

  /**
   * @param reboot the reboot to set
   */
  public void setReboot(Boolean reboot) {
    this.reboot = reboot;
  }

  /**
   * @return the lastcontact
   */
  public Date getLastcontact() {
    return lastcontact;
  }

  /**
   * @param lastcontact the lastcontact to set
   */
  public void setLastcontact(Date lastcontact) {
    this.lastcontact = lastcontact;
  }

  /**
   * @return the hardware
   */
  public String getHardware() {
    return hardware;
  }

  /**
   * @param hardware the hardware to set
   */
  public void setHardware(String hardware) {
    this.hardware = hardware;
  }

  /**
   * @return the conrequser
   */
  public String getConrequser() {
    return conrequser;
  }

  /**
   * @return the conreqpass
   */
  public String getConreqpass() {
    return conreqpass;
  }

  /**
   * @return the confupTime
   */
  public Date getConfupTime() {
    return confupTime;
  }

  /**
   * @return the url
   */
  public String getUrl() {
    return url;
  }

  /**
   * @return the softVersion
   */
  public String getSoftVersion() {
    return softVersion;
  }

  /**
   * @param softVersion the softVersion to set
   */
  public void setSoftVersion(String softVersion) {
    this.softVersion = softVersion;
  }

  /**
   * @return the configVersion
   */
  public String getConfigVersion() {
    return configVersion;
  }

  /**
   * @param configVersion the configVersion to set
   */
  public void setConfigVersion(String configVersion) {
    this.configVersion = configVersion;
  }

  /**
   * @return the serialno
   */
  public String getSerialno() {
    return serialno;
  }

  /**
   * @return the hardwareModelName
   */
  public String getHardwareModelName() {
    return hardwareModelName;
  }

  /**
   * @param hardwareModelName the hardwareModelName to set
   */
  public void setHardwareModelName(String hardwareModelName) {
    this.hardwareModelName = hardwareModelName;
  }

  /**
   * @return the autoConfigurationFilePath
   */
  public String getAutoConfigurationFilePath() {
    return autoConfigurationFilePath;
  }

  /**
   * @param autoConfigurationFilePath the autoConfigurationFilePath to set
   */
  public void setAutoConfigurationFilePath(String autoConfigurationFilePath) {
    this.autoConfigurationFilePath = autoConfigurationFilePath;
  }

}
