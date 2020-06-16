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

package org.commscope.tr069adapter.acs.common.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * Domain Object for Service Settings<br>
 * 
 * @version 1.0
 * @since September 27, 2019
 * @author prashantk
 */

public class ConfigurationData implements Serializable {
  /**
   * 
   */
  private static final long serialVersionUID = -6219229519491907038L;

  private String oui;
  private String productClass;
  private String softwareVersion;
  private String hardwareVersion;

  private String localDn;// ="0005B95196D0"

  private Map<String, String> parameterMONameValueMap;

  public String getLocalDn() {
    return localDn;
  }

  public void setLocalDn(String localDn) {
    this.localDn = localDn;
  }

  public String getHardwareVersion() {
    return hardwareVersion;
  }

  public void setHardwareVersion(String hardwareVersion) {
    this.hardwareVersion = hardwareVersion;
  }

  public String getProductClass() {
    return productClass;
  }

  public void setProductClass(String productClass) {
    this.productClass = productClass;
  }

  public String getOUI() {
    return oui;
  }

  public void setOUI(String oUI) {
    oui = oUI;
  }

  public String getSoftwareVersion() {
    return softwareVersion;
  }

  public void setSoftwareVersion(String softwareVersion) {
    this.softwareVersion = softwareVersion;
  }

  public Map<String, String> getParameterMONameValueMap() {
    return parameterMONameValueMap;
  }

  public void setParameterMONameValueMap(Map<String, String> parameterMONameValueMap) {
    this.parameterMONameValueMap = parameterMONameValueMap;
  }
}
