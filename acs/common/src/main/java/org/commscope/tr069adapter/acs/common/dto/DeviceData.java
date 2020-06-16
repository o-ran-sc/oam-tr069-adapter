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

/**
 * 
 * @version 1.0
 * @since September 27, 2019
 * @author Prashant Kumar
 */
public class DeviceData implements Serializable {

  /**
   * 
   */
  private static final long serialVersionUID = -1408846578781735000L;

  String serialNumber;
  String autenticationString;
  String oui;
  String productClass;

  public String getSerialNumber() {
    return serialNumber;
  }

  public void setSerialNumber(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public String getAutenticationString() {
    return autenticationString;
  }

  public void setAutenticationString(String autenticationString) {
    this.autenticationString = autenticationString;
  }

  public String getOui() {
    return oui;
  }

  public void setOui(String oui) {
    this.oui = oui;
  }

  public String getProductClass() {
    return productClass;
  }

  public void setProductClass(String productClass) {
    this.productClass = productClass;
  }

}
