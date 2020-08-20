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
public class PnfRegEventFields implements Serializable {
  /**
   * 
   */
  private static final long serialVersionUID = 2183003488649290852L;
  private String pnfRegistrationFieldsVersion;
  private String serialNumber;
  private String vendorName;
  private String oamV4IpAddress;
  private String oamV6IpAddress;
  private String softwareVersion;
  private String unitType;
  private String unitFamily;
  private String modelNumber;
  private String manufactureDate;
  private String macAddress;
  private String lastServiceDate;
  @JsonProperty("additionalFields")
  private Serializable additionalFields = null;

  public String getPnfRegistrationFieldsVersion() {
    return pnfRegistrationFieldsVersion;
  }

  public void setPnfRegistrationFieldsVersion(String pnfRegistrationFieldsVersion) {
    this.pnfRegistrationFieldsVersion = pnfRegistrationFieldsVersion;
  }

  public String getSerialNumber() {
    return serialNumber;
  }

  public void setSerialNumber(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public String getVendorName() {
    return vendorName;
  }

  public void setVendorName(String vendorName) {
    this.vendorName = vendorName;
  }

  public String getOamV4IpAddress() {
    return oamV4IpAddress;
  }

  public void setOamV4IpAddress(String oamV4IpAddress) {
    this.oamV4IpAddress = oamV4IpAddress;
  }

  public String getOamV6IpAddress() {
    return oamV6IpAddress;
  }

  public void setOamV6IpAddress(String oamV6IpAddress) {
    this.oamV6IpAddress = oamV6IpAddress;
  }

  public String getSoftwareVersion() {
    return softwareVersion;
  }

  public void setSoftwareVersion(String softwareVersion) {
    this.softwareVersion = softwareVersion;
  }

  public String getUnitType() {
    return unitType;
  }

  public void setUnitType(String unitType) {
    this.unitType = unitType;
  }

  public String getUnitFamily() {
    return unitFamily;
  }

  public void setUnitFamily(String unitFamily) {
    this.unitFamily = unitFamily;
  }

  public String getModelNumber() {
    return modelNumber;
  }

  public void setModelNumber(String modelNumber) {
    this.modelNumber = modelNumber;
  }

  public String getManufactureDate() {
    return manufactureDate;
  }

  public void setManufactureDate(String manufactureDate) {
    this.manufactureDate = manufactureDate;
  }

  public String getMacAddress() {
    return macAddress;
  }

  public void setMacAddress(String macAddress) {
    this.macAddress = macAddress;
  }

  public String getLastServiceDate() {
    return lastServiceDate;
  }

  public void setLastServiceDate(String lastServiceDate) {
    this.lastServiceDate = lastServiceDate;
  }

  public Serializable getAdditionalFields() {
    return additionalFields;
  }

  public void setAdditionalFields(Serializable additionalFields) {
    this.additionalFields = additionalFields;
  }

  @Override
  public String toString() {
    return "PnfRegEventFields [pnfRegistrationFieldsVersion=" + pnfRegistrationFieldsVersion
        + ", serialNumber=" + serialNumber + ", vendorName=" + vendorName + ", oamV4IpAddress="
        + oamV4IpAddress + ", oamV6IpAddress=" + oamV6IpAddress + ", softwareVersion="
        + softwareVersion + ", unitType=" + unitType + ", unitFamily=" + unitFamily
        + ", modelNumber=" + modelNumber + ", manufactureDate=" + manufactureDate + ", macAddress="
        + macAddress + ", lastServiceDate=" + lastServiceDate + ", additionalFields="
        + additionalFields + "]";
  }

}
