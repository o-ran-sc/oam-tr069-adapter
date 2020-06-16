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

import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.vesagent.model.PnfRegEventFields;

public class InformEventData {

  public static final String MANUFACTURER = "Manufacturer";
  public static final String SERIALNUMBER = "SerialNumber";
  public static final String SOFTWAREVERSION = "SoftwareVersion";
  public static final String MANUFACTUREROUI = "ManufacturerOUI";
  public static final String PRODUCTCLASS = "ProductClass";
  public static final String IPV6ADDRESS = "IPv6Address";
  public static final String IPADDRESS = "IPAddress";
  private PnfRegEventFields feilds = new PnfRegEventFields();

  public void parse(ParameterDTO parameter, String tempEEParam) {
    if (null == parameter.getParamValue()) {
      return;
    }
    if (tempEEParam.endsWith(MANUFACTURER)) {
      feilds.setVendorName(parameter.getParamValue());
    } else if (tempEEParam.endsWith(SERIALNUMBER)) {
      feilds.setSerialNumber(parameter.getParamValue());
      feilds.setMacAddress(parameter.getParamValue());
    } else if (tempEEParam.endsWith(SOFTWAREVERSION)) {
      feilds.setSoftwareVersion(parameter.getParamValue());
    } else if (tempEEParam.endsWith(MANUFACTUREROUI)) {
      feilds.setUnitType(parameter.getParamValue());
    } else if (tempEEParam.endsWith(PRODUCTCLASS)) {
      feilds.setUnitFamily(parameter.getParamValue());
    } else if (tempEEParam.contains(IPV6ADDRESS)) {
      feilds.setOamV6IpAddress(parameter.getParamValue());
    } else if (tempEEParam.contains(IPADDRESS)) {
      feilds.setOamV4IpAddress(parameter.getParamValue());
    }
  }

  public PnfRegEventFields getFeilds() {
    return feilds;
  }

  public void setFeilds(PnfRegEventFields feilds) {
    this.feilds = feilds;
  }
}
