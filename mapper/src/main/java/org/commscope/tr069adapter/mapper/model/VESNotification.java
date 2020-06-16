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

package org.commscope.tr069adapter.mapper.model;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

import java.io.Serializable;

import org.commscope.tr069adapter.acs.common.DeviceInform;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
public class VESNotification implements Serializable {
  private DeviceInform devnotification;
  private NetConfServerDetails netconfDetails;
  private String eNodeBName;

  public DeviceInform getDevnotification() {
    return devnotification;
  }

  public void setDevnotification(DeviceInform devnotification) {
    this.devnotification = devnotification;
  }

  public NetConfServerDetails getNetconfDetails() {
    return netconfDetails;
  }

  public void setNetconfDetails(NetConfServerDetails netconfDetails) {
    this.netconfDetails = netconfDetails;
  }

  public String geteNodeBName() {
    return eNodeBName;
  }

  public void seteNodeBName(String eNodeBName) {
    this.eNodeBName = eNodeBName;
  }

}
