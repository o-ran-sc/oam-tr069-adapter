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

package org.commscope.tr069adapter.acs.common.inform;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

import org.commscope.tr069adapter.acs.common.DeviceInform;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
public abstract class AbstractDeviceInform extends DeviceInform {

  private static final long serialVersionUID = -487248664548665087L;

  private String externalIPAddress;

  private String lanIPAddress;

  public String getExternalIPAddress() {
    return externalIPAddress;
  }

  public void setExternalIPAddress(String externalIPAddress) {
    this.externalIPAddress = externalIPAddress;
  }

  public String getLanIPAddress() {
    return lanIPAddress;
  }

  public void setLanIPAddress(String lanIPAddress) {
    this.lanIPAddress = lanIPAddress;
  }
}
