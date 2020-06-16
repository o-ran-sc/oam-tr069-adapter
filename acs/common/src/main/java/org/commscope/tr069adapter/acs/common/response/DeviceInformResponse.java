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

package org.commscope.tr069adapter.acs.common.response;

import java.io.Serializable;

import org.commscope.tr069adapter.acs.common.DeviceDetails;

public class DeviceInformResponse implements Serializable {

  private static final long serialVersionUID = -6889552276964345347L;

  private String sessionId;

  private DeviceDetails deviceDetails;

  public DeviceInformResponse(String sessionId, DeviceDetails deviceDetails) {
    this.sessionId = sessionId;
    this.deviceDetails = deviceDetails;
  }

  public String getSessionId() {
    return sessionId;
  }

  public void setSessionId(String sessionId) {
    this.sessionId = sessionId;
  }

  public DeviceDetails getDeviceDetails() {
    return deviceDetails;
  }

  public void setDeviceDetails(DeviceDetails deviceDetails) {
    this.deviceDetails = deviceDetails;
  }

}
