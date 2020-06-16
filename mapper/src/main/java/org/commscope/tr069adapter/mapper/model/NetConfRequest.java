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

import java.io.Serializable;

public class NetConfRequest implements Serializable {

  private static final long serialVersionUID = -7593813981401577995L;

  private String requestXml;

  // operation status
  private String deviceId;

  public NetConfRequest() {
    super();
  }

  public NetConfRequest(String requestXml, String deviceId) {
    super();
    this.requestXml = requestXml;
    this.deviceId = deviceId;
  }

  public String getRequestXml() {
    return requestXml;
  }

  public void setRequestXml(String requestXml) {
    this.requestXml = requestXml;
  }

  public String getDeviceId() {
    return deviceId;
  }

  public void setDeviceId(String deviceId) {
    this.deviceId = deviceId;
  }

  @Override
  public String toString() {
    return "NetConfRequest [RequestXml=" + requestXml + ", deviceId=" + deviceId + "]";
  }

}
