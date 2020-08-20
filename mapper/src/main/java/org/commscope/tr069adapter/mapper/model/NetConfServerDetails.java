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

public class NetConfServerDetails implements Serializable {

  private static final long serialVersionUID = -7594811982301577995L;

  private String deviceId;
  private String enodeBName;
  private String listenAddress;
  private String listenPort;
  private String swVersion;
  private String hwVersion;

  private NetconfServerManagementError error;

  public NetConfServerDetails() {
    super();
  }

  public NetConfServerDetails(String deviceId, String listenAddress, String listenPort) {
    super();
    this.deviceId = deviceId;
    this.listenAddress = listenAddress;
    this.listenPort = listenPort;
  }

  public String getDeviceId() {
    return deviceId;
  }

  public void setDeviceId(String deviceId) {
    this.deviceId = deviceId;
  }

  public String getListenAddress() {
    return listenAddress;
  }

  public void setListenAddress(String listenAddress) {
    this.listenAddress = listenAddress;
  }

  public String getListenPort() {
    return listenPort;
  }

  public void setListenPort(String listenPort) {
    this.listenPort = listenPort;
  }

  public NetconfServerManagementError getError() {
    return error;
  }

  public void setError(NetconfServerManagementError error) {
    this.error = error;
  }

  public String getEnodeBName() {
    return enodeBName;
  }

  public void setEnodeBName(String enodeBName) {
    this.enodeBName = enodeBName;
  }

  public String getSwVersion() {
    return swVersion;
  }

  public void setSwVersion(String swVersion) {
    this.swVersion = swVersion;
  }

  public String getHwVersion() {
    return hwVersion;
  }

  public void setHwVersion(String hwVersion) {
    this.hwVersion = hwVersion;
  }

  @Override
  public String toString() {
    return "NetConfServerDetails [deviceId=" + deviceId + ", enodeBName=" + enodeBName
        + ", listenAddress=" + listenAddress + ", listenPort=" + listenPort + ", swversion="
        + swVersion + ", hwversion=" + hwVersion + ", error=" + error + "]";
  }
}
