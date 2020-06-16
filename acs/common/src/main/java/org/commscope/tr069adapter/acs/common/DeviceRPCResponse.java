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

package org.commscope.tr069adapter.acs.common;

import java.io.Serializable;

public class DeviceRPCResponse implements Serializable {

  private static final long serialVersionUID = -6581290052228229775L;

  private DeviceDetails deviceDetails;

  private Long operationId;

  private OperationResponse operationResponse;

  private String faultKey;

  private String faultString;

  public DeviceDetails getDeviceDetails() {
    return deviceDetails;
  }

  public void setDeviceDetails(DeviceDetails deviceDetails) {
    this.deviceDetails = deviceDetails;
  }

  public Long getOperationId() {
    return operationId;
  }

  public void setOperationId(Long operationId) {
    this.operationId = operationId;
  }

  public OperationResponse getOperationResponse() {
    return operationResponse;
  }

  public void setOperationResponse(OperationResponse operationResponse) {
    this.operationResponse = operationResponse;
  }

  public String getFaultKey() {
    return faultKey;
  }

  public void setFaultKey(String faultKey) {
    this.faultKey = faultKey;
  }

  public String getFaultString() {
    return faultString;
  }

  public void setFaultString(String faultString) {
    this.faultString = faultString;
  }

  @Override
  public String toString() {
    return "NBIOperationResult [deviceDetails=" + deviceDetails + ", operationId=" + operationId
        + ", operationResponse=" + operationResponse + ", faultKey=" + faultKey + ", faultString="
        + faultString + "]";
  }

}
