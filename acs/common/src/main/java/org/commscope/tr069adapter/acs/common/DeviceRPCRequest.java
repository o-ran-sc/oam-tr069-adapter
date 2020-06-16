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
import java.util.HashMap;
import java.util.Map;

public class DeviceRPCRequest implements Serializable, Cloneable {

  private static final long serialVersionUID = -7300390764969298783L;

  private DeviceDetails deviceDetails;

  private Long operationId;

  private OperationDetails opDetails;

  private OperationOptions options;
  private Map<String, Object> context = new HashMap<>();

  public DeviceRPCRequest() {

  }

  public DeviceRPCRequest(DeviceRPCResponse operationResult) {
    this.deviceDetails = operationResult.getDeviceDetails();
    this.operationId = operationResult.getOperationId();
  }

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

  public OperationDetails getOpDetails() {
    return opDetails;
  }

  public void setOpDetails(OperationDetails opDetails) {
    this.opDetails = opDetails;
  }

  public OperationOptions getOptions() {
    return options;
  }

  public void setOptions(OperationOptions options) {
    this.options = options;
  }

  public Map<String, Object> getContext() {
    return context;
  }

  public void addContextParam(String name, Object value) {
    this.context.put(name, value);
  }

  @Override
  public Object clone() throws CloneNotSupportedException {
    return super.clone();
  }

  @Override
  public String toString() {
    return "NBIDeviceOperationRequest [deviceId=" + deviceDetails.getDeviceId() + ", deviceType="
        + deviceDetails.getDeviceTypeId() + ", operationId=" + operationId + ", opDetails="
        + opDetails + ", options=" + options + "]";
  }

}
