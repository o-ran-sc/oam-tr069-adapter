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

package org.commscope.tr069adapter.acs.requestprocessor.dto;

import java.io.Serializable;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;

public class CustomOperationData implements Serializable {

  private static final long serialVersionUID = 4784758072507503641L;

  private TR069DeviceDetails deviceDetails;
  private DeviceRPCResponse deviceRPCResponse;
  private DeviceRPCRequest deviceRPCRequest;

  public CustomOperationData(TR069DeviceDetails deviceDetails, DeviceRPCResponse deviceRPCResponse,
      DeviceRPCRequest deviceRPCRequest) {
    this.deviceDetails = deviceDetails;
    this.deviceRPCResponse = deviceRPCResponse;
    this.deviceRPCRequest = deviceRPCRequest;
  }

  public TR069DeviceDetails getDeviceDetails() {
    return deviceDetails;
  }

  public void setDeviceDetails(TR069DeviceDetails deviceDetails) {
    this.deviceDetails = deviceDetails;
  }

  public DeviceRPCResponse getDeviceRPCResponse() {
    return deviceRPCResponse;
  }

  public void setDeviceRPCResponse(DeviceRPCResponse deviceRPCResponse) {
    this.deviceRPCResponse = deviceRPCResponse;
  }

  public DeviceRPCRequest getDeviceRPCRequest() {
    return deviceRPCRequest;
  }

  public void setDeviceRPCRequest(DeviceRPCRequest deviceRPCRequest) {
    this.deviceRPCRequest = deviceRPCRequest;
  }

}
