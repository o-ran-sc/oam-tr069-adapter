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

package org.commscope.tr069adapter.acs.common.dto;

import java.io.Serializable;

import org.commscope.tr069adapter.acs.common.OperationCode;

public class DeviceOperationRequestDetails implements Serializable {

  private static final long serialVersionUID = 7705056823973628669L;

  private TR069DeviceDetails deviceDetails;

  private Long operationId;

  private OperationCode opCode;

  public TR069DeviceDetails getDeviceDetails() {
    return deviceDetails;
  }

  public void setDeviceDetails(TR069DeviceDetails deviceDetails) {
    this.deviceDetails = deviceDetails;
  }

  public Long getOperationId() {
    return operationId;
  }

  public void setOperationId(Long operationId) {
    this.operationId = operationId;
  }

  public OperationCode getOpCode() {
    return opCode;
  }

  public void setOpCode(OperationCode opCode) {
    this.opCode = opCode;
  }
}
