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

package org.commscope.tr069adapter.acs.common.faults;

public enum AcsFaultCode {

  FAULT_CODE_8000("8000", "Device not exists"), FAULT_CODE_8001("8001",
      "Device not Activated"), FAULT_CODE_8002("8002", "Operation Aborted"), FAULT_CODE_8003("8003",
          "Operation Timedout"), FAULT_CODE_8004("8004", "Invalid NBI request");

  private String faultKey;
  private String faultString;

  private AcsFaultCode(String faultKey, String faultString) {
    this.faultKey = faultKey;
    this.faultString = faultString;
  }

  public String getFaultKey() {
    return faultKey;
  }

  public String getFaultString() {
    return faultString;
  }

}
