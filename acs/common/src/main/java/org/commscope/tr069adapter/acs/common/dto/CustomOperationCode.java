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

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.commscope.tr069adapter.acs.common.OperationCode;

public enum CustomOperationCode implements OperationCode {

  CONFIGURE_MULTIPLE_OBJECTS(200, "ConfigureMultipleObject"), CONNECT(201,
      "CheckDeviceAvailability");

  private Integer operationCode;
  private String jndiName;

  private CustomOperationCode(Integer operationCode, String jndiName) {
    this.operationCode = operationCode;
    this.jndiName = jndiName;
  }

  public Integer getOperationCode() {
    return operationCode;
  }

  public String getJndiName() {
    return jndiName;
  }

  // Lookup tables
  private static final Map<Integer, CustomOperationCode> reverselookup = new HashMap<>();

  // Populate the lookup tables on loading time
  static {
    for (CustomOperationCode opCode : EnumSet.allOf(CustomOperationCode.class)) {
      reverselookup.put(opCode.getOperationCode(), opCode);
    }
  }

  public static CustomOperationCode getByOperationCode(Integer operationCode) {
    return reverselookup.get(operationCode);
  }
}
