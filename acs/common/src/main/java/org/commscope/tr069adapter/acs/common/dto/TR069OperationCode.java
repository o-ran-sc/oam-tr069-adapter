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

public enum TR069OperationCode implements OperationCode {

  GET_RPC_METHODS(100), SET_PARAMETER_VALUES(101), GET_PARAMETER_VALUES(102), GET_PARAMETER_NAMES(
      103), SET_PARAMETER_ATTRIBUTES(104), GET_PARAMETER_ATTRIBUTES(105), ADD_OBJECT(
          106), DELETE_OBJECT(107), REBOOT(108), DOWNLOAD(
              109), SCHEDULE_DOWNLOAD(110), UPLOAD(111), FACTORY_RESET(112), INITIATE_CR(120);

  private Integer operationCode;

  private TR069OperationCode(Integer operationCode) {
    this.operationCode = operationCode;
  }

  // Lookup tables
  private static final Map<Integer, TR069OperationCode> reverselookup = new HashMap<>();

  // Populate the lookup tables on loading time
  static {
    for (TR069OperationCode opCode : EnumSet.allOf(TR069OperationCode.class)) {
      reverselookup.put(opCode.getOperationCode(), opCode);
    }
  }

  public static TR069OperationCode getByOperationCode(Integer operationCode) {
    return reverselookup.get(operationCode);
  }

  public Integer getOperationCode() {
    return operationCode;
  }
}
