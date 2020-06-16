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

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum SessionState {

  READY(0), PROCESSING(1), LOCKED(2), TERMINATED(3);

  private Integer value;

  private SessionState(Integer operationCode) {
    this.value = operationCode;
  }

  // Lookup tables
  private static final Map<Integer, SessionState> reverselookup = new HashMap<>();

  // Populate the lookup tables on loading time
  static {
    for (SessionState state : EnumSet.allOf(SessionState.class)) {
      reverselookup.put(state.getValue(), state);
    }
  }

  public static SessionState getByValue(Integer value) {
    return reverselookup.get(value);
  }

  public Integer getValue() {
    return value;
  }
}
