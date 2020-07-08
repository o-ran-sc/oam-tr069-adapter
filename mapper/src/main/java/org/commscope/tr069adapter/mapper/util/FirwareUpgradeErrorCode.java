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

package org.commscope.tr069adapter.mapper.util;

import java.util.HashMap;
import java.util.Map;

public class FirwareUpgradeErrorCode {
  private static Map<Integer, String> ErrorCodes = new HashMap<Integer, String>();;

  static {
    ErrorCodes.put(9019, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9001, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9016, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9012, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9015, "PROTOCOL_ERROR");
    ErrorCodes.put(9002, "APPLICATION_ERROR");
    ErrorCodes.put(9003, "APPLICATION_ERROR");
    ErrorCodes.put(9020, "TIMEOUT");
    ErrorCodes.put(0, "COMPLETED");
  }

  public static String getErrorCodeMapping(int errorCode) {
    return ErrorCodes.get(errorCode) != null ? ErrorCodes.get(errorCode) : "APPLICATION_ERROR";
  }

}
