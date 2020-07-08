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

public class MapperConstants {
  public static final String HEART_BEAT_PERIOD = "heartbeatPeriod";
  public static final String HEART_BEAT = "heartbeat";
  public static final String COUNT_DOWN_TIMER = "countDownTimer";
  public static final String COUNT_DOWN_TIMER_SET_VAL = "0";
  public static final String INVALID_COUNT_DOWN_TIMER_MSG =
      "Only 0 is allowed for countDownTimer value during set operation";
  public static final String INVALID_PARAM_VAL_ERROR_CODE = "9007";

  public static final int DEVICE_REACHABLE_STATUS_CODE = 100;
  public static final int DEVICE_REACHABILITY_OP_FAILURE_CODE = 101;
  public static final String DEVICE_TIMEOUT_STATUS_CODE = "8006";
  public static final String DEVICE_REACHABLE_MSG = "Device is reachable";
  public static final String DEFAULT_OP_TIMEOUT_MSG = "Operation Timed out";
  public static final String RPC_SUCCESS = "0";
}
