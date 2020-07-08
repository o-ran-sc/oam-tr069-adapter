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

package org.commscope.tr069adapter.vesagent.util;

/**
 * 
 * @version 1.0
 * @since June 5, 2020
 * @author Prashant Kumar
 */

public class VesAgentConstants {

  public static final String HEART_BEAT = "heartbeat";

  public static final String HEART_BEAT_PERIOD = "heartbeat.heartbeatPeriod";
  public static final String HEART_BEAT_PERIOD_DEFAULT_VAL = "5"; // IN MIUTES

  public static final String COUNT_DOWN_TIMER = "heartbeat.countDownTimer";

  public static final String ENODEB_NAME = "ENODEB_NAME";

  public static final String OBJECT_DATA_TYPE = "object";

  public static final String REMOVE_HEART_BEAT_TIMER_VAL = "0";
  public static final int DEVICE_IS_REACHABLE = 100;
  public static final String ABORTED_BY_BOOT_BOOTSTRAP = "8002";

  public static final int RPC_SUCCESS = 0;
  public static final int RPC_FAILED = 1;
  public static final String INVALID_ARGUMENTS = "9003";
  public static final String INVALID_PARAMETER_NAME = "9005";
  public static final String INVALID_PARAMETER_VALUE = "9007";


}
