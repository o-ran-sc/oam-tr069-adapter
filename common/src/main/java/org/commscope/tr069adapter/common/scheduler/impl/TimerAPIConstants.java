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

package org.commscope.tr069adapter.common.scheduler.impl;

public class TimerAPIConstants {

  private TimerAPIConstants() {}

  public static final String DM_SUB_SYSTEM = "dm";
  public static final String TIMEOUT_HANDLER_JNDI_NAME = "timerHandler";
  public static final String TIMER_ID_KEY = "timerIdKey";
  public static final String TIMER_LISTENER_KEY = "timerListenerKey";
  public static final String TIMER_DATA_KEY = "timerDataKey";
  public static final String TIMER_JOB_GROUP = "timerJobGroup";
  public static final long HOUR_IN_MILLIS = 1000 * 60 * 60l;

}
