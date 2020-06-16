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

public class QuartzSchedulerConstants {

  private QuartzSchedulerConstants() {}

  public static final String JOB_NAME = "jobNameKey";
  public static final String SUB_SYSTEM = "subSystemKey";
  public static final String TRIGGER_APPENDER = "_trigger";
  public static final String JOB_APPENDER = "_job";
  public static final String END_TIME_APPENDER = "_expiry";
  public static final String DEVICE_HISTORY = "deviceHistory";

}
