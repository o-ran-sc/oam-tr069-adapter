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

package org.commscope.tr069adapter.common.scheduler;

public enum SchedulerError {

  INVALID_STARTDATE("QRTZ_01", "Given date is past date",
      "Please provide a future date"), INVALID_TIME_INTERVAL("QRTZ_02",
          "Invalid provided is zero or negative",
          "Provide positive interval"), INVALID_ENDDATE("QRTZ_03", "Given date is past date",
              "Please provide a future date"), UNKNOWN_ERROR("QRTZ_04", "Unexpected Exception",
                  "Please see the system logs for more details."), INVALID_CRON_EXPRESSION(
                      "QRTZ_05", "Not a valid cron expression",
                      "Please provide valid cron expression for schedule"), SCHEDULE_DOES_NOT_EXIST(
                          "QRTZ_06", "Schedule name provided does not exist",
                          "Please provide a valid schedule name"), SCHEDULE_EXIST_ALREADY("QRTZ_07",
                              "Schedule name provided is already exist in the system",
                              "Please modify the existing or add new job with different name."), JOB_EXECUTION_ERROR(
                                  "QRTZ_08", "Unexepcted exeception while executing the job",
                                  "Please look in to system logs for more information"), DATABASE_ERROR(
                                      "QRTZ_09", "There database problem with Sheduling",
                                      "please see the logs for more information"), INVALID_CONFIG(
                                          "QRTZ_10",
                                          "There is some problem with Quartz scheduler configuration.",
                                          "Please look in to system logs for more information "), INTERRUPT_ERROR(
                                              "QRTZ_11", "Not able to interrupt the running job",
                                              "Please look in  to system logs for more information");


  private String errorCode;
  private String errorDescription;
  private String resolution;

  SchedulerError(String errorCode, String errorDescription, String resolution) {
    this.errorCode = errorCode;
    this.errorDescription = errorDescription;
    this.resolution = resolution;
  }

  public String getErrorCode() {
    return errorCode;
  }

  public String getErrorMessage() {
    return errorDescription;
  }

  public String getResolution() {
    return resolution;
  }

  @Override
  public String toString() {
    StringBuilder str =
        new StringBuilder("**********************************************************\n");
    str.append("* Error Code : " + errorCode + " *\n");
    str.append("* Error : " + errorDescription + " *\n");
    str.append("* Resolution : " + resolution + " *\n");
    str.append("**********************************************************");
    return str.toString();
  }

}
