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

import java.io.Serializable;
import java.util.Date;

import org.apache.logging.log4j.core.util.CronExpression;

public class TriggerInfo implements Serializable {

  /**
   * 
   */
  private static final long serialVersionUID = 6020368636888997385L;

  /**
   * Date representing the start of the job that is to be scheduled. If not provided it assumes the
   * current time as the start time.
   */
  private Date startDate = null;

  /**
   * Object which represent the time unit in which the interval to be calculated for periodic jobs.
   * default is days.
   */
  private TimeUnit timeUnit = TimeUnit.DAYS;

  /**
   * long represents the interval.Which scheduling the jobs this property along with timeUnit is
   * used to calculate the interval in between job executions for periodic jobs.
   */
  private long interval = -1;

  /**
   * Date representing the end date of the scheduled periodic jobs. If there is not end date is
   * provided then scheduling will be executed forever for every periodic interval.Periodic interval
   * is calculated using interval and Timeunit properties of this object.
   */
  private Date endDate = null;

  /**
   * This string representing valid cron expression to run Cron Triggger type of jobs. If the string
   * which is set is not a valid cron expression it will throw an scheduler exception indicating
   * invalid cron expression..
   */
  private String cronExpr = null;

  private Date nextFireTime = null;

  public Date getNextFireTime() {
    return nextFireTime;
  }

  public void setNextFireTime(Date nextFireTime) {
    this.nextFireTime = nextFireTime;
  }

  public Date getEndDate() {
    return endDate;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public String getCronExpr() {
    return cronExpr;
  }

  public void setCronExpr(String cronExpr) throws SchedulerException {
    try {
      new CronExpression(cronExpr);
    } catch (Exception excep) {
      throw new SchedulerException(SchedulerError.INVALID_CRON_EXPRESSION);
    }
    this.cronExpr = cronExpr;
  }

  public Date getStartDate() {

    return startDate;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public TimeUnit getTimeUnit() {
    return timeUnit;
  }

  public void setTimeUnit(TimeUnit timeUnit) {
    this.timeUnit = timeUnit;
  }

  public long getInterval() {
    return interval;
  }

  public void setInterval(long interval) throws SchedulerException {
    if (interval < -1)
      throw new SchedulerException(SchedulerError.INVALID_TIME_INTERVAL);
    this.interval = interval;
  }

  public TriggerInfo() {}

  public TriggerInfo(Date startDate) {
    this.startDate = startDate;
  }

  public TriggerInfo(Date startDate, long interval) throws SchedulerException {
    this(startDate);
    if (interval < -1)
      throw new SchedulerException(SchedulerError.INVALID_TIME_INTERVAL);
    this.interval = interval;
  }

  public TriggerInfo(Date startDate, Date endDate, long interval) throws SchedulerException {
    this(startDate, interval);

    if (endDate.getTime() < startDate.getTime())
      throw new SchedulerException(SchedulerError.INVALID_ENDDATE);
    this.endDate = endDate;

  }

  public TriggerInfo(String cronExpression) throws SchedulerException {
    try {
      new CronExpression(cronExpression);
    } catch (Exception excep) {
      throw new SchedulerException(SchedulerError.INVALID_CRON_EXPRESSION);
    }
    this.cronExpr = cronExpression;
  }
}
