package org.commscope.tr069adapter.vesagent.timer;

import java.util.concurrent.TimeUnit;

/**
 * 
 * @version 1.0
 * @since June 5, 2020
 * @author Prashant Kumar
 */

public class ScheduleInfo {
  private int interval;
  private TimeUnit timeUnit;

  public int getInterval() {
    return interval;
  }

  public void setInterval(int interval) {
    this.interval = interval;
  }

  public TimeUnit getTimeUnit() {
    return timeUnit;
  }

  public void setTimeUnit(TimeUnit timeUnit) {
    this.timeUnit = timeUnit;
  }

}
