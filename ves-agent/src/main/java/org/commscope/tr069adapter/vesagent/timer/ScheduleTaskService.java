package org.commscope.tr069adapter.vesagent.timer;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.support.PeriodicTrigger;
import org.springframework.stereotype.Service;

/**
 * 
 * @version 1.0
 * @since June 5, 2020
 * @author Prashant Kumar
 */

@Service
@EnableScheduling
public class ScheduleTaskService {

  @Autowired
  TaskScheduler scheduler;

  Map<String, ScheduledFuture<?>> jobsMap = new HashMap<>();

  public ScheduleTaskService(TaskScheduler scheduler) {
    this.scheduler = scheduler;
  }

  public void schedule(String jobId, ScheduleInfo scheduleInfo, Runnable callBackTask) {
    cancelSchedule(jobId);

    PeriodicTrigger trigger =
        new PeriodicTrigger(scheduleInfo.getInterval(), scheduleInfo.getTimeUnit());
    trigger.setInitialDelay(scheduleInfo.getInterval());

    ScheduledFuture<?> scheduledTask = scheduler.schedule(callBackTask, trigger);
    jobsMap.put(jobId, scheduledTask);
  }

  public void cancelSchedule(String id) {
    ScheduledFuture<?> scheduledTask = jobsMap.get(id);
    if (scheduledTask != null) {
      scheduledTask.cancel(true);
      jobsMap.remove(id);
    }
  }

  public ScheduledFuture<?> getSchedule(String id) {
    return jobsMap.get(id);
  }

  public Long getTimeRemainingTillNextExecution(String id, TimeUnit timeUnit) {
    ScheduledFuture<?> scheduledTask = jobsMap.get(id);

    if (scheduledTask != null) {
      return scheduledTask.getDelay(timeUnit);
    } else {
      return null;
    }
  }
}
