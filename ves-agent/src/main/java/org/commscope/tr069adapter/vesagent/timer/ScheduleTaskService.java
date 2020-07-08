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
