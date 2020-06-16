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

import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.JOB_APPENDER;
import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.JOB_NAME;
import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.SUB_SYSTEM;
import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.TRIGGER_APPENDER;

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.commscope.tr069adapter.common.scheduler.ExecutionContext;
import org.commscope.tr069adapter.common.scheduler.SchedulerException;
import org.commscope.tr069adapter.common.scheduler.TimeUnit;
import org.commscope.tr069adapter.common.scheduler.TriggerInfo;
import org.commscope.tr069adapter.common.scheduler.trigger.TriggerRule;
import org.quartz.CronTrigger;
import org.quartz.JobBuilder;
import org.quartz.JobDetail;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuartzSchedulerUtil {

  @Autowired
  List<TriggerRule> triggerRules;

  private static final Log logger = LogFactory.getLog(QuartzSchedulerUtil.class);

  public void setTriggerList(List<TriggerRule> triggerRules) {
    this.triggerRules = triggerRules;
  }

  Trigger createTrigger(TriggerInfo triggerInfo, String triggerGroup) {
    if (null == triggerInfo || null == triggerGroup || "".equals(triggerGroup))
      return null;
    String triggerName = triggerGroup + TRIGGER_APPENDER;
    Trigger trigger = null;
    for (TriggerRule rule : triggerRules) {
      trigger = rule.apply(triggerInfo, triggerName, triggerGroup);
      if (trigger != null)
        break;
    }
    return trigger;
  }

  Trigger createTrigger(TriggerInfo triggerInfo, String triggerName, String triggerGroup) {
    if (null == triggerInfo || null == triggerGroup || "".equals(triggerGroup))
      return null;
    triggerName = triggerName + TRIGGER_APPENDER;
    Trigger trigger = null;
    for (TriggerRule rule : triggerRules) {
      trigger = rule.apply(triggerInfo, triggerName, triggerGroup);
      if (trigger != null)
        break;
    }
    return trigger;
  }

  JobDetail getJobDetail(ExecutionContext jobContext, String jobGroup) {
    if (null == jobContext || null == jobGroup || "".equals(jobGroup))
      return null;
    String jobName = jobGroup + JOB_APPENDER;
    JobDetail jobDetail =
        JobBuilder.newJob(QuartzJob.class).withIdentity(jobName, jobGroup).build();
    for (String key : jobContext.getJobDatakeySet()) {
      jobDetail.getJobDataMap().put(key, jobContext.getJobData(key));
    }
    jobDetail.getJobDataMap().put(JOB_NAME, jobContext.getJobName());
    jobDetail.getJobDataMap().put(SUB_SYSTEM, jobContext.getSubSystemName());
    return jobDetail;
  }

  JobDetail getJobDetail(ExecutionContext jobContext, String jobName, String jobGroup) {
    if (null == jobContext || null == jobGroup || "".equals(jobGroup))
      return null;
    JobDetail jobDetail =
        JobBuilder.newJob(QuartzJob.class).withIdentity(jobName + JOB_APPENDER, jobGroup).build();
    for (String key : jobContext.getJobDatakeySet()) {
      jobDetail.getJobDataMap().put(key, jobContext.getJobData(key));
    }
    jobDetail.getJobDataMap().put(JOB_NAME, jobContext.getJobName());
    jobDetail.getJobDataMap().put(SUB_SYSTEM, jobContext.getSubSystemName());
    return jobDetail;
  }

  public static long getInterval(long interval, TimeUnit unit) {
    long retVal = 0;
    switch (unit) {
      case DAYS:
        retVal = interval * 24 * 60 * 60 * 1000;
        break;
      case HOURS:
        retVal = interval * 60 * 60 * 1000;
        break;
      case MINUTES:
        retVal = interval * 60 * 1000;
        break;
      case SECONDS:
        retVal = interval * 1000;
        break;
      case MILLISECONDS:
        retVal = interval * 1;
        break;
    }
    return retVal;
  }

  public TriggerInfo getTriggerInfo(Trigger trigger) {
    TriggerInfo triggerInfo = new TriggerInfo();
    if (trigger == null) {
      return triggerInfo;
    }
    try {
      triggerInfo.setStartDate(trigger.getStartTime());
      triggerInfo.setEndDate(trigger.getEndTime());
      triggerInfo.setNextFireTime(trigger.getNextFireTime());
      if (trigger instanceof SimpleTrigger) {
        SimpleTrigger simpleTrigger = (SimpleTrigger) trigger;
        triggerInfo.setInterval(simpleTrigger.getRepeatInterval());

      } else if (trigger instanceof CronTrigger) {
        CronTrigger cronTrigger = (CronTrigger) trigger;
        triggerInfo.setCronExpr(cronTrigger.getCronExpression());

      }

    } catch (SchedulerException e) {
      logger.error("Error in getTriggerInfo : " + e.getMessage());
    }
    return triggerInfo;
  }
}
