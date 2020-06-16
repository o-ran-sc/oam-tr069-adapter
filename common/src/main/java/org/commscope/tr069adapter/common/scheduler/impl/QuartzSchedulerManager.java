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

import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.END_TIME_APPENDER;
import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.JOB_APPENDER;
import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.TRIGGER_APPENDER;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.commscope.tr069adapter.common.scheduler.ExecutionContext;
import org.commscope.tr069adapter.common.scheduler.JobInfo;
import org.commscope.tr069adapter.common.scheduler.SchedulerError;
import org.commscope.tr069adapter.common.scheduler.SchedulerException;
import org.commscope.tr069adapter.common.scheduler.SchedulerManager;
import org.commscope.tr069adapter.common.scheduler.TriggerInfo;
import org.quartz.JobDetail;
import org.quartz.JobExecutionException;
import org.quartz.JobKey;
import org.quartz.JobPersistenceException;
import org.quartz.Scheduler;
import org.quartz.SchedulerConfigException;
import org.quartz.Trigger;
import org.quartz.Trigger.TriggerState;
import org.quartz.TriggerKey;
import org.quartz.UnableToInterruptJobException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuartzSchedulerManager implements SchedulerManager {

  private static final String EXCEPTION_WHILE_GETTING_GET_JOB_INFO =
      "Exception while getting getJobInfo : ";

  private static final String ENABLE_JOB_DISABLE_SCHEDULE_FOR_JOB =
      "enableJob : Disable schedule for job ";

  private static final String METHOD_STARTED = " method started";

  private static final String EXCEPTION_WHILE_DISABLING_JOB = "Exception while disabling job : ";

  private static final String EXCEPTION_WHILE_CHECKING_JOB_ENABLE_STATE =
      "Exception while checking job enable state : ";

  private static final String ENABLE_JOB_ENABLE_SCHEDULE_FOR_JOB =
      "enableJob : Enable schedule for job ";

  private static final String EXCEPTION_WHILE_GETTING_JOB_LIST =
      "Exception while getting JobList : ";

  private static final String GET_JOB_LIST_METHOD_ENDED = "getJobList : method ended";

  private static final String EXCEPTION_WHILE_MODIFYING_SCHEDULE =
      "Exception while modifying schedule : ";

  private static final String MODIFY_SCHEDULE_MODIFY_SCHEDULE_FOR_JOB =
      "modifySchedule : Modify schedule for job ";

  private static final String EXCEPTION_WHILE_DELETING_SCHEDULE =
      "Exception while deleting schedule : ";

  private static final String DELETE_SCHEDULE_DELETING_SCHEDULE_FOR_JOB =
      "deleteSchedule : Deleting schedule for job ";

  private static final String EXCEPTION_WHILE_SCHEDULING_JOB_EXCEPTION =
      "Exception while scheduling job. Exception : ";

  private static final String SUCCESSFULLY_COMPLETED = " successfully completed";

  private static final String STARTED = " started";

  private static final String SCHEDULE_JOB_SCHEDUING_JOB = "scheduleJob : Scheduing job ";

  private static final String QUATZ_SCHEUDLER_FAILED_TO_START_CAUGHT_EXCEPTION =
      "Quatz scheudler failed to start: Caught Exception: ";

  @Autowired
  private Scheduler quartzScheduler;

  @Autowired
  QuartzSchedulerProducer quartzSchedulerProducer;

  private static final Log logger = LogFactory.getLog(QuartzSchedulerManager.class);

  public void setScheduler(Scheduler scheduler) {
    this.quartzScheduler = scheduler;
  }

  @Autowired
  QuartzSchedulerUtil schdulerUtil;

  public void setSchedulerUtil(QuartzSchedulerUtil schedulerUtil) {
    this.schdulerUtil = schedulerUtil;
  }

  @PostConstruct
  public void init() throws org.quartz.SchedulerException {
    logger.debug("init started");
    try {
      if (!quartzScheduler.isStarted()) {
        logger.debug("Running on active server; hence marking quartz as active service");
        quartzScheduler.start();
      }
    } catch (org.quartz.SchedulerException e) {
      logger.error(QUATZ_SCHEUDLER_FAILED_TO_START_CAUGHT_EXCEPTION + e.getMessage());
      throw e;
    }

    logger.debug("init complete");
  }

  @PreDestroy
  public void shutDown() throws org.quartz.SchedulerException {
    try {
      if (!quartzScheduler.isShutdown()) {
        quartzScheduler.shutdown();
      }

    } catch (org.quartz.SchedulerException e) {
      logger.debug("Quatz scheudler failed to stop: Caught Exception: " + e.getMessage());
      throw e;
    }
  }

  @Override
  public void scheduleJob(String jobName, TriggerInfo triggerInfo, ExecutionContext jobContext)
      throws SchedulerException {

    logger.debug(SCHEDULE_JOB_SCHEDUING_JOB + jobName + STARTED);
    Trigger trigger = this.getTrigger(jobName);
    if (null != trigger)
      throw new SchedulerException(SchedulerError.SCHEDULE_EXIST_ALREADY);
    try {
      if (quartzScheduler.isInStandbyMode()) {
        logger.debug(
            "scheduleJob : Server running on stand by mode; hence ignore Scheduing job " + jobName);
        return;
      }

      JobDetail jobDetail = schdulerUtil.getJobDetail(jobContext, jobName);
      trigger = schdulerUtil.createTrigger(triggerInfo, jobName);
      quartzScheduler.scheduleJob(jobDetail, trigger);
      logger.debug(SCHEDULE_JOB_SCHEDUING_JOB + jobName + SUCCESSFULLY_COMPLETED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      logger.error(EXCEPTION_WHILE_SCHEDULING_JOB_EXCEPTION + exception.getMessage());
      throw scheduleExcep;
    }
  }

  @Override
  public void scheduleJob(String jobName, String jobGroup, TriggerInfo triggerInfo,
      ExecutionContext jobContext) throws SchedulerException {

    logger.debug(SCHEDULE_JOB_SCHEDUING_JOB + jobName + STARTED);
    Trigger trigger = this.getTrigger(jobName, jobGroup);
    if (null != trigger)
      throw new SchedulerException(SchedulerError.SCHEDULE_EXIST_ALREADY);
    try {
      if (quartzScheduler.isInStandbyMode()) {
        logger.debug("scheduleJob : Server is running on stand by mode; hence ignore Scheduing job "
            + jobName);
        return;
      }

      JobDetail jobDetail = schdulerUtil.getJobDetail(jobContext, jobName, jobGroup);
      trigger = schdulerUtil.createTrigger(triggerInfo, jobName, jobGroup);
      quartzScheduler.scheduleJob(jobDetail, trigger);
      logger.debug(SCHEDULE_JOB_SCHEDUING_JOB + jobName + SUCCESSFULLY_COMPLETED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      logger.error(EXCEPTION_WHILE_SCHEDULING_JOB_EXCEPTION + exception.getMessage());
      throw scheduleExcep;
    }
  }

  @Override
  public void scheduleJob(String jobName, String jobGroup, TriggerInfo triggerInfo,
      ExecutionContext jobContext, TriggerInfo expiredTriggerInfo,
      ExecutionContext expiredExecutionContext) throws SchedulerException {

    logger.debug(SCHEDULE_JOB_SCHEDUING_JOB + jobName + STARTED);
    Trigger trigger = this.getTrigger(jobName);
    if (null != trigger)
      throw new SchedulerException(SchedulerError.SCHEDULE_EXIST_ALREADY);
    try {
      if (quartzScheduler.isInStandbyMode()) {
        logger.debug("scheduleJob : Server is running on stand by mode; hence ignore Scheduing job "
            + jobName);
        return;
      }

      JobDetail jobDetail = schdulerUtil.getJobDetail(jobContext, jobName, jobGroup);
      trigger = schdulerUtil.createTrigger(triggerInfo, jobName, jobGroup);
      quartzScheduler.scheduleJob(jobDetail, trigger);
      if (expiredTriggerInfo != null && expiredExecutionContext != null) {
        if (isJobScheduleExist(jobName + END_TIME_APPENDER, jobGroup)) {
          deleteSchedule(jobName + END_TIME_APPENDER, jobGroup);
        }
        JobDetail expiredJobDetail = schdulerUtil.getJobDetail(expiredExecutionContext,
            jobName + END_TIME_APPENDER, jobGroup);
        Trigger expiredTrigger =
            schdulerUtil.createTrigger(expiredTriggerInfo, jobName + END_TIME_APPENDER, jobGroup);
        quartzScheduler.scheduleJob(expiredJobDetail, expiredTrigger);
      }
      logger.debug(SCHEDULE_JOB_SCHEDUING_JOB + jobName + SUCCESSFULLY_COMPLETED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      logger.error(EXCEPTION_WHILE_SCHEDULING_JOB_EXCEPTION + exception.getMessage());
      throw scheduleExcep;
    }
  }

  @Override
  public void deleteSchedule(String jobName) throws SchedulerException {
    try {
      logger.debug(DELETE_SCHEDULE_DELETING_SCHEDULE_FOR_JOB + jobName + STARTED);
      Trigger trigger = this.getTrigger(jobName);
      if (null == trigger)
        throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      quartzScheduler.unscheduleJob(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName));
      logger.debug(DELETE_SCHEDULE_DELETING_SCHEDULE_FOR_JOB + jobName + SUCCESSFULLY_COMPLETED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      if (scheduleExcep.getScheduleError() == SchedulerError.UNKNOWN_ERROR)
        scheduleExcep = new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      logger.error(EXCEPTION_WHILE_DELETING_SCHEDULE + exception.getMessage());
      throw scheduleExcep;
    }
  }

  @Override
  public void modifySchedule(String jobName, TriggerInfo triggerInfo) throws SchedulerException {

    logger.debug(MODIFY_SCHEDULE_MODIFY_SCHEDULE_FOR_JOB + jobName + STARTED);
    Trigger trigger = this.getTrigger(jobName);
    if (null == trigger)
      throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
    trigger = schdulerUtil.createTrigger(triggerInfo, jobName);
    try {
      if (quartzScheduler.isInStandbyMode()) {
        logger.debug(
            "modifySchedule : Server is running on stand by mode; hence ignore modifySchedule job "
                + jobName);
      }

      quartzScheduler.rescheduleJob(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName),
          trigger);
      logger.debug(MODIFY_SCHEDULE_MODIFY_SCHEDULE_FOR_JOB + jobName + STARTED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      logger.error(EXCEPTION_WHILE_MODIFYING_SCHEDULE + exception.getMessage());
      throw scheduleExcep;
    }
  }

  @Override
  public void modifySchedule(String jobName, String jobGroup, TriggerInfo triggerInfo,
      TriggerInfo expiredTriggerInfo, ExecutionContext expiredExecutionContext)
      throws SchedulerException {

    logger.debug(MODIFY_SCHEDULE_MODIFY_SCHEDULE_FOR_JOB + jobName + STARTED);
    Trigger trigger = this.getTrigger(jobName, jobGroup);
    if (null == trigger)
      throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
    trigger = schdulerUtil.createTrigger(triggerInfo, jobName, jobGroup);
    try {
      quartzScheduler.rescheduleJob(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup),
          trigger);
      Trigger previousExpiredTrig = this.getTrigger(jobName + END_TIME_APPENDER, jobGroup);
      Trigger curExpiredTrigger =
          schdulerUtil.createTrigger(expiredTriggerInfo, jobName + END_TIME_APPENDER, jobGroup);

      if (previousExpiredTrig != null && expiredTriggerInfo != null) {
        quartzScheduler.rescheduleJob(
            TriggerKey.triggerKey(jobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup),
            curExpiredTrigger);
      } else if (previousExpiredTrig != null) {
        quartzScheduler.unscheduleJob(
            TriggerKey.triggerKey(jobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup));
      } else if (expiredTriggerInfo != null) {
        JobDetail expiredJobDetail = schdulerUtil.getJobDetail(expiredExecutionContext,
            jobName + END_TIME_APPENDER, jobGroup);
        quartzScheduler.scheduleJob(expiredJobDetail, curExpiredTrigger);
      }

      logger.debug(MODIFY_SCHEDULE_MODIFY_SCHEDULE_FOR_JOB + jobName + STARTED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      logger.error(EXCEPTION_WHILE_MODIFYING_SCHEDULE + exception.getMessage());
      throw scheduleExcep;
    }
  }

  @Override
  public List<JobInfo> getJobList() {
    final String methodName = "getJobList";
    logger.debug(methodName + " : " + METHOD_STARTED);
    List<String> triggerGroups = null;
    List<JobInfo> jobInfoList = new ArrayList<>();
    try {
      triggerGroups = quartzScheduler.getTriggerGroupNames();
      for (String triggerGroup : triggerGroups) {
        Trigger trigger = quartzScheduler
            .getTrigger(TriggerKey.triggerKey(triggerGroup + TRIGGER_APPENDER, triggerGroup));
        JobInfo jobInfo = new JobInfo();

        jobInfo.setEnabled(isJobEnabled(triggerGroup));
        jobInfo.setJobName(triggerGroup);
        jobInfo.setTriggerInfo(schdulerUtil.getTriggerInfo(trigger));
        jobInfoList.add(jobInfo);
        logger.debug(GET_JOB_LIST_METHOD_ENDED);
      }
    } catch (org.quartz.SchedulerException exception) {
      logger.error(EXCEPTION_WHILE_GETTING_JOB_LIST + exception.getLocalizedMessage());
    }
    return jobInfoList;
  }

  @Override
  public JobInfo getJobInfo(String jobId) {
    final String methodName = "getJobInfo";
    logger.debug(methodName + " : " + METHOD_STARTED);
    List<String> triggerGroups = null;
    try {
      triggerGroups = quartzScheduler.getTriggerGroupNames();
      for (String triggerGroup : triggerGroups) {
        Trigger trigger = quartzScheduler
            .getTrigger(TriggerKey.triggerKey(triggerGroup + TRIGGER_APPENDER, triggerGroup));
        JobInfo jobInfo = new JobInfo();

        jobInfo.setEnabled(isJobEnabled(triggerGroup));
        jobInfo.setJobName(triggerGroup);
        jobInfo.setTriggerInfo(schdulerUtil.getTriggerInfo(trigger));
        if (jobId.equals(triggerGroup)) {
          logger.debug(methodName + " : " + "method ended");
          return jobInfo;
        }
      }
    } catch (org.quartz.SchedulerException exception) {
      logger.error(EXCEPTION_WHILE_GETTING_GET_JOB_INFO + exception.getLocalizedMessage());
    }
    logger.debug(methodName + " : " + "method ended");
    return null;
  }

  Trigger getTrigger(String jobName) {
    Trigger trigger = null;
    try {
      trigger =
          quartzScheduler.getTrigger(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName));
    } catch (org.quartz.SchedulerException e) {
      logger.error(e.getLocalizedMessage());
    }
    return trigger;
  }

  Trigger getTrigger(String jobName, String jobGroup) {
    Trigger trigger = null;
    try {
      trigger =
          quartzScheduler.getTrigger(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup));
    } catch (org.quartz.SchedulerException e) {
      logger.error(e.getLocalizedMessage());
    }
    return trigger;
  }

  @Override
  public boolean isJobEnabled(String jobName) {
    boolean retVal = true;

    try {
      Trigger trigger = getTrigger(jobName);
      if (null == trigger)
        return false;
      retVal = quartzScheduler
          .getTriggerState(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName))
          .equals(TriggerState.PAUSED);
    } catch (org.quartz.SchedulerException exception) {
      logger.error(EXCEPTION_WHILE_CHECKING_JOB_ENABLE_STATE + exception.getMessage());
    }
    return !retVal;
  }

  @Override
  public void enableJob(String jobName) throws SchedulerException {
    logger.debug(ENABLE_JOB_ENABLE_SCHEDULE_FOR_JOB + jobName + STARTED);
    try {
      Trigger trigger = getTrigger(jobName);
      if (null == trigger)
        throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      if (quartzScheduler
          .getTriggerState(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName))
          .equals(TriggerState.PAUSED))
        quartzScheduler.resumeTrigger(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName));

    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      if (scheduleExcep.getScheduleError() == SchedulerError.UNKNOWN_ERROR)
        scheduleExcep = new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      logger.error(EXCEPTION_WHILE_DISABLING_JOB + exception.getMessage());
      throw scheduleExcep;
    }
    logger.debug(ENABLE_JOB_ENABLE_SCHEDULE_FOR_JOB + jobName + SUCCESSFULLY_COMPLETED);
  }

  @Override
  public void disableJob(String jobName) throws SchedulerException {
    logger.debug(ENABLE_JOB_DISABLE_SCHEDULE_FOR_JOB + jobName + STARTED);
    try {
      Trigger trigger = getTrigger(jobName);
      if (null == trigger)
        throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      if (!quartzScheduler
          .getTriggerState(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName))
          .equals(TriggerState.PAUSED))
        quartzScheduler.pauseTrigger(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobName));

    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      if (scheduleExcep.getScheduleError() == SchedulerError.UNKNOWN_ERROR)
        scheduleExcep = new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      logger.error(EXCEPTION_WHILE_DISABLING_JOB + exception.getMessage());
      throw scheduleExcep;
    }
    logger.debug(ENABLE_JOB_DISABLE_SCHEDULE_FOR_JOB + jobName + SUCCESSFULLY_COMPLETED);
  }

  private SchedulerException resolveError(org.quartz.SchedulerException excep) {
    SchedulerException schedulerExcep = new SchedulerException(SchedulerError.UNKNOWN_ERROR);
    try {
      throw excep;
    } catch (JobExecutionException jobExec) {
      logger.error("JobExecutionException : " + jobExec.getMessage());
      schedulerExcep = new SchedulerException(SchedulerError.JOB_EXECUTION_ERROR);
    } catch (JobPersistenceException persistenExcep) {
      schedulerExcep = new SchedulerException(SchedulerError.DATABASE_ERROR);
    } catch (SchedulerConfigException configExcep) {
      schedulerExcep = new SchedulerException(SchedulerError.INVALID_CONFIG);
    } catch (UnableToInterruptJobException interruExcep) {
      schedulerExcep = new SchedulerException(SchedulerError.INTERRUPT_ERROR);
    } catch (org.quartz.SchedulerException e) {
      logger.error("SchedulerException : " + e.getMessage());
    }
    return schedulerExcep;
  }

  @Override
  public boolean isJobExist(String jobName) throws SchedulerException {
    Trigger trigger = this.getTrigger(jobName);
    return (null != trigger);
  }

  @Override
  public void modifySchedule(String jobName, String jobGroup, TriggerInfo triggerInfo)
      throws SchedulerException {
    logger.debug(MODIFY_SCHEDULE_MODIFY_SCHEDULE_FOR_JOB + jobName + STARTED);
    Trigger trigger = this.getTrigger(jobName, jobGroup);
    if (null == trigger)
      throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
    trigger = schdulerUtil.createTrigger(triggerInfo, jobName, jobGroup);
    try {
      quartzScheduler.rescheduleJob(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup),
          trigger);
      logger.debug(MODIFY_SCHEDULE_MODIFY_SCHEDULE_FOR_JOB + jobName + STARTED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      logger.error(EXCEPTION_WHILE_MODIFYING_SCHEDULE + exception.getMessage());
      throw scheduleExcep;
    }

  }

  @Override
  public void deleteSchedule(String jobName, String jobGroup) throws SchedulerException {
    try {
      logger.debug(DELETE_SCHEDULE_DELETING_SCHEDULE_FOR_JOB + jobName + STARTED);
      Trigger trigger = this.getTrigger(jobName, jobGroup);
      if (null == trigger)
        throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      Trigger endTimeTrigger =
          this.getTrigger(jobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup);
      if (null != endTimeTrigger) {
        quartzScheduler.unscheduleJob(
            TriggerKey.triggerKey(jobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup));
      }
      quartzScheduler.unscheduleJob(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup));
      logger.debug(DELETE_SCHEDULE_DELETING_SCHEDULE_FOR_JOB + jobName + SUCCESSFULLY_COMPLETED);
    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      if (scheduleExcep.getScheduleError() == SchedulerError.UNKNOWN_ERROR)
        scheduleExcep = new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      logger.error(EXCEPTION_WHILE_DELETING_SCHEDULE + exception.getMessage());
      throw scheduleExcep;
    }
  }

  // Delete all jobs together or nothing.
  // TODO- This need to be taken care by Container-Managed Transactions later

  @Override
  public void deleteSchedule(String startJobName, String endJobName, String monitorJobName,
      String jobGroup) throws SchedulerException {
    try {
      logger.debug(DELETE_SCHEDULE_DELETING_SCHEDULE_FOR_JOB + startJobName + STARTED);
      Trigger trigger1 = this.getTrigger(startJobName, jobGroup);
      Trigger trigger2 = this.getTrigger(endJobName, jobGroup);
      Trigger trigger3 = this.getTrigger(monitorJobName, jobGroup);

      Trigger endTimeTrigger1 =
          this.getTrigger(startJobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup);
      Trigger endTimeTrigger2 =
          this.getTrigger(endJobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup);
      Trigger endTimeTrigger3 =
          this.getTrigger(monitorJobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup);

      logger.debug(DELETE_SCHEDULE_DELETING_SCHEDULE_FOR_JOB + startJobName + ", " + endJobName
          + ", " + monitorJobName + SUCCESSFULLY_COMPLETED);
      if (null != endTimeTrigger1) {
        quartzScheduler.unscheduleJob(
            TriggerKey.triggerKey(startJobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup));
      }
      if (null != endTimeTrigger2) {
        quartzScheduler.unscheduleJob(
            TriggerKey.triggerKey(endJobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup));
      }
      if (null != endTimeTrigger3) {
        quartzScheduler.unscheduleJob(
            TriggerKey.triggerKey(monitorJobName + END_TIME_APPENDER + TRIGGER_APPENDER, jobGroup));
      }
      if (null != trigger1) {
        quartzScheduler
            .unscheduleJob(TriggerKey.triggerKey(startJobName + TRIGGER_APPENDER, jobGroup));
      }
      if (null != trigger2) {
        quartzScheduler
            .unscheduleJob(TriggerKey.triggerKey(endJobName + TRIGGER_APPENDER, jobGroup));
      }
      if (null != trigger3) {
        quartzScheduler
            .unscheduleJob(TriggerKey.triggerKey(monitorJobName + TRIGGER_APPENDER, jobGroup));
      }

    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      if (scheduleExcep.getScheduleError() == SchedulerError.UNKNOWN_ERROR)
        scheduleExcep = new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      logger.error(EXCEPTION_WHILE_DELETING_SCHEDULE + exception.getMessage());
      throw scheduleExcep;
    }
  }

  @Override
  public boolean isJobEnabled(String jobName, String jobGroup) {
    boolean retVal = true;

    try {
      Trigger trigger = getTrigger(jobName, jobGroup);
      if (null == trigger)
        return false;
      retVal = quartzScheduler
          .getTriggerState(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup))
          .equals(TriggerState.PAUSED);
    } catch (org.quartz.SchedulerException exception) {
      logger.error(EXCEPTION_WHILE_CHECKING_JOB_ENABLE_STATE + exception.getMessage());
    }
    return !retVal;
  }

  @Override
  public void enableJob(String jobName, String jobGroup) throws SchedulerException {
    logger.debug(ENABLE_JOB_ENABLE_SCHEDULE_FOR_JOB + jobName + STARTED);
    try {
      Trigger trigger = getTrigger(jobName, jobGroup);
      if (null == trigger)
        throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      if (quartzScheduler
          .getTriggerState(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup))
          .equals(TriggerState.PAUSED))
        quartzScheduler.resumeTrigger(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup));

    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      if (scheduleExcep.getScheduleError() == SchedulerError.UNKNOWN_ERROR)
        scheduleExcep = new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      logger.error(EXCEPTION_WHILE_DISABLING_JOB + exception.getMessage());
      throw scheduleExcep;
    }
    logger.debug(ENABLE_JOB_ENABLE_SCHEDULE_FOR_JOB + jobName + SUCCESSFULLY_COMPLETED);

  }

  @Override
  public void disableJob(String jobName, String jobGroup) throws SchedulerException {
    logger.debug(ENABLE_JOB_DISABLE_SCHEDULE_FOR_JOB + jobName + STARTED);
    try {
      Trigger trigger = getTrigger(jobName, jobGroup);
      if (null == trigger)
        throw new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      if (!quartzScheduler
          .getTriggerState(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup))
          .equals(TriggerState.PAUSED))
        quartzScheduler.pauseTrigger(TriggerKey.triggerKey(jobName + TRIGGER_APPENDER, jobGroup));

    } catch (org.quartz.SchedulerException exception) {
      SchedulerException scheduleExcep = resolveError(exception);
      if (scheduleExcep.getScheduleError() == SchedulerError.UNKNOWN_ERROR)
        scheduleExcep = new SchedulerException(SchedulerError.SCHEDULE_DOES_NOT_EXIST);
      logger.error(EXCEPTION_WHILE_DISABLING_JOB + exception.getMessage());
      throw scheduleExcep;
    }
    logger.debug(ENABLE_JOB_DISABLE_SCHEDULE_FOR_JOB + jobName + " completed");

  }

  @Override
  public boolean isJobExist(String jobName, String jobGroup) throws SchedulerException {
    Trigger trigger = this.getTrigger(jobName, jobGroup);
    return (null != trigger);
  }

  @Override
  public JobInfo getJobInfo(String jobId, String jobGroup) {
    final String methodName = "getJobInfo";
    logger.debug(methodName + " : " + METHOD_STARTED);
    try {
      TriggerKey triggerKey = TriggerKey.triggerKey(jobId + TRIGGER_APPENDER, jobGroup);
      Trigger trigger = quartzScheduler.getTrigger(triggerKey);
      if (null != trigger) {
        JobInfo jobInfo = new JobInfo();
        TriggerState triggerState = quartzScheduler.getTriggerState(triggerKey);
        jobInfo.setEnabled(TriggerState.PAUSED.equals(triggerState));
        jobInfo.setJobName(jobId);
        jobInfo.setTriggerInfo(schdulerUtil.getTriggerInfo(trigger));
        logger.debug(methodName + " : method ended");
        return jobInfo;
      }
    } catch (org.quartz.SchedulerException exception) {
      logger.error(EXCEPTION_WHILE_GETTING_GET_JOB_INFO + exception.getLocalizedMessage());
    }
    logger.debug(methodName + " : method ended");
    return null;
  }

  @Override
  public boolean isJobScheduleExist(String jobName, String jobGroup) throws SchedulerException {
    Trigger trigger = this.getTrigger(jobName, jobGroup);
    return null != trigger;
  }

  @Override
  public void updateJobData(String jobName, String jobGroup, Map<String, Object> jobData)
      throws SchedulerException {
    try {
      JobDetail jobDetail =
          quartzScheduler.getJobDetail(new JobKey(jobName + JOB_APPENDER, jobGroup));
      if (null != jobDetail) {
        for (Entry<String, Object> job : jobData.entrySet()) {
          jobDetail.getJobDataMap().put(job.getKey(), job.getValue());
        }
        quartzScheduler.addJob(jobDetail, true, true);
      }

    } catch (org.quartz.SchedulerException e) {
      logger.error("Failed to update job data", e);
      throw new SchedulerException(SchedulerError.UNKNOWN_ERROR);
    }

  }

  @Override
  public Map<String, Object> getJobData(String jobName, String jobGroup) throws SchedulerException {

    Map<String, Object> jobData = new HashMap<>();
    try {
      JobDetail jobDetail =
          quartzScheduler.getJobDetail(new JobKey(jobName + JOB_APPENDER, jobGroup));
      if (null == jobDetail)
        return jobData;
      for (String key : jobDetail.getJobDataMap().keySet()) {
        jobData.put(key, jobDetail.getJobDataMap().get(key));
      }
    } catch (org.quartz.SchedulerException e) {
      logger.error("Failed to get job data", e);
      throw new SchedulerException(SchedulerError.UNKNOWN_ERROR);
    }
    return jobData;
  }

  @Override
  public void deleteJobData(String jobName, String jobGroup, Set<String> keys)
      throws SchedulerException {
    try {
      JobDetail jobDetail =
          quartzScheduler.getJobDetail(new JobKey(jobName + JOB_APPENDER, jobGroup));
      for (String key : keys) {
        jobDetail.getJobDataMap().remove(key);
      }
      quartzScheduler.addJob(jobDetail, true, true);
    } catch (org.quartz.SchedulerException e) {
      logger.error("Failed to update job data", e);
      throw new SchedulerException(SchedulerError.UNKNOWN_ERROR);
    }

  }

  @Override
  public boolean resumeQuartzSchedulars() throws SchedulerException {

    try {
      if (quartzScheduler.isInStandbyMode()) {
        logger.debug("Server is running on active server; hence marking quartz as active service");
        quartzScheduler.start();
      } else if (quartzScheduler.isStarted()) {
        logger.debug("Server is running on active server and quartz already running;");
      }
    } catch (org.quartz.SchedulerException e) {
      logger.error(QUATZ_SCHEUDLER_FAILED_TO_START_CAUGHT_EXCEPTION + e.getMessage());
      throw new SchedulerException(SchedulerError.UNKNOWN_ERROR);
    }

    return true;
  }

  @Override
  public boolean stopQuartzSchedulars() throws SchedulerException {
    try {

      if (quartzScheduler.isStarted()) {
        logger.debug("DM is running on stand-by server; hence marking quartz as stand-by service");
        quartzScheduler.standby();
      } else {
        logger.debug("DM is running on active server/quartz already running in standby mode;");
      }
    } catch (org.quartz.SchedulerException e) {
      logger.error(QUATZ_SCHEUDLER_FAILED_TO_START_CAUGHT_EXCEPTION + e.getMessage());
      throw new SchedulerException(SchedulerError.UNKNOWN_ERROR);
    }

    return true;
  }

  @Override
  public void switchToStandby() throws SchedulerException {
    try {
      if (null != quartzScheduler) {
        quartzScheduler.shutdown();
      }
      quartzScheduler = quartzSchedulerProducer.getStdScheduler();
      if (null != quartzScheduler)
        quartzScheduler.standby();
    } catch (org.quartz.SchedulerException e) {
      logger.error("while switchToStandby SchedulerException : " + e.getMessage());
    }
  }

  @Override
  public void switchToActive() throws SchedulerException {

    try {
      if (null != quartzScheduler) {
        quartzScheduler.shutdown();
      }

      quartzScheduler = quartzSchedulerProducer.getStdScheduler();
      if (null != quartzScheduler)
        quartzScheduler.start();
    } catch (org.quartz.SchedulerException e) {
      logger.error("while switchToActive SchedulerException : " + e.getMessage());
    }
  }

  @Override
  public boolean isQuartzRunninginActiveMode() {
    try {
      if (!quartzScheduler.getSchedulerInstanceId().endsWith("_standby")
          && quartzScheduler.isStarted())
        return true;
    } catch (org.quartz.SchedulerException e) {
      logger.error("Quatz scheudler failed to retrive mode: Caught Exception: " + e.getMessage());
    }
    return false;
  }
}
