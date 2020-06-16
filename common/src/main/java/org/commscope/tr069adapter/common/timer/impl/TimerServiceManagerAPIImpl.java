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

package org.commscope.tr069adapter.common.timer.impl;

import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.DM_SUB_SYSTEM;
import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.TIMEOUT_HANDLER_JNDI_NAME;
import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.TIMER_DATA_KEY;
import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.TIMER_ID_KEY;
import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.TIMER_JOB_GROUP;
import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.TIMER_LISTENER_KEY;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.commscope.tr069adapter.common.scheduler.ExecutionContext;
import org.commscope.tr069adapter.common.scheduler.SchedulerException;
import org.commscope.tr069adapter.common.scheduler.SchedulerManager;
import org.commscope.tr069adapter.common.scheduler.TriggerInfo;
import org.commscope.tr069adapter.common.timer.TimerException;
import org.commscope.tr069adapter.common.timer.TimerServiceManagerAPI;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TimerServiceManagerAPIImpl implements TimerServiceManagerAPI {

  @Autowired
  SchedulerManager quartzScheduleManager;

  private static Log logger = LogFactory.getLog(TimerServiceManagerAPIImpl.class);

  @Override
  public void startTimer(String timerId, String listenerBean, long timeout, Serializable data)
      throws TimerException {
    try {
      logger.debug("Requested to start timer for " + timerId + ", listener bean=" + listenerBean);
      TriggerInfo triggerInfo = new TriggerInfo();
      triggerInfo.setStartDate(new Date(new Date().getTime() + timeout));
      ExecutionContext executionContext =
          new ExecutionContext(DM_SUB_SYSTEM, TIMEOUT_HANDLER_JNDI_NAME);
      executionContext.addJobData(TIMER_ID_KEY, timerId);
      executionContext.addJobData(TIMER_LISTENER_KEY, listenerBean);
      executionContext.addJobData(TIMER_DATA_KEY, data);
      quartzScheduleManager.scheduleJob(timerId, TIMER_JOB_GROUP, triggerInfo, executionContext);
      logger.debug("Successfully started timer for " + timerId);
    } catch (Exception e) {
      logger.error("Failed to start timer for " + timerId + ", Error Details :" + e.getMessage(),
          e);
      throw new TimerException("Failed to start timer for " + timerId, e);
    }

  }

  @Override
  public void modifyTimer(String timerId, long newTimeout, Serializable data)
      throws TimerException {
    try {
      logger.debug("Requested to modify timer for " + timerId);

      if (quartzScheduleManager.isJobScheduleExist(timerId, TIMER_JOB_GROUP)) {
        Map<String, Object> jobDataMap = quartzScheduleManager.getJobData(timerId, TIMER_JOB_GROUP);
        TriggerInfo triggerInfo = new TriggerInfo();
        triggerInfo.setStartDate(new Date(new Date().getTime() + newTimeout));
        ExecutionContext executionContext =
            new ExecutionContext(DM_SUB_SYSTEM, TIMEOUT_HANDLER_JNDI_NAME);
        executionContext.addJobData(TIMER_ID_KEY, timerId);
        executionContext.addJobData(TIMER_LISTENER_KEY, jobDataMap.get(TIMER_LISTENER_KEY));
        executionContext.addJobData(TIMER_DATA_KEY, data);
        quartzScheduleManager.modifySchedule(timerId, TIMER_JOB_GROUP, triggerInfo);
        logger.debug("Successfully modified timer for " + timerId);
      } else {
        logger.error("Timer with timerId " + timerId + " does not exist.");
        throw new TimerException("Timer with timerId " + timerId + " does not exist.");
      }

    } catch (SchedulerException e) {
      logger.error("Failed to modify timer with timerId " + timerId);
      throw new TimerException("Failed to modify timer with timerId " + timerId);
    }
  }

  @Override
  public void stopTimer(String timerId) throws TimerException {
    try {
      logger.debug("Requested to stop the timer." + timerId);
      if (quartzScheduleManager.isJobExist(timerId, TIMER_JOB_GROUP))
        quartzScheduleManager.deleteSchedule(timerId, TIMER_JOB_GROUP);
      logger.debug("Successfully stopped the timer." + timerId);
    } catch (Exception e) {
      logger.error("Failed to stop timer for " + timerId + ", Error Details :" + e.getMessage(), e);
      throw new TimerException("Failed to stop the timer for " + timerId, e);
    }
  }

}
