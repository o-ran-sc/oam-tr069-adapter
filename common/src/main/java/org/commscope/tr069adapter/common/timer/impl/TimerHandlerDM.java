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

import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.TIMER_DATA_KEY;
import static org.commscope.tr069adapter.common.scheduler.impl.TimerAPIConstants.TIMER_LISTENER_KEY;

import java.io.Serializable;
import java.util.Date;

import org.commscope.tr069adapter.common.scheduler.ExecutionContext;
import org.commscope.tr069adapter.common.scheduler.Job;
import org.commscope.tr069adapter.common.scheduler.impl.QuartzJob;
import org.commscope.tr069adapter.common.timer.TimerListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component("timerHandler")
public class TimerHandlerDM implements Job {

  private static final Logger logger = LoggerFactory.getLogger(TimerHandlerDM.class);

  @Override
  public void execute(ExecutionContext context) {

    try {
      logger.debug("Notification is received at {} for timer :{}", new Date(), context.getJobId());
      String beanName = context.getJobData(TIMER_LISTENER_KEY) + "";
      String timerId = context.getJobId();
      Object obj = context.getJobData(TIMER_DATA_KEY);
      Serializable data = (obj == null) ? null : (Serializable) obj;
      ApplicationContext ctx = QuartzJob.getApplicationContext();
      TimerListener listener = (TimerListener) ctx.getBean(beanName);
      listener.notifyTimeout(timerId, data);
    } catch (Exception e) {
      logger.error("Error while notifying timer listener. {}", e.toString());
    }

  }

}
