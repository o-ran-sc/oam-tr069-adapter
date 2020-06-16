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


package org.commscope.tr069adapter.common.scheduler.trigger;

import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerUtil.getInterval;
import static org.quartz.SimpleScheduleBuilder.simpleSchedule;

import java.util.Date;

import org.commscope.tr069adapter.common.scheduler.TriggerInfo;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.springframework.stereotype.Component;

@Component
public class PeriodicTriggerRule implements TriggerRule {

  /**
   * Run the periodically once.
   */
  public Trigger apply(TriggerInfo triggerInfo, String triggerName, String triggerGroup) {

    Date startDate = triggerInfo.getStartDate();
    Date endDate = triggerInfo.getEndDate();
    long interval = triggerInfo.getInterval();
    boolean evaluate = interval != -1 && endDate != null;
    Trigger trigger = null;
    if (evaluate) {
      long intervalInMilliseconds =
          getInterval(triggerInfo.getInterval(), triggerInfo.getTimeUnit());
      TriggerBuilder<Trigger> triggerBuilder =
          TriggerBuilder.newTrigger().withIdentity(triggerName, triggerGroup);

      triggerBuilder =
          (startDate == null) ? triggerBuilder.startNow() : triggerBuilder.startAt(startDate);

      trigger = triggerBuilder
          .withSchedule(simpleSchedule().withIntervalInMilliseconds(intervalInMilliseconds)
              .withMisfireHandlingInstructionNextWithExistingCount().repeatForever())
          .endAt(endDate).build();
    }
    return trigger;
  }
}
