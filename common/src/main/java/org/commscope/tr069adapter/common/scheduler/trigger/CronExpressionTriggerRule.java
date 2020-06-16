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

import org.commscope.tr069adapter.common.scheduler.TriggerInfo;
import org.quartz.CronScheduleBuilder;
import org.quartz.CronTrigger;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.springframework.stereotype.Component;

@Component
public class CronExpressionTriggerRule implements TriggerRule {

  public Trigger apply(TriggerInfo triggerInfo, String triggerName, String triggerGroup) {
    if (triggerInfo.getCronExpr() == null)
      return null;
    CronTrigger trigger = null;
    if (triggerInfo.getEndDate() != null) {
      trigger = TriggerBuilder.newTrigger().startAt(triggerInfo.getStartDate())
          .endAt(triggerInfo.getEndDate()).withIdentity(triggerName, triggerGroup)
          .withSchedule(CronScheduleBuilder.cronSchedule(triggerInfo.getCronExpr())
              .withMisfireHandlingInstructionIgnoreMisfires())
          .build();
    } else {
      trigger = TriggerBuilder.newTrigger().startAt(triggerInfo.getStartDate())
          .withIdentity(triggerName, triggerGroup)
          .withSchedule(CronScheduleBuilder.cronSchedule(triggerInfo.getCronExpr())
              .withMisfireHandlingInstructionIgnoreMisfires())
          .build();
    }

    return trigger;
  }
}
