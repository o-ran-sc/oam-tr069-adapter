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

import java.util.Date;

import org.commscope.tr069adapter.common.scheduler.TriggerInfo;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.springframework.stereotype.Component;

@Component
public class ExpiredTirggerRule implements TriggerRule {

  /**
   * Run the job once.
   */
  public Trigger apply(TriggerInfo triggerInfo, String triggerName, String triggerGroup) {

    Date startDate = triggerInfo.getStartDate();
    Date endDate = triggerInfo.getEndDate();
    long interval = triggerInfo.getInterval();
    String strTrigger = triggerInfo.getCronExpr();
    boolean evaluate =
        (startDate == null) && (interval == -1) && (endDate != null) && (strTrigger == null);
    Trigger trigger = null;
    if (evaluate) {
      TriggerBuilder<Trigger> triggerBuilder =
          TriggerBuilder.newTrigger().withIdentity(triggerName, triggerGroup);
      triggerBuilder = triggerBuilder.startAt(endDate);
      triggerBuilder = triggerBuilder.endAt(new Date(endDate.getTime() + (1000 * 60 * 1)));
      trigger = triggerBuilder.build();
    }
    return trigger;
  }

}
