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

import java.util.ArrayList;
import java.util.List;

import org.commscope.tr069adapter.common.scheduler.trigger.CronExpressionTriggerRule;
import org.commscope.tr069adapter.common.scheduler.trigger.ExpiredTirggerRule;
import org.commscope.tr069adapter.common.scheduler.trigger.OneTimeTirggerRule;
import org.commscope.tr069adapter.common.scheduler.trigger.PeriodicForeverTriggerRule;
import org.commscope.tr069adapter.common.scheduler.trigger.PeriodicTriggerRule;
import org.commscope.tr069adapter.common.scheduler.trigger.TriggerRule;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

public class TriggerRuleProducer {

  @Autowired
  CronExpressionTriggerRule cronExpressionTriggerRule;

  @Autowired
  OneTimeTirggerRule oneTimeTirggerRule;

  @Autowired
  PeriodicForeverTriggerRule periodicForeverTriggerRule;

  @Autowired
  PeriodicTriggerRule periodicTriggerRule;

  @Autowired
  ExpiredTirggerRule expiredTirggerRule;

  @Bean
  public List<TriggerRule> getTriggerRules() {

    List<TriggerRule> triggerRules = new ArrayList<>();
    triggerRules.add(cronExpressionTriggerRule);
    triggerRules.add(oneTimeTirggerRule);
    triggerRules.add(periodicForeverTriggerRule);
    triggerRules.add(periodicTriggerRule);
    triggerRules.add(expiredTirggerRule);

    return triggerRules;

  }
}
