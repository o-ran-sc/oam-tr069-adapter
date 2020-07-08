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

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import javax.annotation.PostConstruct;

import org.commscope.tr069adapter.vesagent.entity.DeviceDataEntity;
import org.commscope.tr069adapter.vesagent.repository.VesDataRepository;
import org.commscope.tr069adapter.vesagent.util.VesAgentConstants;
import org.commscope.tr069adapter.vesagent.util.VesAgentUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class StartupTimerService {
  private static final Logger logger = LoggerFactory.getLogger(StartupTimerService.class);

  @Autowired
  private Function<String, HeartBeatTimeoutTask> beanFactory;

  public HeartBeatTimeoutTask getBeanInstance(String name) {
    return beanFactory.apply(name);
  }

  @Autowired
  VesDataRepository vesDataRepository;

  @Autowired
  ScheduleTaskService timerService;

  @PostConstruct
  public void initializeDeviceReachabilityCheckTimers() {
    logger.debug("Initializing all device connectivity check timer tasks.");
    List<DeviceDataEntity> deviceDataEntityList =
        vesDataRepository.findByAttrGroup(VesAgentConstants.HEART_BEAT);

    if (VesAgentUtils.isNullOrEmpty(deviceDataEntityList)) {
      logger.debug("No device reachability check timer tasks exist.");
      return;
    }

    for (DeviceDataEntity deviceDataEntity : deviceDataEntityList) {
      String heartBeatPeriod = null;

      if (null != deviceDataEntity.getAttributesMap()) {
        heartBeatPeriod =
            deviceDataEntity.getAttributesMap().get(VesAgentConstants.HEART_BEAT_PERIOD);
      }

      if (!VesAgentUtils.isNullOrEmpty(heartBeatPeriod)
          && !heartBeatPeriod.equals(VesAgentConstants.REMOVE_HEART_BEAT_TIMER_VAL)) {
        logger.info("Creating device connectivity check timer tasks for device {}.",
            deviceDataEntity.getDeviceId());
        ScheduleInfo scheduleInfo = new ScheduleInfo();
        scheduleInfo.setInterval(Integer.parseInt(heartBeatPeriod));
        scheduleInfo.setTimeUnit(TimeUnit.SECONDS);

        HeartBeatTimeoutTask callbackTask = getBeanInstance(deviceDataEntity.getDeviceId());

        timerService.schedule(deviceDataEntity.getDeviceId(), scheduleInfo, callbackTask);
      }
    }
  }
}
