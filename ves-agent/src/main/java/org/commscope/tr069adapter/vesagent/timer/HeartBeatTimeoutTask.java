package org.commscope.tr069adapter.vesagent.timer;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.async.AsyncRequestHandler;
import org.commscope.tr069adapter.vesagent.controller.HeartBeatMessageHandler;
import org.commscope.tr069adapter.vesagent.entity.DeviceDataEntity;
import org.commscope.tr069adapter.vesagent.repository.VesDataRepository;
import org.commscope.tr069adapter.vesagent.util.VesAgentConstants;
import org.commscope.tr069adapter.vesagent.util.VesAgentUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class HeartBeatTimeoutTask implements Runnable {
  private static final Logger logger = LoggerFactory.getLogger(HeartBeatTimeoutTask.class);

  @Autowired
  VesDataRepository vesDataRepository;

  @Autowired
  ScheduleTaskService timerService;

  @Autowired
  AsyncRequestHandler asyncHandler;

  @Autowired
  HeartBeatMessageHandler heartBeatMessageHandler;

  @Autowired
  VesConfiguration config;

  private String deviceId;

  @Override
  public void run() {
    logger.debug("Timer task: checking device {} connectivity.", deviceId);
    List<DeviceDataEntity> deviceDataEntityList =
        vesDataRepository.findByDeviceIdAndAttrGroup(deviceId, VesAgentConstants.HEART_BEAT);

    if (VesAgentUtils.isNullOrEmpty(deviceDataEntityList)
        || VesAgentUtils.isNullOrEmpty(deviceDataEntityList.get(0).getAttributesMap())) {
      timerService.cancelSchedule(deviceId);
      return;
    }

    DeviceDataEntity deviceDataEntity = deviceDataEntityList.get(0);
    String heartBeatPeriod =
        deviceDataEntity.getAttributesMap().get(VesAgentConstants.HEART_BEAT_PERIOD);

    if (VesAgentUtils.isNullOrEmpty(heartBeatPeriod)
        || heartBeatPeriod.equals(VesAgentConstants.REMOVE_HEART_BEAT_TIMER_VAL)) {
      timerService.cancelSchedule(deviceId);
      return;
    }

    ScheduleInfo scheduleInfo = new ScheduleInfo();
    scheduleInfo.setInterval(Integer.parseInt(heartBeatPeriod));
    scheduleInfo.setTimeUnit(TimeUnit.MINUTES);

    timerService.schedule(deviceId, scheduleInfo, this);

    asyncHandler.initiateDeviceReachabilityCheck(deviceDataEntity);
  }

  public HeartBeatTimeoutTask(String deviceId) {
    super();
    this.deviceId = deviceId;
  }

  public String getDeviceId() {
    return deviceId;
  }

  public void setDeviceId(String deviceId) {
    this.deviceId = deviceId;
  }
}
