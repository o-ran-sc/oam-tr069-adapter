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

package org.commscope.tr069adapter.vesagent.service;

import com.google.gson.Gson;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.vesagent.async.WaitForNotifications;
import org.commscope.tr069adapter.vesagent.entity.DeviceDataEntity;
import org.commscope.tr069adapter.vesagent.exception.VesAgentException;
import org.commscope.tr069adapter.vesagent.repository.VesDataRepository;
import org.commscope.tr069adapter.vesagent.timer.HeartBeatTimeoutTask;
import org.commscope.tr069adapter.vesagent.timer.ScheduleInfo;
import org.commscope.tr069adapter.vesagent.timer.ScheduleTaskService;
import org.commscope.tr069adapter.vesagent.util.VesAgentConstants;
import org.commscope.tr069adapter.vesagent.util.VesAgentUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class VesAgentServiceHelper {
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  @Autowired
  private Function<String, HeartBeatTimeoutTask> beanFactory;

  public HeartBeatTimeoutTask getBeanInstance(String name) {
    return beanFactory.apply(name);
  }

  @Autowired
  VesDataRepository vesDataRepository;

  @Autowired
  WaitForNotifications waitForNotifications;

  @Autowired
  ScheduleTaskService timerService;

  private boolean saveDeviceDataEntity(DeviceDetails deviceDetails, String eNodeBName,
      String heartBeatPeriod, String countDownTimer) throws VesAgentException {

    List<DeviceDataEntity> deviceDataEntityList = vesDataRepository
        .findByDeviceIdAndAttrGroup(deviceDetails.getDeviceId(), VesAgentConstants.HEART_BEAT);

    DeviceDataEntity deviceDataEntity = null;
    Map<String, String> attrJsonMap = null;

    if (null == deviceDataEntityList || deviceDataEntityList.isEmpty()) {
      deviceDataEntity = new DeviceDataEntity();

      deviceDataEntity.setDeviceId(deviceDetails.getDeviceId());
      deviceDataEntity.seteNodeBName(eNodeBName);
      deviceDataEntity.setOui(deviceDetails.getOui());
      deviceDataEntity.setProductClass(deviceDetails.getProductClass());
      deviceDataEntity.setAttrGroup(VesAgentConstants.HEART_BEAT);

      attrJsonMap = new HashMap<>();
    } else {
      deviceDataEntity = deviceDataEntityList.get(0);
      attrJsonMap = deviceDataEntity.getAttributesMap();
    }

    String existingHeartBeatPeriod = attrJsonMap.get(VesAgentConstants.HEART_BEAT_PERIOD);

    if (null == heartBeatPeriod
        && (Boolean.TRUE.equals(VesAgentUtils.isNullOrEmpty(existingHeartBeatPeriod))
            || existingHeartBeatPeriod
                .equalsIgnoreCase(VesAgentConstants.REMOVE_HEART_BEAT_TIMER_VAL))) {
      return false;
    }
    if (null != countDownTimer
        && !countDownTimer.equalsIgnoreCase(VesAgentConstants.COUNT_DOWN_TIMER_ZERO)) {
      if (null == heartBeatPeriod || heartBeatPeriod.equalsIgnoreCase(existingHeartBeatPeriod)) {
        String exceptionReason = "Can't change timer value if heartbeat value is same";
        throw new VesAgentException(VesAgentConstants.INVALID_PARAMETER_VALUE, exceptionReason);
      }

    }

    if (!VesAgentUtils.isNullOrEmpty(heartBeatPeriod)) {
      attrJsonMap.put(VesAgentConstants.HEART_BEAT_PERIOD, heartBeatPeriod);
    }

    deviceDataEntity.setAttributesMap(attrJsonMap);

    vesDataRepository.save(deviceDataEntity);

    return true;
  }

  public void processHeartBeatSetRequest(DeviceRPCRequest deviceRPCRequest, String heartBeatPeriod,
      String countDownTimer) throws VesAgentException {

    String deviceId = deviceRPCRequest.getDeviceDetails().getDeviceId();

    VesAgentUtils.validateDeviceId(deviceId);

    if (VesAgentUtils.isNullOrEmpty(heartBeatPeriod)
        && VesAgentUtils.isNullOrEmpty(countDownTimer)) {
      String errorMsg =
          "Invalid input: HeartBeatPeriod and countDownTimer both are null for device " + deviceId;
      errorMsg = errorMsg.replaceAll("[\n|\r|\t]", "_");
      logger.error(errorMsg);
      throw new VesAgentException(VesAgentConstants.INVALID_PARAMETER_VALUE, errorMsg);
    }

    Object eNodeBNameObj = deviceRPCRequest.getContext().get(VesAgentConstants.ENODEB_NAME);

    String eNodeBName = null;
    if (null != eNodeBNameObj) {
      eNodeBName = (String) eNodeBNameObj;
    }

    boolean resetTimerJob = saveDeviceDataEntity(deviceRPCRequest.getDeviceDetails(), eNodeBName,
        heartBeatPeriod, countDownTimer);

    if (resetTimerJob) {
      resetTimerJob(deviceId, heartBeatPeriod, countDownTimer);
      abortRunningDeviceConnectivityCheck(deviceRPCRequest);
    }

  }

  public void processHeartBeatGetRequest(DeviceRPCRequest deviceRPCRequest) {

    String deviceId = deviceRPCRequest.getDeviceDetails().getDeviceId();
    List<DeviceDataEntity> deviceDataEntityList =
        vesDataRepository.findByDeviceIdAndAttrGroup(deviceId, VesAgentConstants.HEART_BEAT);

    if (VesAgentUtils.isNullOrEmpty(deviceDataEntityList)
        || VesAgentUtils.isNullOrEmpty(deviceDataEntityList.get(0).getAttributesMap())) {
      return;
    }

    DeviceDataEntity deviceDataEntity = deviceDataEntityList.get(0);

    List<ParameterDTO> resultparamDTOList = null;
    List<ParameterDTO> paramDTOList = deviceRPCRequest.getOpDetails().getParmeters();

    for (ParameterDTO paramDTO : paramDTOList) {
      resultparamDTOList = ifDataTypeObject(paramDTO, deviceDataEntity);

      if (!resultparamDTOList.isEmpty()) {
        break;
      }

      if (paramDTO.getParamName().equalsIgnoreCase(VesAgentConstants.COUNT_DOWN_TIMER)) {
        paramDTO.setParamValue(getCountDownTimerParam(deviceDataEntity).getParamValue());
      } else {
        paramDTO.setParamValue(deviceDataEntity.getAttributesMap().get(paramDTO.getParamName()));
      }
    }

    if (null != resultparamDTOList && !resultparamDTOList.isEmpty()) {
      deviceRPCRequest.getOpDetails().setParmeters(resultparamDTOList);
    }
  }

  public void processHeartBeatDeleteRequest(VESNotification vesNotification) {
    List<ParameterDTO> paramDTOList = vesNotification.getOperationDetails().getParmeters();

    for (ParameterDTO paramDTO : paramDTOList) {
      if (Boolean.TRUE.equals(VesAgentUtils.isVesNotificationRequest(paramDTO))) {
        List<DeviceDataEntity> deviceDataEntityList = vesDataRepository.findByDeviceIdAndAttrGroup(
            vesNotification.geteNodeBName(), VesAgentConstants.HEART_BEAT);

        if (Boolean.TRUE.equals(VesAgentUtils.isNullOrEmpty(deviceDataEntityList))) {
          return;
        }
        vesDataRepository.delete(deviceDataEntityList.get(0));
        timerService.cancelSchedule(vesNotification.geteNodeBName());
        break;
      }
    }
  }

  private List<ParameterDTO> ifDataTypeObject(ParameterDTO paramDTO,
      DeviceDataEntity deviceDataEntity) {
    List<ParameterDTO> paramDTOList = new ArrayList<>();

    if (null != paramDTO.getDataType()
        && paramDTO.getDataType().equalsIgnoreCase(VesAgentConstants.OBJECT_DATA_TYPE.toLowerCase())
        && paramDTO.getParamName().toLowerCase()
            .contains(VesAgentConstants.HEART_BEAT.toLowerCase())) {

      Map<String, String> attrMap = deviceDataEntity.getAttributesMap();

      for (Map.Entry<String, String> entry : attrMap.entrySet()) {
        ParameterDTO param = new ParameterDTO();
        param.setParamName(entry.getKey());
        param.setParamValue(entry.getValue());

        paramDTOList.add(param);
      }

      ParameterDTO countDownParam = getCountDownTimerParam(deviceDataEntity);
      paramDTOList.add(countDownParam);
    }

    return paramDTOList;
  }

  private ParameterDTO getCountDownTimerParam(DeviceDataEntity deviceDataEntity) {
    Long countDownTimerVal = timerService
        .getTimeRemainingTillNextExecution(deviceDataEntity.getDeviceId(), TimeUnit.MINUTES);

    ParameterDTO param = new ParameterDTO();
    param.setParamName(VesAgentConstants.COUNT_DOWN_TIMER);

    if (null != countDownTimerVal) {
      param.setParamValue(countDownTimerVal.toString());
    }

    return param;
  }


  public void processHeartBeatGetRequest(String deviceId, Integer heartBeatPeriod,
      Integer countDownTimer) throws VesAgentException {
    VesAgentUtils.validateDeviceId(deviceId);


    if (null == heartBeatPeriod && null == countDownTimer) {// this should just check if heartbeat
                                                            // is null
      String errorMsg =
          "Invalid input: HeartBeatPeriod and countDownTimer both are null for device " + deviceId;
      logger.error(errorMsg);
      throw new VesAgentException(errorMsg);
    }

    List<DeviceDataEntity> deviceDataEntityList =
        vesDataRepository.findByDeviceIdAndAttrGroup(deviceId, VesAgentConstants.HEART_BEAT);

    DeviceDataEntity deviceDataEntity = null;
    Map<String, String> attrJsonMap = null;

    if (null == deviceDataEntityList || deviceDataEntityList.isEmpty()) {
      deviceDataEntity = new DeviceDataEntity();
      deviceDataEntity.setDeviceId(deviceId);
      deviceDataEntity.setAttrGroup(VesAgentConstants.HEART_BEAT);

      attrJsonMap = new HashMap<>();
    } else {
      deviceDataEntity = deviceDataEntityList.get(0);
      attrJsonMap = new Gson().fromJson(deviceDataEntity.getAttrJson(), Map.class);
    }


    if (null != heartBeatPeriod) {
      attrJsonMap.put(VesAgentConstants.HEART_BEAT_PERIOD, heartBeatPeriod.toString());
    }

    if (null != countDownTimer) {
      attrJsonMap.put(VesAgentConstants.COUNT_DOWN_TIMER, countDownTimer.toString());
    }

    String attrJson = new Gson().toJson(attrJsonMap);
    deviceDataEntity.setAttrJson(attrJson);

    vesDataRepository.save(deviceDataEntity);
  }

  private void resetTimerJob(String deviceId, String heartBeatPeriod, String countDownTimer) {
    if (null == heartBeatPeriod || heartBeatPeriod.isEmpty()) {
      scheduleTimerJob(deviceId, Integer.parseInt(countDownTimer));
    } else if (heartBeatPeriod.equals(VesAgentConstants.REMOVE_HEART_BEAT_TIMER_VAL)) {
      timerService.cancelSchedule(deviceId);
    } else {
      if (Boolean.FALSE.equals(VesAgentUtils.isNullOrEmpty(countDownTimer))) {
        scheduleTimerJob(deviceId, Integer.parseInt(countDownTimer));
      } else {
        scheduleTimerJob(deviceId, Integer.parseInt(heartBeatPeriod));
      }
    }
  }

  private void scheduleTimerJob(String deviceId, Integer timeoutInterval) {
    ScheduleInfo scheduleInfo = new ScheduleInfo();
    scheduleInfo.setInterval(timeoutInterval);
    scheduleInfo.setTimeUnit(TimeUnit.MINUTES);

    HeartBeatTimeoutTask callbackTask = getBeanInstance(deviceId);

    timerService.schedule(deviceId, scheduleInfo, callbackTask);
  }

  private void abortRunningDeviceConnectivityCheck(DeviceRPCRequest deviceRPCRequest) {
    waitForNotifications.notifyResult(VesAgentUtils.getErrorResponse(deviceRPCRequest, null, null));
  }

  public List<DeviceDataEntity> getAllDeviceDataEntity() {
    return (List<DeviceDataEntity>) vesDataRepository.findAll();
  }

  public List<DeviceDataEntity> findByDeviceIdAndGroup(String deviceId, String attrGroup) {
    return vesDataRepository.findByDeviceIdAndAttrGroup(deviceId, attrGroup);
  }



}
