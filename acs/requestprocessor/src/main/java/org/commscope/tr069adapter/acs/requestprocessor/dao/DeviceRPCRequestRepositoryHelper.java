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

package org.commscope.tr069adapter.acs.requestprocessor.dao;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceRPCRequestEntity;
import org.commscope.tr069adapter.acs.requestprocessor.util.TR069RequestProcessorUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DeviceRPCRequestRepositoryHelper {

  private static final Logger logger =
      LoggerFactory.getLogger(DeviceRPCRequestRepositoryHelper.class);

  @Autowired
  protected DeviceRPCRequestRepository deviceRPCRequestRepository;

  /**
   * @param tr069DeviceRPCRequestEntities
   */
  public void saveAll(List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntities) {
    deviceRPCRequestRepository.saveAll(tr069DeviceRPCRequestEntities);
  }

  /**
   * @param deviceId
   * @return
   */
  public List<TR069DeviceRPCRequestEntity> findByDeviceId(String deviceId) {
    List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntities =
        deviceRPCRequestRepository.findByDeviceId(deviceId);
    Collections.sort(deviceRPCRequestEntities, idComparator);
    return deviceRPCRequestEntities;
  }

  /**
   * @param deviceId
   * @param operationId
   * @return
   */
  public List<TR069DeviceRPCRequestEntity> findByDeviceIdAndOperationId(String deviceId,
      Long operationId) {
    List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntities =
        deviceRPCRequestRepository.findByDeviceIdAndOperationId(deviceId, operationId);
    Collections.sort(deviceRPCRequestEntities, idComparator);
    return deviceRPCRequestEntities;
  }

  /**
   * @param deviceId
   * @return
   * @throws TR069EventProcessingException
   */
  public List<DeviceRPCRequest> findAllDeviceRPCRequests(String deviceId) {
    logger.debug("Retrieving all the pending Device RPC requests");
    List<DeviceRPCRequest> deviceRPCRequestList = new ArrayList<>();
    List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntities =
        deviceRPCRequestRepository.findByDeviceIdAndIsProcessed(deviceId, 0);
    if (deviceRPCRequestEntities == null) {
      logger.debug("There exists no pending RPC requests for the device: {}", deviceId);
      return deviceRPCRequestList;
    }

    logger.debug("Grouping the records based on operationId");
    Map<Long, List<TR069DeviceRPCRequestEntity>> opIdEntityMap = new HashMap<>();
    for (TR069DeviceRPCRequestEntity entity : deviceRPCRequestEntities) {
      List<TR069DeviceRPCRequestEntity> entityList = opIdEntityMap.get(entity.getOperationId());
      if (entityList == null) {
        entityList = new ArrayList<>();
        opIdEntityMap.put(entity.getOperationId(), entityList);
      }
      entityList.add(entity);
    }

    logger.debug("There exists {} pending NBI requests for the device", opIdEntityMap.size());
    Iterator<Long> opIdIterator = opIdEntityMap.keySet().iterator();
    while (opIdIterator.hasNext()) {
      Long operationId = opIdIterator.next();
      List<TR069DeviceRPCRequestEntity> entityList = opIdEntityMap.get(operationId);
      Collections.sort(entityList, idComparator);
      deviceRPCRequestList.add(TR069RequestProcessorUtility.convertToDTO(entityList));
    }

    return deviceRPCRequestList;
  }

  /**
   * @param deviceId
   * @return
   * @throws TR069EventProcessingException
   */

  public DeviceRPCRequest findOldestDeviceRPCRequest(String deviceId) {
    logger.debug("Retrieving the oldest pending Device RPC request");

    List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntities =
        deviceRPCRequestRepository.findByDeviceIdAndIsProcessed(deviceId, 0);
    if (deviceRPCRequestEntities == null || deviceRPCRequestEntities.isEmpty()) {
      logger.debug("There exists no pending Device RPC requests for the device: {}", deviceId);
      return null;
    }

    List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntityList =
        getOldestDeviceRPCRequestList(deviceRPCRequestEntities);

    if (deviceRPCRequestEntityList != null) {
      Collections.sort(deviceRPCRequestEntityList, idComparator);
      return TR069RequestProcessorUtility.convertToDTO(deviceRPCRequestEntityList);
    } else {
      logger.debug("Empty deviceRPCRequestEntityList");
      return null;
    }

  }

  private List<TR069DeviceRPCRequestEntity> getOldestDeviceRPCRequestList(
      List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntities) {
    List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntityList = null;

    logger.debug("Grouping the records based on operationId");
    Map<Long, List<TR069DeviceRPCRequestEntity>> opIdEntityMap = new HashMap<>();
    for (TR069DeviceRPCRequestEntity entity : deviceRPCRequestEntities) {
      List<TR069DeviceRPCRequestEntity> entityList = opIdEntityMap.get(entity.getOperationId());
      if (entityList == null) {
        entityList = new ArrayList<>();
        opIdEntityMap.put(entity.getOperationId(), entityList);
      }
      entityList.add(entity);
    }

    Iterator<Long> opIdIterator = opIdEntityMap.keySet().iterator();
    while (opIdIterator.hasNext()) {
      Long operationId = opIdIterator.next();
      List<TR069DeviceRPCRequestEntity> entityList = opIdEntityMap.get(operationId);
      TR069DeviceRPCRequestEntity entity = entityList.get(0);
      if (deviceRPCRequestEntityList == null) {
        deviceRPCRequestEntityList = entityList;
      } else {
        if (entity.getCreateTime().before(deviceRPCRequestEntityList.get(0).getCreateTime()))
          deviceRPCRequestEntityList = entityList;
      }
    }

    return deviceRPCRequestEntityList;
  }

  /**
   * @param operationId
   * @throws TR069EventProcessingException
   */
  public void markDeviceRPCRequestAsProcessed(String deviceId, Long operationId)
      throws TR069EventProcessingException {
    logger.debug("Marking the Device RPC request for device: {} and operationId: {} as processed",
        deviceId, operationId);
    List<TR069DeviceRPCRequestEntity> deviceRPCRequestEntityList =
        deviceRPCRequestRepository.findByDeviceIdAndOperationId(deviceId, operationId);
    if (deviceRPCRequestEntityList == null) {
      TR069EventProcessingException ex =
          new TR069EventProcessingException(ErrorCode.MISSING_OPERATION_DETAILS);
      logger.error(ex.getMessage());
      throw ex;
    }

    for (TR069DeviceRPCRequestEntity deviceRPCRequestEntity : deviceRPCRequestEntityList) {
      deviceRPCRequestEntity.setIsProcessed(Integer.valueOf(1));
    }
    deviceRPCRequestRepository.saveAll(deviceRPCRequestEntityList);
  }


  public static final Comparator<TR069DeviceRPCRequestEntity> idComparator =
      (TR069DeviceRPCRequestEntity e1, TR069DeviceRPCRequestEntity e2) -> e1.getId()
          .compareTo(e2.getId());

}
