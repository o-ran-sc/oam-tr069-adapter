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

package org.commscope.tr069adapter.acs.requestprocessor.impl;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.exception.DeviceOperationException;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRepository;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceEntity;
import org.commscope.tr069adapter.acs.requestprocessor.util.TR069NBIUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
public class DeviceOperationManager implements DeviceOperationInterface {

  private static final Logger logger = LoggerFactory.getLogger(DeviceOperationManager.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private DeviceRepository deviceRepository;

  /*
   * (non-Javadoc)
   * 
   * @see org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface#getDeviceDetails(
   * java.lang.String)
   */
  @Override
  @Transactional(isolation = Isolation.DEFAULT, propagation = Propagation.REQUIRED, timeout = 300,
      rollbackFor = RuntimeException.class)
  public TR069DeviceDetails getDeviceDetails(String deviceId) throws DeviceOperationException {
    TR069DeviceDetails tr069DeviceDetails = null;
    TR069DeviceEntity tr069DeviceEntity = deviceRepository.findByDeviceId(deviceId);
    if (tr069DeviceEntity == null) {
      throw new DeviceOperationException(ErrorCode.DEVICE_NOT_EXISTS, deviceId);
    }
    tr069DeviceDetails = TR069NBIUtility.convertToDTO(tr069DeviceEntity);


    return tr069DeviceDetails;
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface#updateDeviceDetails(
   * org.commscope.tr069adapter.acs.DeviceDetails)
   */
  @Override
  @Transactional(isolation = Isolation.DEFAULT, propagation = Propagation.REQUIRED, timeout = 300,
      rollbackFor = RuntimeException.class)
  public void updateDeviceDetails(DeviceDetails deviceDetails) throws DeviceOperationException {
    try {
      if (deviceDetails != null && deviceDetails.getDeviceId() != null
          && deviceDetails instanceof TR069DeviceDetails) {
        TR069DeviceDetails tr069DeviceDetails = (TR069DeviceDetails) deviceDetails;
        String deviceId = tr069DeviceDetails.getDeviceId();
        MDC.put(CLIENT_STR, deviceId);
        logger.debug("Updating the TR069 device: {}", deviceId);

        TR069DeviceEntity tr069DeviceEntity =
            deviceRepository.findByDeviceId(tr069DeviceDetails.getDeviceId());

        TR069DeviceEntity updatedEntity = TR069NBIUtility.convertToEntity(tr069DeviceDetails);
        if (tr069DeviceEntity != null)
          updatedEntity.setId(tr069DeviceEntity.getId());

        deviceRepository.save(updatedEntity);

      } else {
        logger.error(
            "The device cannot be null or should be of type TR069DeviceDetails, wrong request for device un-register");
        throw new DeviceOperationException(
            "Invalid device update request, device should be of type TR069DeviceDetails");
      }
      logger.debug("Successfully updated the device");
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }

}
