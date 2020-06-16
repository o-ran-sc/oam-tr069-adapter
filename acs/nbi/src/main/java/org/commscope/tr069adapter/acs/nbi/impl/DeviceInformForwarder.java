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

package org.commscope.tr069adapter.acs.nbi.impl;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.NBI_NOTIFICATION_CF;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.NBI_NOTIFICATION_Q;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.nbi.mapper.service.DeviceEventsMapperNotificationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class DeviceInformForwarder {

  private static final Logger logger = LoggerFactory.getLogger(DeviceInformForwarder.class);

  @Autowired
  private DeviceEventsMapperNotificationService deviceEventsMapperNotificationService;

  @JmsListener(destination = NBI_NOTIFICATION_Q, containerFactory = NBI_NOTIFICATION_CF)
  @Transactional(rollbackFor = Exception.class)
  public void onMessage(DeviceInform notification) {
    try {
      if (null != notification) {
        logger.debug(
            "DeviceNotification message is received for deviceId : {} , Notification Type(s): {}",
            notification.getDeviceDetails().getDeviceId(), notification.getInformTypeList());
        deviceEventsMapperNotificationService.processDeviceNotification(notification);
        logger.debug("Successfully processed device notification.");
      } else {
        logger.error("Null device response is received!!!");
      }
    } catch (Exception e) {
      logger.error("Error while processing the notification, Reason: {}", e.getMessage());
    }
  }

}
