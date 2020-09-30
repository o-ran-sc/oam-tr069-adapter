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

package org.commscope.tr069adapter.netconf.notification;

import org.commscope.tr069adapter.mapper.model.NetConfNotificationDTO;
import org.commscope.tr069adapter.netconf.error.NetconfNotificationException;
import org.commscope.tr069adapter.netconf.server.utils.NetConfServerConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class NetConfNotificationQueue {

  private static final Logger logger = LoggerFactory.getLogger(NetConfNotificationQueue.class);

  @Autowired
  NetConfSessionUtil netConfSessionUtil;

  @JmsListener(destination = NetConfServerConstants.NETCONF_NOTIFICATION_Q,
      containerFactory = NetConfServerConstants.NETCONF_NOTIFICATION_CF)
  @Transactional(rollbackFor = NetconfNotificationException.class)
  public void onMessage(NetConfNotificationDTO netConNotifDTO) throws NetconfNotificationException {
    if (null != netConNotifDTO) {
      logger.debug("Netconf notification is received for deviceId : {} ",
          netConNotifDTO.getDeviceID());
      netConfSessionUtil.sendNetConfNotification(netConNotifDTO);
      logger.debug("Successfully processed device notification.");
    } else {
      logger.error("Null device response is received!!!");
    }
  }
}
