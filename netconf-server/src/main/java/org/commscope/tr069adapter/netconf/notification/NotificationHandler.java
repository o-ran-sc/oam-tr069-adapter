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
import org.commscope.tr069adapter.netconf.server.utils.NetConfServerConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Component;

@Component
public class NotificationHandler {

  private static final Logger logger = LoggerFactory.getLogger(NotificationHandler.class);
  private static final String CLIENT_STR = "client";

  @Autowired
  NetConfSessionUtil netConfSessionUtil;

  @Autowired
  private JmsTemplate jmsTemplate;

  public void handleNetConfNotification(NetConfNotificationDTO netConNotifDTO) {
    logger.debug("processing netconf notification {}", netConNotifDTO);
    try {
      MDC.put(CLIENT_STR, netConNotifDTO.getDeviceID());

      logger.debug("NetConf notificaiton reviced for {}", netConNotifDTO.getDeviceID());
      jmsTemplate.convertAndSend(NetConfServerConstants.NETCONF_NOTIFICATION_Q, netConNotifDTO);
      logger.debug("Successfully posted the notiticaiton to JMS to forward to SDNR");
    } catch (Exception e) {
      logger.error("Posting notification failed; Reason: {}", e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }
}
