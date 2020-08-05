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
import org.commscope.tr069adapter.netconf.rpc.CreateSubscription;
import org.opendaylight.netconf.api.NetconfMessage;
import org.opendaylight.netconf.api.xml.XmlUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class NetConfSessionUtil {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfSessionUtil.class);

  public void sendNetConfNotification(NetConfNotificationDTO netConNotifDTO)
      throws NetconfNotificationException {
    NetconfMessage netconfMessage = convertToNetConfMessage(netConNotifDTO);
    CreateSubscription.sendNotification(netconfMessage, netConNotifDTO.getDeviceID());
  }

  private NetconfMessage convertToNetConfMessage(NetConfNotificationDTO netConNotifDTO)
      throws NetconfNotificationException {
    try {
      return new NetconfMessage(XmlUtil.readXmlToDocument(netConNotifDTO.getNotificaiton()));
    } catch (Exception e) {
      LOG.error("Error while converting to netcon notification ");
      throw new NetconfNotificationException("Cannot parse notifications", e);
    }
  }
}
