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

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.NBI_NOTIFICATION_Q;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.NBI_OP_RESULT_Q;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.dto.CustomOperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Component;

@Component
public class TR069EventNotificationService {

  private static final Logger logger = LoggerFactory.getLogger(TR069EventNotificationService.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private JmsTemplate jmsTemplate;

  /**
   * @param deviceNotification
   */
  public void sendDeviceInformToNBI(DeviceInform deviceNotification) {
    String deviceId = deviceNotification.getDeviceDetails().getDeviceId();
    try {
      MDC.put(CLIENT_STR, deviceId);
      TR069InformType notificationType = (TR069InformType) deviceNotification.getInformType();

      logger.debug("Device Inform Event received: '{}'", notificationType.getNotificationCode());
      jmsTemplate.convertAndSend(NBI_NOTIFICATION_Q, deviceNotification);
      logger.debug("Successfully posted the device inform event to DM to forward to NBI");
    } catch (Exception e) {
      logger.error("Posting Device Inform event to mapper failed, Reason: {}", e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }

  /**
   * @param deviceRPCResponse
   */
  public void sendOperationResultToNBI(DeviceRPCResponse deviceRPCResponse) {
    String deviceId = deviceRPCResponse.getDeviceDetails().getDeviceId();
    try {
      MDC.put(CLIENT_STR, deviceId);
      if (deviceRPCResponse.getOperationResponse()
          .getOperationCode() instanceof TR069OperationCode) {
        TR069OperationCode operCode =
            (TR069OperationCode) deviceRPCResponse.getOperationResponse().getOperationCode();
        logger.debug("Device RPC Response received for operation: '" + operCode.name()
            + "' with operation ID:" + deviceRPCResponse.getOperationId());
      } else if (deviceRPCResponse.getOperationResponse()
          .getOperationCode() instanceof CustomOperationCode) {
        CustomOperationCode operCode =
            (CustomOperationCode) deviceRPCResponse.getOperationResponse().getOperationCode();
        logger.debug("Device RPC Response received for operation: '" + operCode.getJndiName()
            + "' with operation ID:" + deviceRPCResponse.getOperationId());
      }
      jmsTemplate.convertAndSend(NBI_OP_RESULT_Q, deviceRPCResponse);
      logger.debug("Successfully posted the operation result event to DM to forward to NBI");
    } catch (Exception e) {
      logger.error("Posting Device RPC response event to mapper failed, Reason: {}",
          e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }

}
