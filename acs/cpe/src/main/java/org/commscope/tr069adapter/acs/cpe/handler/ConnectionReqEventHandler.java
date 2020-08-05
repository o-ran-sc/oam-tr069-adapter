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


package org.commscope.tr069adapter.acs.cpe.handler;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CR_REQ_CF;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CR_REQ_Q;
import java.io.IOException;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.cpe.processor.ConnectionReqEventProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class ConnectionReqEventHandler {

  private static final Logger logger = LoggerFactory.getLogger(ConnectionReqEventHandler.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private ConnectionReqEventProcessor connectionReqEventProcessor;

  @JmsListener(destination = CR_REQ_Q, containerFactory = CR_REQ_CF)
  @Transactional(rollbackFor = Exception.class)
  public void onMessage(TR069DeviceDetails tr069DeviceDetails)
      throws SessionManagerException, IOException {
    try {
      if (tr069DeviceDetails != null) {
        MDC.put(CLIENT_STR, tr069DeviceDetails.getDeviceId());
        logger.info(
            "Received a JMS message from Request Processor for initiating connection request");
        connectionReqEventProcessor.initiateConnectionRequest(tr069DeviceDetails, false);
        logger.debug("Sent TR069 connection request to device");
      } else {
        logger.warn(
            "Received a JMS message for initiating connection request with no device details, "
                + "hence could not initiate connection request");
      }
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }
}
