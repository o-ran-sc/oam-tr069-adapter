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

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CR_TIMEOUT_CALLBACK;

import java.io.Serializable;

import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.cpe.processor.ConnectionReqEventProcessor;
import org.commscope.tr069adapter.common.timer.TimerListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component(CR_TIMEOUT_CALLBACK)
public class ConnectionReqTimeoutHandler implements TimerListener {

  private static final Logger logger = LoggerFactory.getLogger(ConnectionReqTimeoutHandler.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private ConnectionReqEventProcessor connectionReqEventProcessor;

  @Override
  public void notifyTimeout(String timerId, Serializable data) {
    TR069DeviceDetails deviceDetails = (TR069DeviceDetails) data;
    MDC.put(CLIENT_STR, deviceDetails.getDeviceId());
    logger.debug(
        "Connection request initiation has timed out, where as device has not sent INFORM event");

    try {
      connectionReqEventProcessor.initiateConnectionRequest(deviceDetails, true);
      logger.debug("Successfully initiated Connection request on device");
    } catch (Exception e) {
      logger.error("Couldn't initiate connection request, an error occurred: {}", e.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }

}
