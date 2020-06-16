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


package org.commscope.tr069adapter.acs.requestprocessor.handler;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.TR069_NBI_REQUEST_CF;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.TR069_NBI_REQUEST_Q;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.exception.SessionConcurrentAccessException;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.requestprocessor.impl.TR069RequestProcessEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
public class DeviceRPCRequestHandler {

  private static final Logger logger = LoggerFactory.getLogger(DeviceRPCRequestHandler.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private TR069RequestProcessEngine tr069RequestProcessEngine;

  @JmsListener(destination = TR069_NBI_REQUEST_Q, containerFactory = TR069_NBI_REQUEST_CF)
  @Transactional(rollbackFor = Exception.class)
  public void onMessage(DeviceRPCRequest mapperDeviceOperationRequest)
      throws SessionConcurrentAccessException {
    logger.debug("Received a JMS message from Mapper for TR069 Device RPC operation");
    try {
      if (mapperDeviceOperationRequest != null) {
        MDC.put(CLIENT_STR, mapperDeviceOperationRequest.getDeviceDetails().getDeviceId());
        logger.debug("Received a TR069 operation request from Mapper with operation ID: {} ",
            mapperDeviceOperationRequest.getOperationId());
        tr069RequestProcessEngine.processDeviceRPCRequest(mapperDeviceOperationRequest);
      }
    } catch (TR069EventProcessingException ex) {
      logger.error(ex.getMessage());
    } finally {
      MDC.remove(CLIENT_STR);
    }
  }

}
