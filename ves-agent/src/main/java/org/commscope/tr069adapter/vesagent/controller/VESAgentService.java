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

package org.commscope.tr069adapter.vesagent.controller;

import com.fasterxml.jackson.core.JsonProcessingException;

import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.acs.common.inform.PeriodicInform;
import org.commscope.tr069adapter.acs.common.inform.ValueChangeInform;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.vesagent.exception.InvalidFaultOperationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(path = "/vesagent")
public class VESAgentService {
  private static final Logger logger = LoggerFactory.getLogger(VESAgentService.class);

  @Autowired
  PnfRegMappingHandler pnfHandler;

  @Autowired
  AlarmMappingHandler alarmHanler;

  @Autowired
  HeartBeatMessageHandler hbHandler;

  @PostMapping(path = "/deviceEvent", consumes = "application/json")
  public VESNotificationResponse processDeviceNotificationAsVESEvent(
      @RequestBody VESNotification notification)
      throws JsonProcessingException, InvalidFaultOperationException {
    VESNotificationResponse response = null;
    logger.debug("VES Notification request processing started");
    if (notification.getDevnotification() == null) {
      logger.debug("VES Notification request PnfRegister on container restart processing started");
      response = pnfHandler.handlePnfRegNotificationOnRestart(notification);
    } else if (notification.getDevnotification() instanceof BootstrapInform) {
      logger.debug("VES Notification request PnfRegister processing started");
      response = pnfHandler.handlePnfRegNotification(notification);
      logger.debug("VES Notification request PnfRegister processing completed");
    } else if (notification.getDevnotification() instanceof ValueChangeInform) {
      logger.debug("VES Notification request Fault processing started");
      response = alarmHanler.handleAlarmNotification(notification);
      logger.debug("VES Notification request Fault processing completed");
    } else if (notification.getDevnotification() instanceof PeriodicInform) {
      logger.debug("VES Notification request PI processing started");
      response = hbHandler.handlePINotification(notification);
      logger.debug("VES Notification request PI processing completed");
    } else {
      logger.error("VES Notification request is unknown");
      response =
          new VESNotificationResponse(HttpStatus.BAD_REQUEST.value(), "Method not supported");
    }
    logger.debug("VES Notification request processing completed");
    return response;
  }
}
