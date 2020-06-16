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

package org.commscope.tr069adapter.mapper.acs.controller;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.mapper.acs.ACSNotificationHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/tr069MapperSBI")
public class ACSNotificationReceiver {

  private static final Logger LOG = LoggerFactory.getLogger(ACSNotificationReceiver.class);

  @Autowired
  ACSNotificationHandler handler;

  @PostMapping("/opResult")
  public void processOpResult(@RequestBody DeviceRPCResponse opResult) {
    LOG.debug("Received Operation result : {}", opResult);
    handler.handleOperationResponse(opResult);
    LOG.debug("Processed Operation result for opId : {}", opResult.getOperationId());
  }

  @PostMapping("/notification")
  public void processNotification(@RequestBody DeviceInform notification) {
    LOG.debug("Received device notification : {}", notification);
    handler.handleNotification(notification);
    LOG.debug("Processed device notification : {}", notification.getInformType());
  }

}
