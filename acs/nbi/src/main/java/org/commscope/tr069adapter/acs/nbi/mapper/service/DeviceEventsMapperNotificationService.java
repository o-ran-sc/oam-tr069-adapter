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

package org.commscope.tr069adapter.acs.nbi.mapper.service;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.nbi.util.MapperSrvcDependencyConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class DeviceEventsMapperNotificationService {

  private static final Logger logger =
      LoggerFactory.getLogger(DeviceEventsMapperNotificationService.class);

  @Autowired
  RestTemplate restTemplate;

  @Autowired
  MapperSrvcDependencyConfig mapperSrvcDependencyConfig;

  public void processOperationResponse(DeviceRPCResponse opResult) {
    logger.debug("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
    logger.debug("Received Operation result from NBI");
    logger.debug("Posting the operation request to WebService: {}",
        mapperSrvcDependencyConfig.getMapperDeviceRPCResponseNotificationServiceURL());
    try {
      restTemplate.postForEntity(
          mapperSrvcDependencyConfig.getMapperDeviceRPCResponseNotificationServiceURL(), opResult,
          ResponseEntity.class);
      logger.debug("Successfully posted the NBI operation request to SBI Service");
    } catch (Exception e) {
      logger.error("Unable to post the operation result, Reason: {}", e.getMessage());
    }
    logger.debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++");

  }

  public void processDeviceNotification(DeviceInform notification) {
    logger.debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++");
    logger.debug("Received Notification from NBI");
    logger.debug("Posting the Device Notification to WebService: {}",
        mapperSrvcDependencyConfig.getMapperInformNotificationServiceURL());
    try {
      restTemplate.postForEntity(mapperSrvcDependencyConfig.getMapperInformNotificationServiceURL(),
          notification, ResponseEntity.class);
      logger.debug("Successfully posted the Device Notification to SBI Service");
    } catch (Exception e) {
      logger.error("Unable to post the Device Notification, Reason: {}", e.getMessage());
    }
    logger.debug("+++++++++++++++++++++++++++++++++++++++++++++++++++++");
  }

}
