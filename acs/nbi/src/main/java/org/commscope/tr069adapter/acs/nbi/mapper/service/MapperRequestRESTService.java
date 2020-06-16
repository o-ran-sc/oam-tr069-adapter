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

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.MAPPER_SERVICE_QUALILFIER;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.exception.MapperServiceException;
import org.commscope.tr069adapter.acs.common.mapper.ACSServiceAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/MapperService")
public class MapperRequestRESTService {

  private static final Logger logger = LoggerFactory.getLogger(MapperRequestRESTService.class);

  @Qualifier(value = MAPPER_SERVICE_QUALILFIER)
  @Autowired
  ACSServiceAPI acsServiceAPI;

  @PostMapping("/initiateDeviceOperation")
  public Long initiateDeviceOperation(@RequestBody DeviceRPCRequest deviceRPCRequest) {
    logger.debug("Received a Device operation request from Mapper");
    Long operationId = 0l;
    try {
      operationId = acsServiceAPI.performDeviceOperation(deviceRPCRequest);
      logger.debug(
          "Successfully initiated device operation, The operation id for the request is: {}",
          operationId);
    } catch (MapperServiceException e) {
      logger.error("An exception occurred while calling the device operation, Reason: {}",
          e.getMessage());
    }

    return operationId;
  }

}
