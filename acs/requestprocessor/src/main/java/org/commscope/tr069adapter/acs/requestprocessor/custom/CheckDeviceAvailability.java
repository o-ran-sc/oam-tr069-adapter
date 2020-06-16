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

package org.commscope.tr069adapter.acs.requestprocessor.custom;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepositoryHelper;
import org.commscope.tr069adapter.acs.requestprocessor.dto.CustomOperationData;
import org.commscope.tr069adapter.acs.requestprocessor.impl.TR069EventNotificationService;
import org.commscope.tr069adapter.acs.requestprocessor.impl.TR069RequestProcessEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("CheckDeviceAvailability")
public class CheckDeviceAvailability implements CustomOperation {

  private static final Logger logger = LoggerFactory.getLogger(CheckDeviceAvailability.class);

  @Autowired
  protected DeviceRPCRequestRepositoryHelper deviceRPCRequestRepositoryHelper;

  @Autowired
  TR069RequestProcessEngine tr069RequestProcessEngine;

  @Autowired
  TR069EventNotificationService eventNotificationService;

  @Override
  public CustomOperationData executeCustomLogic(CustomOperationData customOperationData)
      throws TR069EventProcessingException {

    TR069DeviceDetails deviceDetails = customOperationData.getDeviceDetails();
    DeviceRPCRequest deviceRPCRequest = customOperationData.getDeviceRPCRequest();

    logger.debug("Started processing Check device availability");

    DeviceRPCResponse deviceRPCResponse = new DeviceRPCResponse();
    OperationResponse operationResponse = new OperationResponse();
    operationResponse.setOperationCode(TR069OperationCode.INITIATE_CR);
    Long operationId = deviceRPCRequest.getOperationId();
    deviceRPCResponse.setDeviceDetails(deviceDetails);
    deviceRPCResponse.setOperationId(operationId);
    deviceRPCResponse.setOperationResponse(operationResponse);

    logger.debug("Marking as processed the corresponding NBI Operation request record");
    deviceRPCRequestRepositoryHelper.markDeviceRPCRequestAsProcessed(deviceDetails.getDeviceId(),
        operationId);

    logger.debug("Sending the operation response for check device availability");
    // Sending the operation response to NBI
    eventNotificationService.sendOperationResultToNBI(deviceRPCResponse);
    customOperationData.setDeviceRPCResponse(null);
    customOperationData.setDeviceRPCRequest(null);

    logger.debug("Stopping request timer if any running for operation ID : {}", operationId);
    tr069RequestProcessEngine.stopDeviceRPCRequestTimer(deviceDetails.getDeviceId(),
        deviceRPCRequest.getOperationId());
    logger.debug("Finished processing Check device availability");
    return customOperationData;
  }

}
