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

package org.commscope.tr069adapter.acs.nbi.impl;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.MAPPER_SERVICE_QUALILFIER;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.TR069_NBI_REQUEST_Q;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.exception.MapperServiceException;
import org.commscope.tr069adapter.acs.common.mapper.ACSServiceAPI;
import org.commscope.tr069adapter.acs.common.utils.OperationIdGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Component;

@Component(MAPPER_SERVICE_QUALILFIER)
public class ACSServiceAPIImpl implements ACSServiceAPI {

  private static final Logger logger = LoggerFactory.getLogger(ACSServiceAPIImpl.class);

  private static final String CLIENT_STR = "client";

  @Autowired
  private OperationIdGenerator opIdGenerator;

  @Autowired
  private JmsTemplate jmsTemplate;

  /*
   * (non-Javadoc)
   * 
   * @see org.commscope.tr069adapter.acs.mapper.ACSServiceAPI#performDeviceOperation(org.commscope.
   * tr069adapter.acs.DeviceRPCRequest)
   */
  @Override
  public long performDeviceOperation(DeviceRPCRequest deviceRPCRequest)
      throws MapperServiceException {

    Long opId = 0l;
    try {
      if (deviceRPCRequest != null && deviceRPCRequest.getDeviceDetails() != null) {
        MDC.put(CLIENT_STR, deviceRPCRequest.getDeviceDetails().getDeviceId());
      }

      // validate the request and reject if not valid.
      if (null == deviceRPCRequest) {
        logger.error("Received null Mapper Request.");
        throw new MapperServiceException("Received null Mapper Request.");
      } else if (null == deviceRPCRequest.getOpDetails()) {
        logger.error("Received null operation details.");
        throw new MapperServiceException("Received null operation details.");
      } else if (null == deviceRPCRequest.getOpDetails().getOpCode()) {
        logger.error("Received null operation code.");
        throw new MapperServiceException("Received null operation code.");
      }
      OperationCode opCode = deviceRPCRequest.getOpDetails().getOpCode();
      logger.info("Received request to perform device operation. OperationCode: {}", opCode);
      opId = opIdGenerator.generateOpId();
      logger.debug("The operation ID generated for processing the Device RPC request is - {}",
          opId);
      // set opId and forward the request
      deviceRPCRequest.setOperationId(opId);
      jmsTemplate.convertAndSend(TR069_NBI_REQUEST_Q, deviceRPCRequest);

      logger.debug(
          "Successfully posted the Mapper Request to Queue with OperationId : {} OperationCode : {}",
          deviceRPCRequest.getOperationId(), deviceRPCRequest.getOpDetails().getOpCode());
    } catch (Exception ex) {
      MapperServiceException mapperEx =
          new MapperServiceException("ACS Internal Error. Unknown Exception. Details :", ex);
      logger.error(mapperEx.getMessage());
      throw mapperEx;
    } finally {
      MDC.remove(CLIENT_STR);
    }

    return opId;
  }

}
