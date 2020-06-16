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

package org.commscope.tr069adapter.acs.common.requestprocessor.service;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.dto.DeviceOperationRequestDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.exception.DeviceOperationException;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.response.DeviceInformResponse;

public interface TR069DeviceEventHandler {

  /**
   * @param deviceNotification
   * @return
   */
  public DeviceInformResponse processDeviceInform(DeviceInform deviceNotification) throws Exception;

  /**
   * Return type can be null, such case Empty HTTP response to be sent to the device
   * 
   * @param operationResult
   * @return
   */
  public DeviceRPCRequest processDeviceRPCResponse(DeviceRPCResponse operationResult)
      throws Exception;

  /**
   * Return type can be null, such case Empty HTTP response to be sent to the device
   * 
   * @return
   */
  public DeviceRPCRequest processEmptyDeviceRequest(TR069DeviceDetails deviceDetails)
      throws Exception;

  /**
   * @param sessionId
   * @return
   * @throws SessionManagerException
   */
  public DeviceOperationRequestDetails getOpRequestDetailsBySessionId(String sessionId)
      throws SessionManagerException;

  /**
   * @param deviceId
   * @return
   * @throws DeviceOperationException
   */
  public TR069DeviceDetails getDeviceDetails(String deviceId) throws DeviceOperationException;

}
