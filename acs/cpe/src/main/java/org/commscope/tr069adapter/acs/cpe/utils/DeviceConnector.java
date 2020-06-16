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

package org.commscope.tr069adapter.acs.cpe.utils;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.DEFAULT_CONNECTION_TIMEOUT;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.HTTP_CONNECTION_IDLE_TIMEOUT;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.HTTP_OP_FAILED;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.HTTP_OP_SUCCESS;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.HTTP_STATUS_OK;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.HTTP_STATUS_OK_WITH_NO_CONTENT;

import java.io.IOException;

import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.params.HttpConnectionManagerParams;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class DeviceConnector {

  private static final Logger logger = LoggerFactory.getLogger(DeviceConnector.class);

  /**
   * Performs HTTP get on the connection request URL for the device.
   * 
   * @param deviceDetails
   * @return
   * @throws IOException
   * @throws Exception
   */
  public DeviceRPCResponse requestConnectionHttp(TR069DeviceDetails deviceDetails)
      throws IOException {

    HttpClient client = new HttpClient();
    HttpConnectionManager hcm = client.getHttpConnectionManager();
    HttpConnectionManagerParams hcmParam = hcm.getParams();
    hcmParam.setConnectionTimeout(DEFAULT_CONNECTION_TIMEOUT);
    hcmParam.setSoTimeout(DEFAULT_CONNECTION_TIMEOUT);
    hcm.setParams(hcmParam);
    client.setHttpConnectionManager(hcm);

    if (deviceDetails.getUsername() != null && deviceDetails.getPassword() != null) {
      client.getState().setCredentials(new AuthScope(null, -1), new UsernamePasswordCredentials(
          deviceDetails.getUsername(), deviceDetails.getPassword()));
    } else {
      logger.error("Insufficient HTTP arguments: UserName or password is null");
      throw new HttpException("Insufficient HTTP arguments: UserName or password is null");
    }

    DefaultHttpMethodRetryHandler retryhandler = new DefaultHttpMethodRetryHandler(1, true);
    client.getParams().setParameter("http.method.retry-handler", retryhandler);
    GetMethod get = new GetMethod(deviceDetails.getConnectionRequestURL());
    get.setDoAuthentication(true);

    try {
      int status = client.executeMethod(get);
      return populateOperationResult(deviceDetails, get.getResponseBodyAsString(), status);
    } finally {
      get.releaseConnection();
      hcm.closeIdleConnections(HTTP_CONNECTION_IDLE_TIMEOUT);
    }
  }

  /**
   * @param deviceDtails
   * @param message
   * @param httpStatus
   * @return
   */
  private DeviceRPCResponse populateOperationResult(TR069DeviceDetails deviceDtails, String message,
      int httpStatus) {

    DeviceRPCResponse deviceRPCResponse = new DeviceRPCResponse();
    OperationResponse operationResponse = new OperationResponse();

    operationResponse.setOperationCode(TR069OperationCode.INITIATE_CR);

    // HTTP status for successful connection can be 200=Ok, 204=No Content)
    if (httpStatus != HTTP_STATUS_OK && httpStatus != HTTP_STATUS_OK_WITH_NO_CONTENT) {
      operationResponse.setStatus(HTTP_OP_FAILED);
      deviceRPCResponse.setFaultKey(httpStatus + "");
      deviceRPCResponse.setFaultString(message);
    } else {
      operationResponse.setStatus(HTTP_OP_SUCCESS);
    }

    deviceRPCResponse.setOperationResponse(operationResponse);
    deviceRPCResponse.setDeviceDetails(deviceDtails);

    // nBIOperationResult.setOperationId(operationId);// why operation id is here ?

    return deviceRPCResponse;
  }

}
