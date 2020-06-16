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

package org.commscope.tr069adapter.mapper.model;

import java.io.Serializable;

public class VESNotificationResponse implements Serializable {
  private int statusCode;
  private String responseMsg;

  public VESNotificationResponse() {
    super();
  }

  public VESNotificationResponse(int statusCode, String responseMsg) {
    super();
    this.statusCode = statusCode;
    this.responseMsg = responseMsg;
  }

  public int getStatusCode() {
    return statusCode;
  }

  public void setStatusCode(int statusCode) {
    this.statusCode = statusCode;
  }

  public String getResponseMsg() {
    return responseMsg;
  }

  public void setResponseMsg(String responseMsg) {
    this.responseMsg = responseMsg;
  }

}
