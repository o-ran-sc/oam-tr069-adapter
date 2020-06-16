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

public class NetConfResponse implements Serializable {

  private static final long serialVersionUID = -7593813981401577995L;

  // Netconf Response XML containing data xml element
  private String netconfResponseXml;

  // operation error code
  private ErrorCodeDetails errorCode;

  // Error Message
  private String errorMessage;

  public String getNetconfResponseXml() {
    return netconfResponseXml;
  }

  public void setNetconfResponseXml(String netconfResponseXml) {
    this.netconfResponseXml = netconfResponseXml;
  }

  public ErrorCodeDetails getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(ErrorCodeDetails errorCode) {
    this.errorCode = errorCode;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  @Override
  public String toString() {
    return "NetConfResponse [netconfResponseXml=" + netconfResponseXml + ", errorCode=" + errorCode
        + ", errorMessage=" + errorMessage + "]";
  }
}
