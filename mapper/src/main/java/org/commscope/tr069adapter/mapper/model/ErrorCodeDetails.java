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

public class ErrorCodeDetails implements Serializable {

  private static final long serialVersionUID = -2772256485667214776L;

  private String faultCode;

  private String errorMessage;

  private String errorType;

  private String errorTag;

  private String errorSeverity;

  public ErrorCodeDetails() {

  }

  public ErrorCodeDetails(String errorMessage, String errorType, String errorTag,
      String errorSeverity) {
    super();
    this.errorMessage = errorMessage;
    this.errorType = errorType;
    this.errorTag = errorTag;
    this.errorSeverity = errorSeverity;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public String getErrorType() {
    return errorType;
  }

  public void setErrorType(String errorType) {
    this.errorType = errorType;
  }

  public String getErrorTag() {
    return errorTag;
  }

  public void setErrorTag(String errorTag) {
    this.errorTag = errorTag;
  }

  public String getErrorSeverity() {
    return errorSeverity;
  }

  public void setErrorSeverity(String errorSeverity) {
    this.errorSeverity = errorSeverity;
  }

  public String getFaultCode() {
    return faultCode;
  }

  public void setFaultCode(String faultCode) {
    this.faultCode = faultCode;
  }

  @Override
  public String toString() {
    return "ErrorCodeMetaData [errorMessage=" + errorMessage + ", errorType=" + errorType
        + ", errorTag=" + errorTag + ", errorSeverity=" + errorSeverity + "]";
  }

}
