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

package org.commscope.tr069adapter.vesagent.exception;

/**
 * 
 * @version 1.0
 * @since May 21, 2020
 * @author Prashant
 */
public class VesAgentException extends Exception {
  private static final long serialVersionUID = -3742697051389101875L;

  private static final String ERRORMSG_PREFIX = "ves-agent";

  private String[] arguments;

  private String errorCode;
  private String message;

  // index of the error occurred in the given list or in the given file
  protected int errorIndex = -1;


  /**
   * Constructs a <code>VesOperationException</code> with no detail message.
   * 
   */
  public VesAgentException() {
    super();
  }

  /**
   * Constructs a <code>VesOperationException</code> with the specified detail message.
   * 
   * @param s as the details message
   */
  public VesAgentException(String s) {
    super(s);
  }

  public VesAgentException(String... args) {
    super();
    arguments = args;
    setErrorMessage(getErrorMessage());
  }

  public VesAgentException(String errorCode, String errorMsg) {
    super();
    this.errorCode = errorCode;
    setErrorMessage(getErrorMessage());
  }

  public int getErrorIndex() {
    return errorIndex;
  }

  public void setErrorIndex(int errorIndex) {
    this.errorIndex = errorIndex;
  }


  public String getMessage() {
    return message;
  }

  private String getErrorMessage() {
    String key = ERRORMSG_PREFIX + ".";
    return key;
  }

  private void setErrorMessage(String message) {
    this.message = message;
  }

  public String getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(String errorCode) {
    this.errorCode = errorCode;
  }
}
