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

package org.commscope.tr069adapter.acs.common.exception;

import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.common.utils.Utility;

public class ACSException extends Exception {

  private static final long serialVersionUID = 9116478433222830454L;

  private static final String ERRORMSG_PREFIX = "TR069";

  private ErrorCode errorCode;

  private String[] arguments;

  private String message;

  /**
   * @param s
   */
  public ACSException(String s) {
    super(s);
  }

  /**
   * @param errorCode
   */
  public ACSException(ErrorCode errorCode) {
    super();
    this.errorCode = errorCode;
    setErrorMessage(getErrorMessage());
  }

  /**
   * @param errorCode
   * @param args
   */
  public ACSException(ErrorCode errorCode, String... args) {
    super();
    this.errorCode = errorCode;
    arguments = args;
    setErrorMessage(getErrorMessage());
  }

  public ErrorCode getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(ErrorCode errorCode) {
    this.errorCode = errorCode;
  }

  @Override
  public String getMessage() {
    return message;
  }

  /**
   * All the error messages defined in the TR069 ErrorMessage properties must have prefix "TR069"
   * 
   * @return
   */
  private String getErrorMessage() {
    String key = ERRORMSG_PREFIX + "." + errorCode.getErrorCodeKey();
    return Utility.getMessage(key, arguments);
  }

  private void setErrorMessage(String message) {
    this.message = message;
  }
}
