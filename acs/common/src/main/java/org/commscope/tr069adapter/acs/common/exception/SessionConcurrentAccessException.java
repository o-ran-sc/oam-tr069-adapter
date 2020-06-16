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

public class SessionConcurrentAccessException extends ACSException {

  private static final long serialVersionUID = -6589214938353510704L;

  public SessionConcurrentAccessException(ErrorCode errorCode, String... args) {
    super(errorCode, args);
  }

  public SessionConcurrentAccessException(ErrorCode errorCode) {
    super(errorCode);
  }

  public SessionConcurrentAccessException(String s) {
    super(s);
  }
}
