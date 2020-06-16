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

package org.commscope.tr069adapter.acs.common.response;

import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;

public class SetParameterValueResponse extends OperationResponse {

  private static final long serialVersionUID = -6420901558440394915L;

  public SetParameterValueResponse() {
    operationCode = TR069OperationCode.SET_PARAMETER_VALUES;
  }

}
