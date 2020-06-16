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

package org.commscope.tr069adapter.acs.cpe.rpc;

import java.io.Serializable;

public class SetParameterValueFault implements Serializable {

  private static final long serialVersionUID = -1594414731431087298L;

  public SetParameterValueFault(String parameterName, String faultCode, String faultString) {
    this.faultCode = faultCode;
    this.faultString = faultString;
    this.parameterName = parameterName;
  }

  @Override
  public String toString() {
    return "SetParameterValueFault: ParameterName=" + parameterName + " FaultCode=" + faultCode
        + " FaultString=" + faultString;
  }

  private String parameterName;
  private String faultCode;
  private String faultString;

  public String getParameterName() {
    return parameterName;
  }

  public String getFaultCode() {
    return faultCode;
  }

  public String getFaultString() {
    return faultString;
  }

}
