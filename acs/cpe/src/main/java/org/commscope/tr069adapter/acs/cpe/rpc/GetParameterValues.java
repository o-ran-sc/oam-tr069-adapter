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

import java.util.Map;

import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetParameterValues extends TR069RPC {

  private static final String GET_PARAMETER_VALUES = "GetParameterValues";
  private static final long serialVersionUID = -2247007005513782433L;

  /** Creates a new instance of GetParameterValues */
  public GetParameterValues() {
    name = GET_PARAMETER_VALUES;
  }

  public GetParameterValues(String[] parameterNames) {
    name = GET_PARAMETER_VALUES;
    this.parameterNames = parameterNames;
  }

  public GetParameterValues(Map<String, String> parameters) {
    name = GET_PARAMETER_VALUES;
    parameters.keySet().toArray(parameterNames);
  }

  public GetParameterValues(String paramName) {
    name = GET_PARAMETER_VALUES;
    this.parameterNames = new String[1];
    this.parameterNames[0] = paramName;
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    SOAPElement elm = body.addChildElement(spf.createName("ParameterNames"));
    elm.setAttribute(SOAP_ARRAY_TYPE, "xsd:string[" + parameterNames.length + "]");
    for (int i = 0; i < parameterNames.length; i++) {
      SOAPElement s = elm.addChildElement("string");
      s.setValue(parameterNames[i]);
    }
  }

  @Override
  public String toString() {
    StringBuilder s = new StringBuilder();
    for (int i = 0; i < parameterNames.length; i++) {
      s.append('\t');
      s.append(parameterNames[i]);
      s.append("'\t");
    }
    return s.toString();
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException {
    logger.isDebugEnabled();
  }

  private String[] parameterNames;

  public String[] getParameterNames() {
    return parameterNames;
  }

  public void setParameterNames(String[] parameterNames) {
    this.parameterNames = parameterNames;
  }
}
