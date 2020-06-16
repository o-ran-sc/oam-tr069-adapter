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
import java.util.Map.Entry;

import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetParameterValuesResponse extends TR069RPC {

  private static final long serialVersionUID = 6927206690856966492L;

  /** Creates a new instance of GetParameterValuesResponse */
  public GetParameterValuesResponse() {
    name = "GetParameterValuesResponse";
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    logger.isDebugEnabled();
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    values = parseParamList(body, spf);
  }

  private Map<String, String> values;

  public Integer getParamInt(String name) {
    String v = values.get(name);
    if (v != null) {
      try {
        return Integer.parseInt(v);
      } catch (NumberFormatException e) {
        logger.error("Exception while getParamInt detail {}", e.toString());
      }
    }
    return null;
  }

  public Integer getParamInt(String name, int defaultValue) {
    Integer value = getParamInt(name);
    return (value != null) ? value : defaultValue;
  }

  public String getParam(String name) {
    return values.get(name);
  }

  public String getParam(String name, String defaultValue) {
    String value = getParam(name);
    return (value != null) ? value : defaultValue;
  }

  public Map<String, String> getValues() {
    return values;
  }

  public void setValues(Map<String, String> values) {
    this.values = values;
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder(1024);
    for (Entry<String, String> e : values.entrySet()) {
      b.append(e.getKey());
      b.append("=");
      b.append(e.getValue());
      b.append("\n");
    }
    return b.toString();
  }
}
