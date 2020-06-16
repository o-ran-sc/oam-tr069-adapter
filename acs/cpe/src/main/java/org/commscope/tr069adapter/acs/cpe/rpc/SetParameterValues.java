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

import java.util.ArrayList;
import java.util.List;

import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class SetParameterValues extends TR069RPC {

  private static final long serialVersionUID = -7143315228088632032L;

  /** Creates a new instance of GetParameterValues */
  public SetParameterValues() {
    name = "SetParameterValues";
    names = new ArrayList<>();
    values = new ArrayList<>();
    types = new ArrayList<>();
  }

  public void merge(SetParameterValues other) {
    for (int i = 0; i < other.names.size(); i++) {
      names.add(other.names.get(i));
      values.add(other.values.get(i));
      types.add(other.types.get(i));
    }
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    SOAPElement elm = body.addChildElement(spf.createName("ParameterList"));
    elm.setAttribute(SOAP_ARRAY_TYPE, "cwmp:ParameterValueStruct[" + names.size() + "]");
    for (int i = 0; i < names.size(); i++) {
      SOAPElement param = elm.addChildElement("ParameterValueStruct");
      param.addChildElement("Name").setValue(names.get(i));
      SOAPElement v = param.addChildElement("Value");
      v.setValue(values.get(i));
      v.setAttribute(XSI_TYPE, getXmlType(types.get(i)));
    }
    body.addChildElement(PARAMETER_KEY).setValue(key);
  }

  @Override
  protected void createBody(SOAPBodyElement body, SOAPFactory spf, String key)
      throws SOAPException {
    SOAPElement elm = body.addChildElement(spf.createName("ParameterList"));
    elm.setAttribute(SOAP_ARRAY_TYPE, "cwmp:ParameterValueStruct[" + names.size() + "]");
    for (int i = 0; i < names.size(); i++) {
      SOAPElement param = elm.addChildElement("ParameterValueStruct");
      param.addChildElement("Name").setValue(names.get(i));
      SOAPElement v = param.addChildElement("Value");
      v.setValue(values.get(i));
      v.setAttribute(XSI_TYPE, getXmlType(types.get(i)));
    }
    body.addChildElement(PARAMETER_KEY).setValue(key);
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException {
    logger.isDebugEnabled();
  }

  public void addValue(String name, String value) {
    addValue(name, value, XSD_STRING);
  }

  public void addValue(String name, Integer value) {
    addValue(name, value.toString(), XSD_UNSIGNEDINT);
  }

  public void addValue(String name, String value, String type) {
    names.add(name);
    values.add(value);
    types.add(type);
  }

  public boolean isEmpty() {
    return names.isEmpty();
  }

  @Override
  public String toString() {
    StringBuilder s = new StringBuilder();
    for (int i = 0; i < names.size(); i++) {
      s.append('\t');
      s.append(names.get(i));
      s.append(" (");
      s.append(types.get(i));
      s.append(") '");
      s.append(values.get(i));
      s.append("'\n");
    }
    return s.toString();
  }

  private List<String> names;
  private List<String> values;
  private List<String> types;
  private String key = "unsetCommandKey";
}
