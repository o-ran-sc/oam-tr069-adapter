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
import java.util.Iterator;

import javax.xml.soap.Name;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetParameterAttributesResponse extends TR069RPC {

  private static final long serialVersionUID = 5241691699448063027L;

  /** Creates a new instance of GetParameterAttributes */
  public GetParameterAttributesResponse() {
    name = "GetParameterAttributesResponse";
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    logger.isDebugEnabled();
  }

  @SuppressWarnings({"rawtypes", "unused"})
  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    SOAPElement ml = getRequestChildElement(spf, body, "ParameterList");
    int i = getArrayCount(spf, ml);
    Iterator<SOAPElement> mlist = ml.getChildElements(spf.createName("ParameterAttributeStruct"));
    attributes = new ParameterAttributeStruct[i];
    Name nameKey = spf.createName("Name");
    Name nameNotification = spf.createName("Notification");
    Name nameString = spf.createName("string");
    i = 0;
    while (mlist.hasNext()) {
      SOAPElement param = mlist.next();
      attributes[i] = new ParameterAttributeStruct();
      attributes[i].name = getRequestElement(param, nameKey);
      attributes[i].notification = Integer.parseInt(getRequestElement(param, nameNotification));
      // get acl array
      SOAPElement elementAccessList = getRequestChildElement(spf, param, "AccessList");
      int ii = getArrayCount(spf, elementAccessList);
      attributes[i].accessList = new String[ii];

      ii = 0;
      Iterator<SOAPElement> iteratorAccessList = elementAccessList.getChildElements(nameString);
      while (iteratorAccessList.hasNext()) {
        attributes[i].accessList[ii++] = ((SOAPElement) iteratorAccessList.next()).getValue();
      }
      i++;
    }

  }

  private ParameterAttributeStruct[] attributes;

  public ParameterAttributeStruct[] getAttributes() {
    return attributes;
  }

  public void setAttributes(ParameterAttributeStruct[] attributes) {
    this.attributes = attributes;
  }

  public class ParameterAttributeStruct implements Serializable {

    private static final long serialVersionUID = -5185719453218694220L;

    ParameterAttributeStruct() {}

    private String name;
    private int notification;
    private String[] accessList;

    public String getName() {
      return name;
    }

    public int getNotification() {
      return notification;
    }

    public String[] getAccessList() {
      return accessList;
    }
  }
}
