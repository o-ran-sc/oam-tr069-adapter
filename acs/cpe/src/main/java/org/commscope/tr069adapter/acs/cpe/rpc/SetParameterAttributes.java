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

public class SetParameterAttributes extends TR069RPC {

  private static final long serialVersionUID = 1L;

  public SetParameterAttributes() {
    name = "SetParameterAttributes";
    attrs = new ArrayList<>();
  }

  public void addAttribute(String name, boolean notificationChange, int notification,
      boolean accessListChange, String[] accessList) {
    attrs.add(new SetParameterAttributesStruct(name, notificationChange, notification,
        accessListChange, accessList));
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    SOAPElement elm = body.addChildElement(spf.createName("ParameterList"));
    elm.setAttribute(SOAP_ARRAY_TYPE, "cwmp:SetParameterAttributesStruct[" + attrs.size() + "]");
    int c = attrs.size();
    for (int i = 0; i < c; i++) {
      SOAPElement param = elm.addChildElement("SetParameterAttributesStruct");
      param.addChildElement("Name").setValue(attrs.get(i).getName());
      param.addChildElement("NotificationChange")
          .setValue(b2s(attrs.get(i).isNotificationChange()));
      param.addChildElement("Notification")
          .setValue(String.valueOf(attrs.get(i).getNotification()));

      param.addChildElement("AccessListChange").setValue(b2s(attrs.get(i).isAccessListChange()));

      SOAPElement al = param.addChildElement(spf.createName("AccessList"));
      if (null != attrs.get(i).getAccessList()) {
        String[] acl = attrs.get(i).getAccessList();
        int ca = 0;
        if (acl != null)
          ca = acl.length;
        al.setAttribute(SOAP_ARRAY_TYPE, "xsd:string[" + ca + "]");
        for (int i2 = 0; i2 < ca; i2++) {
          SOAPElement acle = al.addChildElement("string");
          acle.setValue(acl[i2]);
          acle.setAttribute(XSI_TYPE, XSD_STRING);
        }
      }

    }
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException {
    logger.isDebugEnabled();
  }

  private List<SetParameterAttributesStruct> attrs;
}
