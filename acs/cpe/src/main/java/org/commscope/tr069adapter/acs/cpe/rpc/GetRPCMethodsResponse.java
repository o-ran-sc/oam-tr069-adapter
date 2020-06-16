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
import java.util.Iterator;

import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetRPCMethodsResponse extends TR069RPC {

  private static final long serialVersionUID = -2718028310071462414L;

  /** Creates a new instance of GetRPCMethodsResponse */
  public GetRPCMethodsResponse() {}

  public GetRPCMethodsResponse(GetRPCMethods req) {
    this.id = req.getId();
    methods = new String[] {"Inform", "TransferComplete", "GetRPCMethods"};
    name = "GetRPCMethodsResponse";
  }

  @Override
  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    SOAPElement mlst = body.addChildElement(spf.createName("MethodList"));
    mlst.setAttribute(SOAP_ARRAY_TYPE, "xsd:string[" + methods.length + "]");
    for (String m : methods) {
      mlst.addChildElement("string").setValue(m);
    }

  }

  @SuppressWarnings({"rawtypes", "unused"})
  @Override
  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    SOAPElement ml = getRequestChildElement(spf, body, "MethodList");
    getArrayCount(spf, ml);
    Iterator<Object> mlist = ml.getChildElements();
    ArrayList<String> m = new ArrayList<>();
    while (mlist.hasNext()) {
      Object e = mlist.next();
      if (e instanceof SOAPElement) {
        SOAPElement el = (SOAPElement) e;
        if (el.getElementQName().getLocalPart().equals("string")) {
          m.add(el.getValue());
        }
      }
    }
    methods = m.toArray(new String[1]);
  }

  private String[] methods;

  public String[] getMethods() {
    return methods;
  }
}
