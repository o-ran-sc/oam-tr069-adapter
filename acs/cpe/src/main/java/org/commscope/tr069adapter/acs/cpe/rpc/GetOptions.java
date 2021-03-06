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

import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetOptions extends TR069RPC {

  private static final long serialVersionUID = -2337523798125454936L;

  public GetOptions() {
    name = "GetOptions";
  }

  public GetOptions(String option) {
    name = "GetOptions";
    optionName = option;
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    body.addChildElement("OptionName").setValue(optionName);
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException {
    logger.isDebugEnabled();
  }

  private String optionName = "";
}
