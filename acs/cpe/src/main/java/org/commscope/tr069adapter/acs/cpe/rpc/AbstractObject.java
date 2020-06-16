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

public class AbstractObject extends TR069RPC {

  private static final long serialVersionUID = 416107715167714733L;

  protected String parameterKey = "";
  protected String objectName = "";

  @Override
  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    body.addChildElement("ObjectName").setValue(objectName);
    body.addChildElement(PARAMETER_KEY).setValue(parameterKey);
  }

  @Override
  protected void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException {
    logger.isDebugEnabled();
  }
}
