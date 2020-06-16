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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.xml.soap.Name;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetQueuedTransfersResponse extends TR069RPC {

  private static final long serialVersionUID = 8169512570542407908L;

  @Override
  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    logger.isDebugEnabled();
  }

  @SuppressWarnings("rawtypes")
  @Override
  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    Iterator<SOAPElement> pi = getRequestChildElement(spf, body, "TransferList")
        .getChildElements(spf.createName("QueuedTransferStruct"));
    Name nameCommandKey = spf.createName(COMMAND_KEY);
    Name nameState = spf.createName("State");
    Map<String, String> transferList = new HashMap<>();
    while (pi.hasNext()) {
      SOAPElement param = pi.next();
      String key = getRequestElement(param, nameCommandKey);
      String state = getRequestElement(param, nameState);
      transferList.put(key, state);
    }

  }

}
