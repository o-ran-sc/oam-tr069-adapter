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

import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetParameterNamesResponse extends TR069RPC {

  private static final long serialVersionUID = 8724736185069683719L;

  /** Creates a new instance of GetParameterNamesResponse */
  public GetParameterNamesResponse() {
    name = "GetParameterNamesResponse";
  }

  @Override
  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    logger.isDebugEnabled();
  }

  @Override
  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    names = parseParamList(body, spf, "ParameterInfoStruct", "Writable");
  }

  public int[] getMultiInstanceNames(String prefix) {
    int[] r = new int[names.size()];
    int ix = 0;
    int pfxlength = prefix.length();
    for (Entry<String, String> e : names.entrySet()) {
      String k = e.getKey();
      String n;
      if (k.endsWith(".")) {
        n = k.substring(pfxlength, k.length() - 1);
      } else {
        n = k.substring(pfxlength);
      }
      r[ix++] = Integer.parseInt(n);
    }
    Arrays.sort(r);
    return r;
  }

  private Map<String, String> names;
}
