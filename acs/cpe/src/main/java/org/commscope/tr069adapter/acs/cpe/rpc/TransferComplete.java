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
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class TransferComplete extends TR069RPC {

  private static final long serialVersionUID = -2176842223562717026L;

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    logger.isDebugEnabled();
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    startTime = getRequestElement(spf, body, "StartTime");
    completeTime = getRequestElement(spf, body, "CompleteTime");
    commandKey = getRequestElement(spf, body, COMMAND_KEY);
    SOAPElement fault = getRequestChildElement(spf, body, "FaultStruct");
    if (fault != null) {
      faultCode = Integer.parseInt(getRequestElement(spf, fault, "FaultCode"));
      faultString = getRequestElement(spf, fault, "FaultString");
    } else {
      faultCode = 0;
      faultString = null;
    }
  }

  @Override
  public String toString() {
    return "TransferComplete: cmdkey=" + commandKey + " faultcode=" + faultCode + " faultstring="
        + faultString;
  }

  private String commandKey;
  private String startTime;
  private String completeTime;
  private int faultCode;
  private String faultString;

  public String getCommandKey() {
    return commandKey;
  }

  public String getStartTime() {
    return startTime;
  }

  public String getCompleteTime() {
    return completeTime;
  }

  public int getFaultCode() {
    return faultCode;
  }

  public String getFaultString() {
    return faultString;
  }

}
