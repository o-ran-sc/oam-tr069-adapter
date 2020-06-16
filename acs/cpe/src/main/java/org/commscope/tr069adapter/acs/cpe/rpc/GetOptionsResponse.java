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
import java.util.List;

import javax.xml.soap.Name;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class GetOptionsResponse extends TR069RPC {

  private static final long serialVersionUID = -9160354248384671344L;

  public class OptionStruct {
    private String optionName;
    private String voucherSN;
    private int state;
    private int mode;
    private String startDate;
    private String expirationDate;
    private boolean isTransferable;

    public String getOptionName() {
      return optionName;
    }

    public String getVoucherSN() {
      return voucherSN;
    }

    public int getState() {
      return state;
    }

    public int getMode() {
      return mode;
    }

    public String getStartDate() {
      return startDate;
    }

    public String getExpirationDate() {
      return expirationDate;
    }

    public boolean isTransferable() {
      return isTransferable;
    }
  }

  public GetOptionsResponse() {
    name = "GetOptionsResponse";
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    logger.isDebugEnabled();
  }

  @SuppressWarnings("rawtypes")
  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    Iterator<SOAPElement> pi = getRequestChildElement(spf, body, "OptionList")
        .getChildElements(spf.createName("OptionStruct"));
    Name nameOptionName = spf.createName("OptionName");
    Name nameVoucherSN = spf.createName("VoucherSN");
    Name nameState = spf.createName("State");
    Name nameMode = spf.createName("Mode");
    Name nameStartDate = spf.createName("StartDate");
    Name nameExpirationDate = spf.createName("ExpirationDate");
    Name nameIsTransferable = spf.createName("IsTransferable");

    List<OptionStruct> optionList = new ArrayList<>();

    while (pi.hasNext()) {
      SOAPElement option = pi.next();
      OptionStruct o = new OptionStruct();
      o.optionName = getRequestElement(option, nameOptionName);
      o.voucherSN = getRequestElement(option, nameVoucherSN);
      o.state = Integer.parseInt(getRequestElement(option, nameState));
      o.mode = Integer.parseInt(getRequestElement(option, nameMode));
      o.startDate = getRequestElement(option, nameStartDate);
      o.expirationDate = getRequestElement(option, nameExpirationDate);
      o.isTransferable = Boolean.parseBoolean(getRequestElement(option, nameIsTransferable));
      optionList.add(o);
    }
  }

}
