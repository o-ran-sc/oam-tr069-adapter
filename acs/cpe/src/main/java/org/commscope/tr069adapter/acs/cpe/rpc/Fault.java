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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;

import javax.xml.soap.Detail;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPHeaderElement;
import javax.xml.soap.SOAPMessage;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class Fault extends TR069RPC {
  private static final String FAULTSTRING = "FaultString";
  private static final String FAULTCODE = "FaultCode";
  private static final String FAULT_RPC = "Fault";
  private static final long serialVersionUID = 1L;

  public Fault() {
    name = FAULT_RPC;
  }

  public Fault(String soapFaultCode, String soapFaultString, String id) {
    name = FAULT_RPC;
    this.faultCodeCwmp = this.soapFaultCode = soapFaultCode;
    this.faultStringCwmp = this.soapFaultString = soapFaultString;
    this.id = id;
  }

  @Override
  public void writeTo(OutputStream out) {
    try {
      MessageFactory mf = MessageFactory.newInstance();
      SOAPFactory spf = SOAPFactory.newInstance();

      String s = "<soapenv:Envelope";
      s += " xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"";
      s += " xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"";
      s += " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"";
      s += " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"";
      s += " xmlns:cwmp=\"" + urnCWMP
          + "\"><soapenv:Header></soapenv:Header><soapenv:Body></soapenv:Body></soapenv:Envelope>";
      ByteArrayInputStream in = new ByteArrayInputStream(s.getBytes());
      SOAPMessage msg = mf.createMessage(null, in);

      SOAPHeaderElement elmntId =
          msg.getSOAPHeader().addHeaderElement(spf.createName("ID", CWMP, urnCWMP));
      elmntId.setValue(getId());
      elmntId.setAttribute("soapenv:mustUnderstand", "1");
      SOAPFault fault = msg.getSOAPBody().addFault();
      fault.addChildElement(spf.createName("faultcode")).setValue(String.valueOf("Server"));
      fault.addChildElement(spf.createName("faultstring")).setValue(String.valueOf("CWMP Fault"));

      if (name == null || name.equals("")) {
        name = this.getClass().getSimpleName();
      }
      msg.writeTo(out);
    } catch (SOAPException ex) {
      logger.error("Exception  : {}", ex.getMessage());
    } catch (IOException e) {
      logger.error("Exception  : {}", e.getMessage());
    }
  }

  @Override
  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    logger.isDebugEnabled();
  }

  protected void createBody(Detail body, SOAPFactory spf) throws SOAPException {

    SOAPElement element = spf.createElement(FAULT_RPC, CWMP, urnCWMP);
    element.addChildElement(spf.createName(FAULTCODE)).setValue(String.valueOf(this.faultCodeCwmp));
    element.addChildElement(spf.createName(FAULTSTRING))
        .setValue(String.valueOf(this.faultStringCwmp));
    body.addChildElement(element);
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    try {
      soapFaultCode = getRequestElement(spf, body, "faultcode");
    } catch (Exception e) {
      logger.error("Exception while parseBody faultcode {}", e.toString());
    }
    try {
      soapFaultString = getRequestElement(spf, body, "faultstring");
    } catch (Exception e) {
      logger.error("Exception while parseBody faultstring {}", e.toString());
    }
    SOAPElement detail = null;
    try {
      detail = getRequestChildElement(spf, body, "detail");
    } catch (Exception e) {
      logger.error("Exception while parseBody detail {}", e.toString());
    }
    if (detail == null) {
      detail = body; // for one broken cpe
    }
    SOAPElement cwmpfault = getRequestChildElement2(spf, detail, FAULT_RPC);
    faultCodeCwmp = getRequestElement(spf, cwmpfault, FAULTCODE);
    faultStringCwmp = getRequestElement(spf, cwmpfault, FAULTSTRING);

    Iterator<?> i = cwmpfault.getChildElements(spf.createName("SetParameterValuesFault"));
    if (i.hasNext()) {
      setParameterValuesFaults = new ArrayList<>();
    }
    while (i.hasNext()) {
      SOAPElement f = (SOAPElement) i.next();
      SetParameterValueFault vf =
          new SetParameterValueFault(getRequestElement(spf, f, "ParameterName"),
              getRequestElement(spf, f, FAULTCODE), getRequestElement(spf, f, FAULTSTRING));
      setParameterValuesFaults.add(vf);
    }
    if (setParameterValuesFaults != null) {
      for (SetParameterValueFault f : setParameterValuesFaults) {
        logger.error("n={} c={} s={}", f.getParameterName(), f.getFaultCode(), f.getFaultString());
      }
    }
  }

  @Override
  public String toString() {
    return "FAULT: code=" + soapFaultCode + " msg=" + soapFaultString + " code=" + faultCodeCwmp
        + " cmsg=" + faultStringCwmp;
  }

  private String soapFaultCode;
  private String soapFaultString;
  private String faultCodeCwmp;
  private String faultStringCwmp;
  private ArrayList<SetParameterValueFault> setParameterValuesFaults;

  public String getFaultString() {
    return soapFaultString;
  }

  public String getFaultStringCwmp() {
    return faultStringCwmp;
  }

  public String getFaultCode() {
    return soapFaultCode;
  }

  public String getCwmpFaultCode() {
    return faultCodeCwmp;
  }

  public static final String FCODE_REQUEST_DENIED = "9001";
  public static final String FCODE_INTERNAL = "9002";
  public static final String FCODE_INVALID_ARGS = "9003";
  public static final String FCODE_RESOURCE_EXCEEDED = "9004";
  public static final String FCODE_INVALID_PARAMETER_NAME = "9005";
  public static final String FCODE_INVALID_PARAMETER_TYPE = "9006";
  public static final String FCODE_INVALID_PARAMETER_VALUE = "9007";
  public static final String FCODE_PARAMETER_READONLY = "9008";
  public static final String FCODE_NOTIFICATION_REJECTED = "9009";
  public static final String FCODE_DOWNLOAD_FAILURE = "9010";
  public static final String FCODE_UPLOAD_FAILURE = "9011";
  public static final String FCODE_FILE_TRANSFER_AUTHENTICATION_FAILURE = "9012";
  public static final String FCODE_PROTOCOL_NOT_SUPPORTED = "9013";
  public static final String FCODE_DLF_MULTICAST = "9014";
  public static final String FCODE_DLF_NO_CONTACT = "9015";
  public static final String FCODE_DLF_FILE_ACCESS = "9016";
  public static final String FCODE_DLF_UNABLE_TO_COMPLETE = "9017";
  public static final String FCODE_DLF_FILE_CORRUPTED = "9018";
  public static final String FCODE_DLF_FILE_AUTHENTICATION = "9019";
  public static final String FCODE_ACS_METHOD_NOT_SUPPORTED = "8000";
  public static final String FCODE_ACS_REQUEST_DENIED = "8001";
  public static final String FCODE_ACS_INTERNAL_ERROR = "8002";
  public static final String FCODE_ACS_INVALID_ARGS = "8003";
  public static final String FCODE_ACS_RESOURCE_EXCEEDED = "8004";
  public static final String FCODE_ACS_RETRY = "8005";

}
