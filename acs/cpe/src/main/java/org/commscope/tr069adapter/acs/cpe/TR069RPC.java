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


package org.commscope.tr069adapter.acs.cpe;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.security.SecureRandom;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;

import javax.xml.soap.MessageFactory;
import javax.xml.soap.Name;
import javax.xml.soap.Node;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPHeaderElement;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;

import org.commscope.tr069adapter.acs.cpe.rpc.Fault;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class TR069RPC implements Serializable {

  private static final String HTTP_SCHEMA_ENCODING = "http://schemas.xmlsoap.org/soap/encoding/";

  private static final long serialVersionUID = 7270475819053880884L;

  protected static final Logger logger = LoggerFactory.getLogger(TR069RPC.class);

  private Random mrandom = new SecureRandom();

  /** Creates a new instance of Message */
  public TR069RPC() {}

  public static final String ENVELOPE_NAMESPACE = "http://schemas.xmlsoap.org/soap/envelope/";

  protected abstract void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException;

  protected abstract void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException;

  protected class ArrayType {

    public ArrayType() {
      super();
    }

    private String type;

    public String getType() {
      return type;
    }

    public Name getType(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
      int i = type.indexOf(':');
      if (i == -1) {
        return spf.createName(type);
      } else {
        String prefix = type.substring(0, i);
        SOAPBody b = (SOAPBody) body.getParentElement();
        SOAPEnvelope e = (SOAPEnvelope) b.getParentElement();
        SOAPHeader h = e.getHeader();
        String uri = null;
        try {
          uri = h.lookupNamespaceURI(prefix);
        } catch (Exception ee) {
          logger.error("While geting namespace URI 1 {}", ee.toString());
        }
        if (uri == null) {
          try {
            uri = e.lookupNamespaceURI(prefix);
          } catch (Exception ee) {
            logger.error("While geting namespace URI 2 {}", ee.toString());
          }
        }
        if (uri == null) {
          try {
            uri = b.lookupNamespaceURI(prefix);
          } catch (Exception ee) {
            logger.error("While geting namespace URI {} ", ee.toString());
          }
        }
        return spf.createName(type.substring(i + 1), prefix, uri);
      }
    }

    public void setType(String type) {
      this.type = type;
    }
  }

  public static SOAPBodyElement getRequest(SOAPMessage msg) throws SOAPException {
    SOAPBodyElement request = null;
    Iterator<Node> i1 = msg.getSOAPBody().getChildElements();
    while (i1.hasNext()) {
      Node n = i1.next();
      if (n.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
        request = (SOAPBodyElement) n;
      }
    }
    return request;
  }

  private static String getRequestName(SOAPMessage msg) throws SOAPException {
    if (msg.getSOAPBody().hasFault()) {
      return "Fault";
    }
    String name = "";
    SOAPBodyElement element = getRequest(msg);
    if (element != null) {
      name = element.getNodeName();
    }
    if (name.startsWith("cwmp:")) {
      name = name.substring(5);
    } else if (name.startsWith("cwmp_x:")) {
      name = name.substring(7);
    } else if (name.indexOf(':') != -1) {
      name = name.substring(name.indexOf(':') + 1, name.length());
    }
    return name;
  }

  public static TR069RPC parse(SOAPMessage soapMsg)
      throws SOAPException, InstantiationException, IllegalAccessException, ClassNotFoundException {
    String reqname = TR069RPC.getRequestName(soapMsg);

    TR069RPC msg = null;
    try {
      msg = (TR069RPC) Class.forName("org.commscope.tr069adapter.acs.cpe.rpc." + reqname)
          .newInstance();
    } catch (Exception e) {
      msg = (TR069RPC) Class.forName("org.commscope.tr069adapter.acs.cpe.rpc." + reqname)
          .newInstance();
    }
    msg = msg.parseSoapMessage(soapMsg);
    return msg;
  }

  @SuppressWarnings("unchecked")
  private TR069RPC parseSoapMessage(SOAPMessage soapMsg) throws SOAPException {
    SOAPEnvelope env = soapMsg.getSOAPPart().getEnvelope();

    Iterator<String> pfxs = env.getNamespacePrefixes();
    while (pfxs.hasNext()) {
      String pfx = pfxs.next();
      String uri = env.getNamespaceURI(pfx);
      if (uri.startsWith("urn:dslforum-org:cwmp-")) {
        urnCWMP = uri;
      }
    }
    SOAPFactory spf = SOAPFactory.newInstance();
    SOAPBodyElement soaprequest = getRequest(soapMsg);
    SOAPHeader hdr = soapMsg.getSOAPHeader();
    id = "device_did_not_send_id"; // or make it null?...
    if (hdr != null) {
      try {
        id = getHeaderElement(spf, hdr, "ID");
      } catch (Exception e) {
        logger.error("While parsing the soap message {}", e.toString());
      }
    }
    name = getRequestName(soapMsg);
    if (soaprequest != null) {
      Fault fault = parseSOAPRequest(soaprequest, spf);
      if (fault != null)
        return fault;
    }
    return this;
  }

  private Fault parseSOAPRequest(SOAPBodyElement soaprequest, SOAPFactory spf)
      throws SOAPException {
    if (soaprequest != null) {
      try {
        parseBody(soaprequest, spf);
      } catch (Exception e) {
        SOAPElement se = getRequestChildElement(spf, soaprequest, FAULT_CODE);
        String fc = (se != null) ? se.getValue() : "0";
        SOAPElement se2 = getRequestChildElement(spf, soaprequest, FAULT_STRING);
        String fs = (se2 != null) ? se2.getValue() : "0";

        if (se != null || se2 != null) {
          return new Fault(fc, fs, id);
        }
        throw e;
      }
    }
    return null;
  }

  public void writeTo(OutputStream out) {
    try {
      SOAPFactory spf = SOAPFactory.newInstance();
      MessageFactory factory = MessageFactory.newInstance();
      SOAPMessage soapMsg = factory.createMessage();
      SOAPPart part = soapMsg.getSOAPPart();

      SOAPEnvelope envelope = part.getEnvelope();
      SOAPHeader header = envelope.getHeader();
      SOAPBody body = envelope.getBody();

      String responseId = getId();

      envelope.addNamespaceDeclaration("xsd", "http://www.w3.org/2001/XMLSchema");
      envelope.addNamespaceDeclaration("cwmp", urnCWMP);
      envelope.addNamespaceDeclaration("SOAP-ENC", HTTP_SCHEMA_ENCODING);
      envelope.addNamespaceDeclaration("SOAP-ENV", ENVELOPE_NAMESPACE);
      envelope.addNamespaceDeclaration("xsi", "http://www.w3.org/2001/XMLSchema-instance");

      SOAPElement element = header.addChildElement(spf.createName("ID", "cwmp", urnCWMP));
      element.addAttribute(spf.createName("mustUnderstand", "SOAP-ENV", ENVELOPE_NAMESPACE),
          responseId);
      element.addTextNode("1");

      body.setEncodingStyle(HTTP_SCHEMA_ENCODING);
      SOAPBodyElement bd = body.addBodyElement(spf.createName(name, CWMP, urnCWMP));

      if (name == null || name.equals("")) {
        name = this.getClass().getSimpleName();
      }
      createBody(bd, spf, id);

      soapMsg.writeTo(out);
    } catch (SOAPException ex) {
      logger.error("Exception occurred while constructing SOAP message: {}", ex.getMessage());
    } catch (IOException e) {
      logger.error("Exception occurred while constructing SOAP message: {}", e.getMessage());
    }
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf, String key)
      throws SOAPException {
    logger.debug("Key element is: {}", key);
    createBody(body, spf);
  }

  protected SOAPElement getRequestChildElement(SOAPFactory f, SOAPElement req, String name) {
    @SuppressWarnings("unchecked")
    Iterator<Object> i = req.getChildElements();
    f.getClass();
    while (i.hasNext()) {
      Object o = i.next();
      try {
        Node nn = (Node) o;
        String n = nn.getLocalName();
        if (n != null && n.equals(name)) {
          return (SOAPElement) o;
        }
      } catch (Exception e) {
        logger.debug("Exception: {}, {}", e.getMessage(), e.getClass().getName());
      }
    }
    return null;
  }

  protected SOAPElement getRequestChildElement2(SOAPFactory f, SOAPElement req, String name)
      throws SOAPException {
    return (SOAPElement) req.getChildElements(f.createName(name, CWMP, urnCWMP)).next();
  }

  protected String getRequestElement(SOAPFactory f, SOAPElement req, String name) {
    return getRequestChildElement(f, req, name).getValue();
  }

  protected String getRequestElement(SOAPFactory f, SOAPElement req, String name, String def) {
    String v = getRequestChildElement(f, req, name).getValue();
    return (v != null) ? v : def;
  }

  protected SOAPElement getRequestChildElement(SOAPElement req, Name name) {
    return (SOAPElement) req.getChildElements(name).next();
  }

  protected String getRequestElement(SOAPElement req, Name name) {
    return getRequestChildElement(req, name).getValue();
  }

  protected String getHeaderElement(SOAPFactory f, SOAPHeader hdr, String name)
      throws SOAPException {
    return ((SOAPHeaderElement) hdr.getChildElements(f.createName(name, CWMP, urnCWMP)).next())
        .getValue();
  }

  protected HashMap<String, String> parseParamList(SOAPElement body, SOAPFactory spf)
      throws SOAPException {
    return parseParamList(body, spf, "ParameterValueStruct", "Value");
  }

  protected HashMap<String, String> parseParamList(SOAPElement body, SOAPFactory spf, String sn,
      String vn) throws SOAPException {
    Iterator<SOAPElement> pi =
        getRequestChildElement(spf, body, "ParameterList").getChildElements(spf.createName(sn));
    Name nameKey = spf.createName("Name");
    Name nameValue = spf.createName(vn);
    HashMap<String, String> pl = new HashMap<>();
    while (pi.hasNext()) {
      SOAPElement param = pi.next();
      String key = getRequestElement(param, nameKey);
      String value = getRequestElement(param, nameValue);
      if (value == null) {
        value = "";
      }
      pl.put(key, value);
    }
    return pl;
  }

  protected int getArrayCount(SOAPFactory spf, SOAPElement e) throws SOAPException {
    return getArrayCount(spf, e, null);
  }

  protected int getArrayCount(SOAPFactory spf, SOAPElement e, ArrayType type) throws SOAPException {
    Name nameArray = spf.createName("arrayType", "soap-enc", HTTP_SCHEMA_ENCODING);
    String attr = e.getAttributeValue(nameArray);
    if (attr == null) {
      return 0;
    }
    attr = attr.replace(" ", "");
    int i = attr.indexOf('[');
    String c = attr.substring(i + 1, attr.length() - 1);
    if (type != null) {
      type.setType(attr.substring(0, i));
    }
    return Integer.parseInt(c);
  }

  public boolean isFault() {
    return name.equals("Fault");
  }

  protected String b2s(boolean b) {
    return (b) ? "1" : "0";
  }

  protected String name;

  public String getName() {
    return name;
  }

  protected String id;

  public String getId() {
    if (id == null) {
      id = "" + mrandom.nextInt(99999);
    }
    return id;
  }

  protected void println(StringBuilder b, String n, String v) {
    b.append(n);
    b.append(": ");
    b.append(v);
    b.append("\n");
  }

  protected void println(StringBuilder b, String n, String n2, String v) {
    b.append(n);
    println(b, n2, v);
  }

  public String getCWMPVersion() {
    return urnCWMP;
  }

  public void setCWMPVersion(String cwmpVersion) {
    urnCWMP = cwmpVersion;
  }

  protected String urnCWMP = "urn:dslforum-org:cwmp-1-0";
  protected static final String CWMP = "cwmp";
  protected static final String PARAMETER_KEY = "ParameterKey";
  protected static final String COMMAND_KEY = "CommandKey";
  protected static final String XSI_TYPE = "xsi:type";
  protected static final String XSD_STRING = "xsd:string";
  protected static final String XSD_UNSIGNEDINT = "xsd:unsignedInt";
  protected static final String XSD_INT = "xsd:int";
  protected static final String XSD_BOOLEAN = "xsd:boolean";
  protected static final String XSD_DATETIME = "xsd:dateTime";
  protected static final String XSD_BASE64 = "xsd:base64";
  protected static final String SOAP_ARRAY_TYPE = "SOAP-ENC:arrayType";
  public static final String FAULT_CODE = "FaultCode";
  public static final String FAULT_STRING = "FaultString";
  public static final String TYPE_OBJECT = "object";
  public static final String TYPE_STRING = "string";
  public static final String TYPE_BOOLEAN = "boolean";
  public static final String TYPE_DATETIME = "dateTime";
  public static final String TYPE_UNSIGNEDINT = "unsignedInt";
  public static final String TYPE_INT = "int";
  public static final String TYPE_BASE64 = "base64";

  public String getXmlType(String type) {
    if (type.equals(TYPE_BASE64)) {
      return TR069RPC.XSD_BASE64;
    } else if (type.equals(TYPE_BOOLEAN)) {
      return TR069RPC.XSD_BOOLEAN;
    } else if (type.equals(TYPE_DATETIME)) {
      return TR069RPC.XSD_DATETIME;
    } else if (type.equals(TYPE_INT)) {
      return TR069RPC.XSD_INT;
    } else if (type.equals(TYPE_OBJECT)) {
      return "";
    } else if (type.equals(TYPE_STRING)) {
      return TR069RPC.XSD_STRING;
    } else if (type.equals(TYPE_UNSIGNEDINT)) {
      return TR069RPC.XSD_UNSIGNEDINT;
    }
    return type;
  }
}
