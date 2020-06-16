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
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.soap.Name;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class Inform extends TR069RPC {

  private static final long serialVersionUID = 929353181551497549L;

  public class Event implements Entry<String, String> {

    private String eventCode;
    private String cmdKey;

    public Event(String event, String cmdKey) {
      this.eventCode = event;
      this.cmdKey = cmdKey;
    }

    public String getKey() {
      return eventCode;
    }

    public String getValue() {
      return cmdKey;
    }

    public String setValue(String value) {
      cmdKey = value;
      return cmdKey;
    }
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    throw new UnsupportedOperationException();
  }

  @SuppressWarnings("rawtypes")
  protected void parseBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    SOAPElement deviceid = getRequestChildElement(spf, body, "DeviceId");
    defns = deviceid.getNamespaceURI();

    oui = getRequestElement(spf, deviceid, "OUI");
    sn = getRequestElement(spf, deviceid, "SerialNumber");
    manufacturer = getRequestElement(spf, deviceid, "Manufacturer");
    productClass = getRequestElement(spf, deviceid, "ProductClass");
    if (productClass == null) {
      productClass = "";
    }
    maxEnvelopes = Integer.parseInt(getRequestElement(spf, body, "MaxEnvelopes"));
    retryCount = Integer.parseInt(getRequestElement(spf, body, "RetryCount"));
    currentTime = getRequestElement(spf, body, "CurrentTime");

    Iterator<SOAPElement> pi = getRequestChildElement(spf, body, "ParameterList")
        .getChildElements(spf.createName("ParameterValueStruct"));
    Name nameKey = spf.createName("Name");
    Name nameValue = spf.createName("Value");
    params = new HashMap<>();
    while (pi.hasNext()) {
      SOAPElement param = pi.next();
      String key = getRequestElement(param, nameKey);
      if (root == null && !key.startsWith(".")) {
        if (key.startsWith("Device.")) {
          root = "Device";
        } else {
          root = "InternetGatewayDevice";
        }
      }
      String value = "";
      try {
        value = getRequestElement(param, nameValue);
        value = setEmptyValueIfNull(value);
      } catch (Exception e) {
        logger.error("Exception while Inform parseBody. {}", e.toString());
      }
      params.put(key, value);
    }

    if (root == null) {
      throw new SOAPException("Invalid root. Must be InternetGatewayDevice or Device");
    }

    pi = getRequestChildElement(spf, body, "Event").getChildElements(spf.createName("EventStruct"));
    Name eventCode = spf.createName("EventCode");
    Name commandKey = spf.createName(COMMAND_KEY);
    events = new LinkedHashSet<>();
    while (pi.hasNext()) {
      SOAPElement param = pi.next();
      String event = getRequestElement(param, eventCode);
      String cmdKey = getRequestElement(param, commandKey);
      cmdKey = setEmptyValueIfNull(cmdKey);
      events.add(new Event(event, cmdKey));
    }

  }

  private String setEmptyValueIfNull(String value) {
    if (value == null)
      value = "";

    return value;
  }

  public String getSoftwareVersion() {
    String v = params.get(root + ".DeviceInfo.SoftwareVersion");
    if (v != null) {
      v = v.replace('-', '.');
      v = v.replace(',', ' ');
    }
    return v;
  }

  public String getHardwareVersion() {
    return params.get(root + ".DeviceInfo.HardwareVersion");
  }

  public String getConfigVersion() {
    return params.get(root + ".DeviceInfo.VendorConfigFile.1.Version");
  }

  public String getURL() {
    String url = params.get(root + ".ManagementServer.ConnectionRequestURL");
    if (url != null) {
      return url;
    }
    url = params.get(root + ".ManagementServer.UDPConnectionRequestAddress");
    if (url != null) {
      url = (url.indexOf(':') == -1) ? "udp://" + url + ":80" : "udp://" + url;
    }
    return url;
  }

  public String getConreqUser() {
    return params.get(root + ".ManagementServer.ConnectionRequestUsername");
  }

  public String getConreqPass() {
    return params.get(root + ".ManagementServer.ConnectionRequestPassword");
  }

  public String getProvisiongCode() {
    return params.get(root + ".DeviceInfo.ProvisioningCode");
  }

  public void setProvisiongCode(String code) {
    params.put(root + ".DeviceInfo.ProvisioningCode", code);
  }

  public String getRoot() {
    return root;
  }

  public boolean isEvent(String event) {
    for (Entry<String, String> e : events) {
      if (e.getKey().equals(event)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public String toString() {
    StringBuilder s = new StringBuilder(1024);
    s.append("Inform:\n");
    println(s, "\toui: ", oui);
    println(s, "\tsn: ", sn);
    println(s, "\tManufacturer: ", manufacturer);

    s.append("\tEvents:\n");
    for (Entry<String, String> ev : events) {
      println(s, "\t\t", ev.getKey(), ev.getValue());
    }

    s.append("\tParams:\n");
    for (Entry<String, String> k : params.entrySet()) {
      println(s, "\t\t", k.getKey(), k.getValue());
    }
    return s.toString();
  }

  private String oui;
  private String sn;
  private String productClass;
  private String manufacturer;
  private int retryCount;
  private String currentTime;
  private Map<String, String> params;
  private transient Set<Entry<String, String>> events;
  private transient Set<Entry<String, String>> response;
  private int maxEnvelopes;
  private String defns;
  private String root = null;
  public static final String EVENT_BOOT_STRAP = "0 BOOTSTRAP";
  public static final String EVENT_BOOT = "1 BOOT";
  public static final String EVENT_PERIODIC = "2 PERIODIC";
  public static final String EVENT_SCHEDULED = "3 SCHEDULED";
  public static final String EVENT_VALUE_CHANGE = "4 VALUE CHANGE";
  public static final String EVENT_KICKED = "5 KICKED";
  public static final String EVENT_CONNECTION_REQUEST = "6 CONNECTION REQUEST";
  public static final String EVENT_TRANSFER_COMPLETE = "7 TRANSFER COMPLETE";

  public String getOui() {
    return oui;
  }

  public void setOui(String oui) {
    this.oui = oui;
  }

  public String getSn() {
    return sn;
  }

  public void setSn(String sn) {
    this.sn = sn;
  }

  public String getProductClass() {
    return productClass;
  }

  public void setProductClass(String productClass) {
    this.productClass = productClass;
  }

  public String getManufacturer() {
    return manufacturer;
  }

  public void setManufacturer(String manufacturer) {
    this.manufacturer = manufacturer;
  }

  public int getRetryCount() {
    return retryCount;
  }

  public void setRetryCount(int retryCount) {
    this.retryCount = retryCount;
  }

  public String getCurrentTime() {
    return currentTime;
  }

  public void setCurrentTime(String currentTime) {
    this.currentTime = currentTime;
  }

  public Map<String, String> getParams() {
    return params;
  }

  public void setParams(Map<String, String> params) {
    this.params = params;
  }

  public Set<Entry<String, String>> getResponse() {
    return response;
  }

  public void setResponse(Set<Entry<String, String>> response) {
    this.response = response;
  }

  public int getMaxEnvelopes() {
    return maxEnvelopes;
  }

  public void setMaxEnvelopes(int maxEnvelopes) {
    this.maxEnvelopes = maxEnvelopes;
  }

  public Set<Entry<String, String>> getEvents() {
    return events;
  }

  public void setEvents(Set<Entry<String, String>> events) {
    this.events = events;
  }

  public String getDefns() {
    return defns;
  }

  public void setDefns(String defns) {
    this.defns = defns;
  }
}
