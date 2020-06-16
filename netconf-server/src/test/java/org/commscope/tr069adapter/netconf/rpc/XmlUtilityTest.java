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

package org.commscope.tr069adapter.netconf.rpc;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

class XmlUtilityTest {

  @Test
  void testConvertDocumentToStringXmlElement() {

    String xmlStr =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc/></lte></cell-config></fap-service></services></device></filter></get-config>";
    try {
      Element el = XmlUtility.convertStringToDocument(xmlStr);
      String result = XmlUtility.convertDocumentToString(el);
      assertTrue(result != null);
    } catch (Exception e) {
      fail("Failed to convert string into document.");
    }
  }

  @Test
  void testConvertStringToDocument() {
    String xmlStr =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rpc-reply xmlns=\"URN\" xmlns:junos=\"URL\"><data/></rpc-reply>";
    try {
      XmlUtility.convertStringToDocument(xmlStr);
    } catch (Exception e) {
      fail("Failed to convert string into document.");
    }
    assertTrue(true);
  }

}
