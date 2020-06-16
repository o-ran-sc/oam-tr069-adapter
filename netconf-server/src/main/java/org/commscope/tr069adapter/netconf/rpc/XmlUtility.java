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

import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.commscope.tr069adapter.mapper.model.NetConfRequest;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.opendaylight.netconf.api.xml.XmlElement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.web.client.RestTemplate;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

public class XmlUtility {
  private static final Logger logger = LoggerFactory.getLogger(XmlUtility.class);

  private XmlUtility() {}

  private static final Logger LOG = LoggerFactory.getLogger(XmlUtility.class);

  public static String convertDocumentToString(XmlElement element) {
    return convertDocumentToString(element.getDomElement());
  }

  public static String convertDocumentToString(Element element) {
    String strxml = null;
    try {
      TransformerFactory transformerFactory = TransformerFactory.newInstance();
      transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
      Transformer transformer = transformerFactory.newTransformer();
      DOMSource source = new DOMSource(element);
      StreamResult result = new StreamResult(new StringWriter());
      transformer.transform(source, result);
      strxml = result.getWriter().toString();
    } catch (Exception e) {
      LOG.error("Error while converting Element to String {}", e.toString());
    }

    return strxml;
  }

  public static Element convertStringToDocument(String xmlStr) {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
    DocumentBuilder builder;
    try {
      builder = factory.newDocumentBuilder();
      Document doc = builder.parse(new InputSource(new StringReader(xmlStr)));
      return doc.getDocumentElement();
    } catch (Exception e) {
      LOG.error("Error while converting String to element {}", e.toString());
    }
    return null;
  }

  public static NetConfResponse invokeMapperCall(String requestUrl, String requestXml,
      String deviceID) {
    URI uri = null;
    try {
      uri = new URI(requestUrl);
    } catch (URISyntaxException e) {
      logger.error("invalid URI {}", e.toString());
    }

    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    NetConfRequest req = new NetConfRequest(requestXml, deviceID);

    HttpEntity<NetConfRequest> entity = new HttpEntity<>(req, headers);
    NetConfResponse restResponse = null;
    if (uri != null) {
      restResponse = restTemplate.postForObject(uri, entity, NetConfResponse.class);
    }

    return restResponse;
  }
}
