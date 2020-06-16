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

package org.commscope.tr069adapter.netconf.notification;

import java.io.IOException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.netconf.rpc.CreateSubscription;
import org.opendaylight.netconf.api.NetconfMessage;
import org.opendaylight.netconf.api.xml.XmlNetconfConstants;
import org.opendaylight.netconf.api.xml.XmlUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

@Component
public class NetConfSessionUtil {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfSessionUtil.class);

  private static final String INDEX_STR = "index";
  private static final String INDEX_REGEX = "[0-9]{1,}";
  private static final String NS_URI = "urn:onf:otcc:wireless:yang:tr069-notification";

  public void sendNetConfNotification(DeviceInform notification) {
    NetconfMessage netconfMessage = convertToNetConfMessage(notification);
    LOG.debug("Notification converted to NetConf format {}", netconfMessage);
    CreateSubscription.sendNotification(netconfMessage,
        notification.getDeviceDetails().getDeviceId());
  }

  private NetconfMessage convertToNetConfMessage(DeviceInform notification) {
    try {
      String netConfXmlMsg = getNetconfResponseXML(notification);
      if (netConfXmlMsg == null)
        throw new IllegalArgumentException("There are no parameters found in the response");
      return new NetconfMessage(XmlUtil.readXmlToDocument(netConfXmlMsg));
    } catch (SAXException | IOException e) {
      throw new IllegalArgumentException("Cannot parse notifications", e);
    }
  }

  private static String getNetconfResponseXML(DeviceInform notification) {
    if (notification == null || notification.getParameters().isEmpty()) {
      LOG.debug("There are no parameters found in the response.");
      return null;
    }

    List<ParameterDTO> parameters = notification.getParameters();

    String result = null;
    try {
      DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
      docFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      docFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
      DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
      Document doc = docBuilder.newDocument();

      Map<String, Element> parentNodeMap = new HashMap<>();
      Element dataNode = null; // root of all nodes

      for (ParameterDTO paramDto : parameters) {
        String paramName = paramDto.getParamName();
        String paramValue = paramDto.getParamValue();
        StringTokenizer tokenizer = new StringTokenizer(paramName, ".");
        String parentNodeName = null;
        StringBuilder parentNodeKey = null;
        Element parentNode = null;
        while (tokenizer.hasMoreElements()) {
          String nodeName = (String) tokenizer.nextElement();
          if (null == parentNodeName) { // construct first node or
                                        // Device node
            parentNodeName = nodeName;
            parentNodeKey = new StringBuilder(nodeName);
            // check if the node already exists in parentNodeMap
            parentNode = parentNodeMap.get(parentNodeKey.toString());
            if (null == dataNode) {
              dataNode = parentNode;
            }
          } else if (nodeName.matches(INDEX_REGEX)) { // construct
                                                      // tabular and
                                                      // index nodes

            // get parent tabular node from parent MAP
            parentNodeKey = parentNodeKey.append(".").append(nodeName);
            Element node = parentNodeMap.get(parentNodeKey.toString());

            // create a tabular parent node if doesn't exit in MAP
            if (null == node) {
              node = doc.createElement(parentNodeName);
              parentNodeMap.put(parentNodeKey.toString(), node);

              // update current tabular parent node.
              if (null != parentNode)
                parentNode.appendChild(node);

              // prepare and add index node to tabular parent node
              Element indexNode = doc.createElement(INDEX_STR);
              indexNode.setTextContent(nodeName);
              node.appendChild(indexNode);
            }
            parentNode = node; // move parent to child
            parentNodeName = nodeName;
          } else if (parentNodeName.matches(INDEX_REGEX)) { // move to
                                                            // next
                                                            // node
                                                            // if
                                                            // tabular
                                                            // attribute
                                                            // is
                                                            // found
            parentNodeKey = parentNodeKey.append(".").append(nodeName);
            parentNodeName = nodeName;
          } else {
            // construct intermediate nodes
            Element node = parentNodeMap.get(parentNodeKey.toString());
            if (null == node) {
              if (null == dataNode) {
                node = doc.createElementNS(NS_URI, parentNodeName);
                dataNode = node;
              } else {
                node = doc.createElement(parentNodeName);
              }
              parentNodeMap.put(parentNodeKey.toString(), node);
              if (null != parentNode)
                parentNode.appendChild(node);
            }
            parentNodeKey = parentNodeKey.append(".").append(nodeName);
            parentNodeName = nodeName;
            parentNode = node;
          }
        }
        // construct leaf node
        Element leafNode = doc.createElement(parentNodeName);
        leafNode.setTextContent(paramValue);
        if (null != parentNode)
          parentNode.appendChild(leafNode);
      }

      if (null != dataNode) {
        final Element element = doc.createElement(XmlNetconfConstants.NOTIFICATION_ELEMENT_NAME);
        final Element eventTime = doc.createElement(XmlNetconfConstants.EVENT_TIME);
        eventTime
            .setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX").format(new Date()));

        final Element evtTypeElement =
            doc.createElementNS(NS_URI, notification.getInformTypeList().get(0).toString());
        evtTypeElement.appendChild(dataNode);
        element.appendChild(element.getOwnerDocument().importNode(eventTime, true));
        element.appendChild(element.getOwnerDocument().importNode(evtTypeElement, true));
        result = convertDocumentToString(element);
      }
    } catch (ParserConfigurationException pce) {
      LOG.error("Error while getNetconfResponseXML {}", pce.toString());
    }

    return result;
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
    LOG.debug("Converted XML is : {}", strxml);
    return strxml;
  }

}
