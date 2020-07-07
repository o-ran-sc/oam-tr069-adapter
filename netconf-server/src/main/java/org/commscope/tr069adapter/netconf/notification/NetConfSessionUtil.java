package org.commscope.tr069adapter.netconf.notification;

import java.io.IOException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.model.NetConfNotificationDTO;
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

  public void sendNetConfNotification(NetConfNotificationDTO netConNotifDTO) {
    NetconfMessage netconfMessage = convertToNetConfMessage(netConNotifDTO);
    LOG.debug("Notification converted to NetConf format" + netconfMessage);
    CreateSubscription.sendNotification(netconfMessage, netConNotifDTO.getDeviceID());
  }

  private NetconfMessage convertToNetConfMessage(NetConfNotificationDTO netConNotifDTO) {
    try {
      String nameSpace = "";
      if (netConNotifDTO.getUri() != null) {
        nameSpace = netConNotifDTO.getUri();
      } else {
        nameSpace = NS_URI;
      }
      return new NetconfMessage(
          XmlUtil.readXmlToDocument(getNetconfResponseXML(netConNotifDTO, nameSpace)));
    } catch (SAXException | IOException e) {
      throw new IllegalArgumentException("Cannot parse notifications", e);
    }
  }

  private static String getNetconfResponseXML(NetConfNotificationDTO netConNotifDTO,
      String nameSpace) {
    if (netConNotifDTO == null || netConNotifDTO.getParameters().isEmpty()) {
      LOG.debug("There are no parameters found in the response.");
      return null;
    }

    List<ParameterDTO> parameters = netConNotifDTO.getParameters();

    String result = null;
    try {
      DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
      Document doc = docBuilder.newDocument();

      Map<String, Element> parentNodeMap = new HashMap<>();
      Element dataNode = null; // root of all nodes

      for (ParameterDTO paramDto : parameters) {
        String paramName = paramDto.getParamName();
        String paramValue = paramDto.getParamValue();
        StringTokenizer tokenizer = new StringTokenizer(paramName, ".");
        String parentNodeName = null;
        String parentNodeKey = null;
        Element parentNode = null;
        while (tokenizer.hasMoreElements()) {
          String nodeName = (String) tokenizer.nextElement();
          if (null == parentNodeName) { // construct first node or
                                        // Device node
            parentNodeName = nodeName;
            parentNodeKey = nodeName;
            // check if the node already exists in parentNodeMap
            parentNode = parentNodeMap.get(parentNodeKey);
            if (null == dataNode) {
              dataNode = parentNode;
            }
            continue;
          } else if (nodeName.matches(INDEX_REGEX)) { // construct
                                                      // tabular and
                                                      // index nodes

            // get parent tabular node from parent MAP
            parentNodeKey = parentNodeKey + "." + nodeName;
            Element node = parentNodeMap.get(parentNodeKey);

            // create a tabular parent node if doesn't exit in MAP
            if (null == node) {
              node = doc.createElement(parentNodeName);
              parentNodeMap.put(parentNodeKey, node);

              // update current tabular parent node.
              if (null != parentNode)
                parentNode.appendChild(node);
              else
                parentNode = node;

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
            parentNodeKey = parentNodeKey + "." + nodeName;
            parentNodeName = nodeName;
          } else {
            // construct intermediate nodes
            Element node = parentNodeMap.get(parentNodeKey);
            if (null == node) {
              if (null == dataNode) {
                node = doc.createElementNS(nameSpace, parentNodeName);
                dataNode = node;
              } else {
                node = doc.createElement(parentNodeName);
              }
              parentNodeMap.put(parentNodeKey, node);
              if (null != parentNode)
                parentNode.appendChild(node);
            }
            parentNodeKey = parentNodeKey + "." + nodeName;
            parentNodeName = nodeName;
            parentNode = node;
          }
        }
        // construct leaf node
        Element leafNode = doc.createElement(parentNodeName);
        leafNode.setTextContent(paramValue);
        parentNode.appendChild(leafNode);
      }

      if (null != dataNode) {
        final Element element = doc.createElement(XmlNetconfConstants.NOTIFICATION_ELEMENT_NAME);
        final Element eventTime = doc.createElement(XmlNetconfConstants.EVENT_TIME);
        eventTime
            .setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX").format(new Date()));
        element.appendChild(element.getOwnerDocument().importNode(eventTime, true));

        if (netConNotifDTO.getNotificationType() != null) {
          final Element evtTypeElement =
              doc.createElementNS(nameSpace, netConNotifDTO.getNotificationType());
          evtTypeElement.appendChild(dataNode);
          element.appendChild(element.getOwnerDocument().importNode(evtTypeElement, true));
        } else {
          element.appendChild(element.getOwnerDocument().importNode(dataNode, true));
        }

        result = convertDocumentToString(element);
      }
    } catch (ParserConfigurationException pce) {
      pce.printStackTrace();
    }

    return result;
  }

  public static String convertDocumentToString(Element element) {
    String strxml = null;
    try {
      TransformerFactory transformerFactory = TransformerFactory.newInstance();
      Transformer transformer = transformerFactory.newTransformer();
      DOMSource source = new DOMSource(element);
      StreamResult result = new StreamResult(new StringWriter());
      transformer.transform(source, result);
      strxml = result.getWriter().toString();
    } catch (Exception e) {
      LOG.error("Error while converting Element to String" + e);
    }
    LOG.debug("Converted XML is : " + strxml);
    return strxml;
  }

}
