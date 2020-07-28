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

package org.commscope.tr069adapter.mapper.netconf;

import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
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
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.model.NetConfNotificationDTO;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

@Component
public class NetConfNotificationSender {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfNotificationSender.class);
  private static final String BOOLEAN_TRUE_VALUE = "1";
  private static final String BOOLEAN_FALSE_VALUE = "0";
  private static final String BOOLEAN_DATA_TYPE = "boolean";
  public static final String NOTIFICATION_ELEMENT_NAME = "notification";
  public static final String EVENT_TIME = "eventTime";
  private static final String INDEX_STR = "index";
  private static final String INDEX_REGEX = "[0-9]{1,}";


  @Autowired
  MapperConfigProperties config;

  @Autowired
  MOMetaDataUtil metaDataUtil;

  @Autowired
  RestTemplate restTemplate;

  public ResponseEntity sendNotification(DeviceInform deviceInform) {
    ResponseEntity response = null;
    final String uri = getUri();
    LOG.debug("Posting notification to netconf server {}", uri);

    try {
      LOG.debug("deviceInform : {} {}", deviceInform.getInformTypeList(),
          deviceInform.getParameters());
      List<ParameterDTO> parameters = new ArrayList<>();
      for (ParameterDTO parameterDTO : deviceInform.getParameters()) {
        parameters.add(parameterDTO);
      }

      convertTR069ToNetConfParams(parameters, deviceInform.getDeviceDetails().getSoftwareVersion(),
          deviceInform.getDeviceDetails().getHardwareVersion());

      String notificationXml =
          getNetconfResponseXML(parameters, deviceInform.getInformType().toString(), null,
              deviceInform.getDeviceDetails().getSoftwareVersion(),
              deviceInform.getDeviceDetails().getHardwareVersion());
      NetConfNotificationDTO netConfDTO = new NetConfNotificationDTO(
          deviceInform.getDeviceDetails().getDeviceId(), notificationXml);

      LOG.debug("Posting notification to netconf server");
      response = restTemplate.postForObject(uri, netConfDTO, ResponseEntity.class);
      LOG.debug("Posting notification to netconf server completed ");
    } catch (Exception e) {
      LOG.error("Exception while sending the notification.", e);
    }
    return response;
  }

  public ResponseEntity sendCustomNotification(String deviceId, List<ParameterDTO> parameters,
      String nameSpace) {
    ResponseEntity response = null;
    final String uri = getUri();
    LOG.debug("Posting custom notification to netconf server " + uri);
    try {
      String notificationXml = getNetconfResponseXML(parameters, null, nameSpace, null, null);
      NetConfNotificationDTO netConfDTO = new NetConfNotificationDTO(deviceId, notificationXml);

      response = restTemplate.postForObject(uri, netConfDTO, ResponseEntity.class);
      LOG.debug("Posting custom notification to netconf server sucessfull");
    } catch (Exception e) {
      LOG.error("Exception while sending the custom notification.", e.toString());
    }
    return response;
  }

  private void convertTR069ToNetConfParams(List<ParameterDTO> parameters, String swVersion,
      String hwVersion) {
    List<ParameterDTO> removeList = new ArrayList<>();
    if (null != parameters) {
      for (ParameterDTO param : parameters) {
        if (param.getParamValue() == null || param.getParamValue().trim().length() <= 0) {
          continue;
        }
        handleBooleanParameters(param, swVersion, hwVersion);
        if (null != param.getParamName()) {
          String netConfMOName = metaDataUtil
              .getNetconfNameByTR69NameWithIndexes(param.getParamName(), swVersion, hwVersion);
          if (null != netConfMOName)
            param.setParamName(netConfMOName);
          else
            removeList.add(param); // unknown parameter found.
        }
      }
      parameters.removeAll(removeList);
    }
  }

  private void handleBooleanParameters(ParameterDTO param, String swVersion, String hwVersion) {
    MOMetaData metaData =
        metaDataUtil.getMetaDataByTR69Name(param.getParamName(), swVersion, hwVersion);
    if (null != metaData && BOOLEAN_DATA_TYPE.equalsIgnoreCase(metaData.getDataType())) {
      if (BOOLEAN_TRUE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
        param.setParamValue(Boolean.TRUE.toString());
      } else if (BOOLEAN_FALSE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
        param.setParamValue(Boolean.FALSE.toString());
      }
    }
  }

  private String getUri() {
    return config.getNbiNotificationUri();
  }

  private String getNetconfResponseXML(List<ParameterDTO> parameters, String notificationType,
      String nameSpace, String swVersion, String hwVersion) {
    if (parameters == null || parameters.isEmpty()) {
      LOG.debug("There are no parameters found in the response.");
      return null;
    }

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
        final Element element = doc.createElement(NOTIFICATION_ELEMENT_NAME);
        final Element eventTime = doc.createElement(EVENT_TIME);
        eventTime
            .setTextContent(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX").format(new Date()));
        element.appendChild(element.getOwnerDocument().importNode(eventTime, true));

        if (notificationType != null) {
          if (nameSpace == null && metaDataUtil.getMetaDataByTR69Name(notificationType, swVersion,
              hwVersion) != null) {
            nameSpace =
                metaDataUtil.getMetaDataByTR69Name(notificationType, swVersion, hwVersion).getURI();
          }

          final Element evtTypeElement = doc.createElementNS(nameSpace, notificationType);
          evtTypeElement.appendChild(dataNode);
          element.appendChild(element.getOwnerDocument().importNode(evtTypeElement, true));
        } else {
          element.appendChild(element.getOwnerDocument().importNode(dataNode, true));
        }

        result = convertDocumentToString(element);
      }
    } catch (ParserConfigurationException pce) {
      LOG.error("Error occured while preparing the notification");
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
