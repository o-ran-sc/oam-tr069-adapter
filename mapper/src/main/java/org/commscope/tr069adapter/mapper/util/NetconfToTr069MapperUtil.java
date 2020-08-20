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

package org.commscope.tr069adapter.mapper.util;

import java.io.Serializable;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
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

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationOptions;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.ParameterAttributeDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.model.ErrorCodeDetails;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

@Component
public class NetconfToTr069MapperUtil {

  @Autowired
  MOMetaDataUtil metaDataUtil;

  @Autowired
  MapperConfigProperties config;

  private static final Logger logger = LoggerFactory.getLogger(NetconfToTr069MapperUtil.class);
  private static final String INDEX_STR = "index";
  private static final String INDEX_REGEX = "[0-9]{1,}";
  private static final String APPLICATION = "application";
  private static final String OPERATION_FAILED = "operation-failed";
  private static final String ERROR = "ERROR";
  private static final String OPERATION_ABORTED = "Operation Aborted";
  private static final String INDEX_NODE_FOUND = "Index node found : {}";

  @Autowired
  private ErrorCodeUtil errorCodeUtil;

  public static Element convertStringToDocument(String xmlStr) {
    try {
      Document doc = convertStringToDocumentXml(xmlStr);
      if (null != doc)
        return doc.getDocumentElement();
    } catch (Exception e) {
      logger.error("Error while converting String to element {}", e.getMessage());
    }
    return null;
  }

  public static Document convertStringToDocumentXml(String xmlStr) {
    try {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
      DocumentBuilder builder;
      builder = factory.newDocumentBuilder();
      return builder.parse(new InputSource(new StringReader(xmlStr)));
    } catch (Exception e) {
      logger.error("Error while converting String to element {}", e.getMessage());
    }
    return null;
  }

  public static Map<String, String> extractRequestParamters(Document operationElement,
      String netconfTag, String filterElement) {
    Node requestDataNode = getParameterDataNode(operationElement, netconfTag, filterElement);
    Map<String, String> result = new HashMap<>();
    try {
      result = getParameterMapForNode(requestDataNode, -1);
    } catch (Exception e) {
      logger.error("Error while getting parameter Map {}", e.getMessage());
    }
    return result;
  }

  public static DeviceRPCRequest prepareTR069Request(String deviceId, Element operationElement,
      String netconfTag, TR069OperationCode opCode) {
    Node requestDataNode = getDeviceDataNode(operationElement, netconfTag);
    if (requestDataNode == null) {
      logger.debug("No matching device parameters found in the netconf request XML.");
      return null;
    }
    Map<String, String> map = getParameterMapForNode(requestDataNode, -1);
    List<ParameterDTO> params = new ArrayList<>();

    for (java.util.Map.Entry<String, String> entry : map.entrySet()) {
      if (!entry.getKey().equalsIgnoreCase("filter")) {
        String moName = entry.getKey();
        String value = entry.getValue();
        if (moName.endsWith("." + INDEX_STR)
            && (TR069OperationCode.GET_PARAMETER_VALUES.equals(opCode)
                || TR069OperationCode.DELETE_OBJECT.equals(opCode))) {
          logger.debug(INDEX_NODE_FOUND, moName);
          moName = moName.replaceFirst("." + INDEX_STR, ".");
          value = null;

        }

        ParameterDTO param = new ParameterDTO(moName, value);
        params.add(param);
      }
    }

    TR069OperationDetails opDetails = new TR069OperationDetails();
    opDetails.setOpCode(opCode);
    return handleParamsOperation(params, opDetails, deviceId);
  }

  public static DeviceRPCRequest handleParamsOperation(List<ParameterDTO> params,
      TR069OperationDetails opDetails, String deviceId) {
    opDetails.setParmeters(params);

    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
    tr069DeviceDetails.setDeviceId(deviceId);
    deviceRPCRequest.setOpDetails(opDetails);
    deviceRPCRequest.setDeviceDetails(tr069DeviceDetails);
    OperationOptions options = new OperationOptions();
    options.setExecutionTimeout(60l);
    deviceRPCRequest.setOptions(options);
    return deviceRPCRequest;
  }

  public NetConfResponse getNetconfResponse(DeviceRPCResponse opResult, String swVersion,
      String hwVersion, boolean isCustomparameter) {
    NetConfResponse netConfResponse = new NetConfResponse();
    ErrorCodeDetails errorCodeDetails = errorCodeUtil.getErrorCodeMetaData(opResult.getFaultKey());
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    if (errorCodeDetails != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(errorCodeDetails.getErrorMessage());
      errorCode.setErrorType(errorCodeDetails.getErrorType());
      errorCode.setErrorTag(errorCodeDetails.getErrorTag());
      errorCode.setErrorSeverity(errorCodeDetails.getErrorSeverity());
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
    } else if (opResult.getFaultKey() != null && opResult.getFaultString() != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(opResult.getFaultString());
      errorCode.setErrorType(APPLICATION);
      errorCode.setErrorTag(OPERATION_FAILED);
      errorCode.setErrorSeverity(ERROR);
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
    }
    netConfResponse.setNetconfResponseXml(
        getNetconfResponseXML(opResult.getOperationResponse().getParameterDTOs(), swVersion,
            hwVersion, isCustomparameter));
    return netConfResponse;
  }

  public NetConfResponse getNetconfResponseForSoftwareInventory(DeviceRPCResponse opResult,
      String swVersion, String hwVersion) {

    NetConfResponse netConfResponse = new NetConfResponse();
    ErrorCodeDetails errorCodeDetails = errorCodeUtil.getErrorCodeMetaData(opResult.getFaultKey());
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    if (errorCodeDetails != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(errorCodeDetails.getErrorMessage());
      errorCode.setErrorType(errorCodeDetails.getErrorType());
      errorCode.setErrorTag(errorCodeDetails.getErrorTag());
      errorCode.setErrorSeverity(errorCodeDetails.getErrorSeverity());
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
      return netConfResponse;
    } else if (opResult.getFaultKey() != null && opResult.getFaultString() != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(opResult.getFaultString());
      errorCode.setErrorType(APPLICATION);
      errorCode.setErrorTag(OPERATION_FAILED);
      errorCode.setErrorSeverity(ERROR);
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
      return netConfResponse;
    }
    List<ParameterDTO> paramDTOList = new ArrayList<>();

    String build = null;
    String productClass = null;
    for (ParameterDTO paramDto : opResult.getOperationResponse().getParameterDTOs()) {
      if (paramDto.getParamName().equals("Device.DeviceInfo.SoftwareVersion"))
        build = paramDto.getParamValue();
      else if (paramDto.getParamName().equals("Device.DeviceInfo.ProductClass"))
        productClass = paramDto.getParamValue();
    }

    String[] arrOfBuild = build.split("\\.");
    String buildId = arrOfBuild[arrOfBuild.length - 1];
    StringBuilder buildVersion = new StringBuilder();
    for (int i = 0; i < arrOfBuild.length - 1; i++) {
      if (i == arrOfBuild.length - 2) {
        buildVersion.append(arrOfBuild[i]);
      } else {
        buildVersion.append(arrOfBuild[i]);
        buildVersion.append(".");
      }
    }

    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.name", "Active Partition"));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.status", "VALID"));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.active", "true"));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.running", "true"));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.access", "READ_ONLY"));
    paramDTOList
        .add(new ParameterDTO("software-inventory.software-slot.product-code", productClass));
    paramDTOList.add(
        new ParameterDTO("software-inventory.software-slot.vendor-code", config.getVendorName()));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.build-id", buildId));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.build-version",
        buildVersion.toString()));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.files.name", "BC_ONE"));
    paramDTOList.add(new ParameterDTO("software-inventory.software-slot.files.integrity", "OK"));

    String xmlStr = getNetconfResponseXML(paramDTOList, swVersion, hwVersion, true);

    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
    DocumentBuilder builder;
    Document doc = null;
    try {
      builder = factory.newDocumentBuilder();
      doc = builder.parse(new InputSource(new StringReader(xmlStr)));
    } catch (Exception e) {
      logger.error("Error while converting String to element", e);
      errorCode.setFaultCode("8002");
      errorCode.setErrorMessage(OPERATION_ABORTED);
      errorCode.setErrorType(APPLICATION);
      errorCode.setErrorTag(OPERATION_FAILED);
      errorCode.setErrorSeverity(ERROR);
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(OPERATION_ABORTED);
      return netConfResponse;
    }

    Element originalDocumentElement = doc.getDocumentElement();
    Element newDocumentElement = doc.createElementNS("urn:o-ran:software-management:1.0",
        originalDocumentElement.getNodeName());
    NodeList list = originalDocumentElement.getChildNodes();
    while (list.getLength() != 0) {
      newDocumentElement.appendChild(list.item(0));
    }
    // Replace the original element
    doc.replaceChild(newDocumentElement, originalDocumentElement);

    String strxml = null;
    try {
      TransformerFactory transformerFactory = TransformerFactory.newInstance();
      transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      transformerFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
      Transformer transformer = transformerFactory.newTransformer();
      DOMSource source = new DOMSource(doc);
      StreamResult result = new StreamResult(new StringWriter());
      transformer.transform(source, result);
      strxml = result.getWriter().toString();
    } catch (Exception e) {
      logger.error("Error while converting Element to String", e);
      errorCode.setFaultCode("8002");
      errorCode.setErrorMessage(OPERATION_ABORTED);
      errorCode.setErrorType(APPLICATION);
      errorCode.setErrorTag(OPERATION_FAILED);
      errorCode.setErrorSeverity(ERROR);
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(OPERATION_ABORTED);
      return netConfResponse;
    }

    netConfResponse.setNetconfResponseXml(strxml);
    logger.debug("NetConf Response XML String for software inventory:{} ", strxml);
    return netConfResponse;
  }

  public NetConfResponse getNetconfResponseForRequestWithoutInputParams(
      DeviceRPCResponse opResult) {
    NetConfResponse netConfResponse = new NetConfResponse();
    ErrorCodeDetails errorCodeDetails = errorCodeUtil.getErrorCodeMetaData(opResult.getFaultKey());
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    if (errorCodeDetails != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(errorCodeDetails.getErrorMessage());
      errorCode.setErrorType(errorCodeDetails.getErrorType());
      errorCode.setErrorTag(errorCodeDetails.getErrorTag());
      errorCode.setErrorSeverity(errorCodeDetails.getErrorSeverity());
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
    } else if (opResult.getFaultKey() != null && opResult.getFaultString() != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(opResult.getFaultString());
      errorCode.setErrorType(APPLICATION);
      errorCode.setErrorTag(OPERATION_FAILED);
      errorCode.setErrorSeverity(ERROR);
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
    }
    String xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rpc-reply><OK>true</OK></rpc-reply>";
    netConfResponse.setNetconfResponseXml(xml);
    return netConfResponse;
  }

  public NetConfResponse getNetconfResponseForGPA(DeviceRPCResponse opResult, String swVersion,
      String hwVersion) {
    NetConfResponse netConfResponse = new NetConfResponse();
    ErrorCodeDetails errorCodeDetails = errorCodeUtil.getErrorCodeMetaData(opResult.getFaultKey());
    ErrorCodeDetails errorCode = new ErrorCodeDetails();
    if (errorCodeDetails != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(errorCodeDetails.getErrorMessage());
      errorCode.setErrorType(errorCodeDetails.getErrorType());
      errorCode.setErrorTag(errorCodeDetails.getErrorTag());
      errorCode.setErrorSeverity(errorCodeDetails.getErrorSeverity());
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
    } else if (opResult.getFaultKey() != null && opResult.getFaultString() != null) {
      errorCode.setFaultCode(opResult.getFaultKey());
      errorCode.setErrorMessage(opResult.getFaultString());
      errorCode.setErrorType(APPLICATION);
      errorCode.setErrorTag(OPERATION_FAILED);
      errorCode.setErrorSeverity(ERROR);
      netConfResponse.setErrorCode(errorCode);
      netConfResponse.setErrorMessage(opResult.getFaultString());
    }
    netConfResponse.setNetconfResponseXml(getGPANetconfResponseXML(
        opResult.getOperationResponse().getParameterDTOs(), swVersion, hwVersion));
    return netConfResponse;
  }

  private String getGPANetconfResponseXML(List<ParameterDTO> parameters, String swVersion,
      String hwVersion) {

    try {
      DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
      docFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      docFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
      DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
      DOMImplementation impl = docBuilder.getDOMImplementation();
      Document doc = impl.createDocument(null, "rpc-reply", (DocumentType) null);
      Element parent = doc.getDocumentElement();
      Element child = null;
      String result = null;
      int i = 1;
      for (ParameterDTO param : parameters) {
        child = doc.createElement("ns1:data");
        Element keyNode = doc.createElement("ns1:keyindex");
        keyNode.setTextContent(String.valueOf(i));
        child.appendChild(keyNode);
        i++;
        List<ParameterDTO> paramList = new ArrayList<>();
        paramList.add(param);
        String paramName = metaDataUtil
            .getNetconfXPathNameByTR69NameWithIndexes(param.getParamName(), swVersion, hwVersion);
        Element parameterTag = doc.createElement("ns1:parameter");
        parameterTag.setTextContent(paramName);
        child.appendChild(parameterTag);
        String[] accesslist = ((ParameterAttributeDTO) param).getAccessList();
        int notification = ((ParameterAttributeDTO) param).getNotification();
        Element notfChild = doc.createElement("ns1:notification");
        notfChild.setTextContent(String.valueOf(notification));
        child.appendChild(notfChild);
        for (String access : accesslist) {
          Element accChild = doc.createElement("ns1:access-list");
          accChild.setTextContent(access);
          child.appendChild(accChild);
        }

        parent.appendChild(child);
      }
      result = convertDocumentToString(parent);
      return result;
    } catch (Exception e) {
      logger.error("Exception in getGPANetconfResponseXML: {}", e.getMessage());
    }
    return null;
  }

  public String getNetconfResponseXML(List<ParameterDTO> parameters, String swVersion,
      String hwVersion, boolean isCustomparameter) {
    if (null == parameters || parameters.isEmpty()) {
      logger.debug("There are no parameters found in the response.");
      return null;
    }
    Collections.sort(parameters, new SortByParamterName());

    String result = null;
    try {
      Document doc = null;
      Element dataNode = getNetconfResponseXMLElement(parameters, isCustomparameter, doc, false,
          swVersion, hwVersion);
      if (null != dataNode) {
        result = convertDocumentToString(dataNode);
      }
    } catch (Exception pce) {
      logger.error("Exception: {}", pce.getMessage());
    }

    return result;
  }

  private Element getNetconfResponseXMLElement(List<ParameterDTO> parameters,
      boolean isCustomparameter, Document doc, boolean isGPA, String swVersion, String hwVersion) {
    if (null == parameters || parameters.isEmpty()) {
      return null;
    }
    Collections.sort(parameters, new SortByParamterName());
    try {
      if (doc == null) {
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        docFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
        docFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
        doc = docBuilder.newDocument();
      }

      Map<String, Element> parentNodeMap = new HashMap<>();
      Element dataNode = null; // root of all nodes

      for (ParameterDTO paramDto : parameters) {
        String paramName = metaDataUtil.getNetconfNameByTR69NameWithIndexes(paramDto.getParamName(),
            swVersion, hwVersion);
        if (paramName == null && isCustomparameter) {
          paramName = paramDto.getParamName();
        }
        String paramValue = paramDto.getParamValue();

        if ((!isGPA) && (paramValue == null || paramValue.trim().isEmpty())) {
          logger.debug("Values is empty so skipping this parameter.");
          continue;
        }
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
            StringBuilder bld = new StringBuilder();
            bld.append(parentNodeKey);
            bld.append(".");
            bld.append(nodeName);
            parentNodeKey = bld.toString();
            Element node = parentNodeMap.get(parentNodeKey);

            // create a tabular parent node if doesn't exit in MAP
            if (null == node) {
              if ((!isGPA)
                  && metaDataUtil.getMetaDataByNetConfName(parentNodeKey + ".", swVersion,
                      hwVersion) != null
                  && metaDataUtil
                      .getMetaDataByNetConfName(parentNodeKey + ".", swVersion, hwVersion)
                      .getURI() != null) {
                node = doc.createElementNS(metaDataUtil
                    .getMetaDataByNetConfName(parentNodeKey + ".", swVersion, hwVersion).getURI(),
                    parentNodeName);
              } else {
                node = doc.createElement(parentNodeName);
              }
              parentNodeMap.put(parentNodeKey, node);

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
            StringBuilder bld = new StringBuilder();
            bld.append(parentNodeKey);
            bld.append(".");
            bld.append(nodeName);
            parentNodeKey = bld.toString();
            parentNodeName = nodeName;
          } else {
            // construct intermediate nodes
            Element node = parentNodeMap.get(parentNodeKey);
            if (null == node) {
              if ((!isGPA)
                  && metaDataUtil.getMetaDataByNetConfName(parentNodeKey, swVersion,
                      hwVersion) != null
                  && metaDataUtil.getMetaDataByNetConfName(parentNodeKey, swVersion, hwVersion)
                      .getURI() != null) {
                node = doc.createElementNS(metaDataUtil
                    .getMetaDataByNetConfName(parentNodeKey, swVersion, hwVersion).getURI(),
                    parentNodeName);

              } else {
                node = doc.createElement(parentNodeName);
              }
              if (dataNode == null)
                dataNode = node;
              parentNodeMap.put(parentNodeKey, node);
              if (null != parentNode)
                parentNode.appendChild(node);

            }
            StringBuilder bld = new StringBuilder();
            bld.append(parentNodeKey);
            bld.append(".");
            bld.append(nodeName);
            parentNodeKey = bld.toString();
            parentNodeName = nodeName;
            parentNode = node;
          }
        }
        // construct leaf node
        Element leafNode = doc.createElement(parentNodeName);
        leafNode.setTextContent(paramValue);
        if (parentNode != null)
          parentNode.appendChild(leafNode);
      }

      if (null != dataNode) {
        return dataNode;
      }
    } catch (ParserConfigurationException pce) {
      logger.error("Response xml formatting is failed : {} ", pce.toString());
    }

    return null;
  }

  public ParameterDTO getParamNameAndValueForGPA(Element operationElement, String netconfTag,
      String swVersion, String hwVersion) {
    Node deviceNode = getDeviceDataNode(operationElement, netconfTag);
    Map<String, String> map = null;
    try {
      map = getParameterMapForNode(deviceNode, -1);
    } catch (Exception e) {
      logger.error("Exception in getParamNameAndValueForGPA: {}", e.getMessage());
      return null;
    }
    ParameterDTO param = new ParameterDTO();
    for (String moName : map.keySet()) {
      if (moName.endsWith("." + INDEX_STR)) {
        logger.debug(INDEX_NODE_FOUND, moName);
        moName = moName.replaceFirst("." + INDEX_STR, ".");
      }
      String tr069Moname =
          metaDataUtil.getTR069NameByNetconfNameWithIndexes(moName, swVersion, hwVersion);
      param.setParamName(tr069Moname);
      param.setParamValue("");
    }
    return param;
  }

  public ParameterAttributeDTO getParamNameAndValueForSPA(Element operationElement,
      String netconfTag, String swVersion, String hwVersion) {
    Node requestDataNode = getDeviceDataNode(operationElement, netconfTag);
    if (requestDataNode == null) {
      logger.debug("No matching device parameters found in the netconf request XML.");
    }
    Node deviceNode = getDeviceDataNode(operationElement, "parameter");
    Map<String, String> map = new HashMap<>();
    try {
      map = getParameterMapForNode(deviceNode, -1);
    } catch (Exception e) {
      logger.error("Error while getting parameter Map {}", e.getMessage());
    }

    NodeList nodeList = operationElement.getChildNodes();
    int length = nodeList.getLength();
    String notificationVal = null;
    List<String> accessList = new ArrayList<>();
    String accesslistChange = null;
    String notificationChange = null;
    for (int i = 0; i < length; i++) {
      if (nodeList.item(i).getNodeName().equals("notification")) {
        notificationVal = nodeList.item(i).getTextContent();
      } else if (nodeList.item(i).getNodeName().equals("access-list")) {
        if (nodeList.item(i).getTextContent() != null
            && !(nodeList.item(i).getTextContent().isEmpty())) {
          accessList.add(nodeList.item(i).getTextContent());
        }
      } else if (nodeList.item(i).getNodeName().equals("access-list-change")) {
        accesslistChange = nodeList.item(i).getTextContent();
      } else if (nodeList.item(i).getNodeName().equals("notification-change")) {
        notificationChange = nodeList.item(i).getTextContent();
      }
    }
    String[] accList = accessList.toArray(new String[accessList.size()]);
    ParameterAttributeDTO param = new ParameterAttributeDTO();
    param.setAccessList(accList);
    param.setNotification(Integer.valueOf(notificationVal));
    param.setAccesslistChange(Boolean.parseBoolean(accesslistChange));
    param.setNotificationChange(Boolean.parseBoolean(notificationChange));

    for (String moName : map.keySet()) {
      if (moName.endsWith("." + INDEX_STR)) {
        logger.debug(INDEX_NODE_FOUND, moName);
        moName = moName.replaceFirst("." + INDEX_STR, ".");
      }
      String tr069Moname =
          metaDataUtil.getTR069NameByNetconfNameWithIndexes(moName, swVersion, hwVersion);
      param.setParamName(tr069Moname);
      param.setParamValue("");
    }

    return param;
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
      logger.error("Error while converting Element to String {}", e.toString());
    }
    logger.debug("Converted XML is : {} ", strxml);
    return strxml;
  }

  // TODO: Need to optimize the Node element logic for better performance
  private static Map<String, String> getParameterMapForNode(Node moRNode, int numberOfChilds) {
    Map<String, String> result = new HashMap<>();
    if (moRNode.getNodeType() == Node.ELEMENT_NODE) {
      NodeList childs = moRNode.getChildNodes();
      boolean hasChildElements = false;
      hasChildElements = checkMoreElements(result, childs, hasChildElements);
      if (!hasChildElements) {
        String moName = getMOName(moRNode);
        if (moName.contains("software-inventory")) {
          result.put("device.device-info.software-version", moRNode.getTextContent());
          result.put("device.device-info.product-class", moRNode.getTextContent());
        } else if (!moName.endsWith("." + INDEX_STR)) {
          result.put(moName, moRNode.getTextContent());
        } else if (numberOfChilds == 1) {
          result.put(moName, moRNode.getTextContent());
        }
      }
    }

    return result;
  }

  private static boolean checkMoreElements(Map<String, String> result, NodeList childs,
      boolean hasChildElements) {
    if (childs.getLength() > 0) {
      int counter = 0;
      for (int i = 0; i < childs.getLength(); i++) {
        Node cNode = childs.item(i);
        if (cNode != null && cNode.getNodeType() == Node.ELEMENT_NODE) {
          counter++;
        }
      }

      for (int i = 0; i < childs.getLength(); i++) {
        Node cNode = childs.item(i);
        if (cNode != null && cNode.getNodeType() == Node.ELEMENT_NODE) {
          hasChildElements = true;
          Map<String, String> subResult = getParameterMapForNode(cNode, counter);
          result.putAll(subResult);
        }
      }
    }
    return hasChildElements;
  }

  private static String getMOName(Node moRNode) {
    String result = removeNS(moRNode.getNodeName());
    Node pNode = moRNode;
    while (true) {
      pNode = pNode.getParentNode();
      if (pNode == null || pNode.getNodeType() != Node.ELEMENT_NODE
          || pNode.getNodeName().equals("edit-config") || pNode.getNodeName().equals("config")
          || pNode.getNodeName().equals("get-config") || pNode.getNodeName().equals("filter")
          || pNode.getNodeName().equals("get") || pNode.getNodeName().equals("parameter")) {
        return result;
      } else {
        String indexStr = getMOIndex(pNode);
        StringBuilder bld = new StringBuilder(removeNS(pNode.getNodeName()));
        bld.append(".");
        if (indexStr == null) {
          bld.append(result);
          result = bld.toString();
        } else {
          bld.append(indexStr);
          bld.append(".");
          bld.append(result);
          result = bld.toString();
        }
      }
    }
  }

  private static Node getParameterDataNode(Document el, String docStart, String filterElement) {
    NodeList nodeList = el.getElementsByTagName(docStart);
    if (nodeList.getLength() > 0) {
      nodeList = nodeList.item(0).getChildNodes();
      for (int i = 0; i < nodeList.getLength(); i++) {
        Node node = nodeList.item(i);
        String nodeName = removeNS(node.getNodeName());
        if (nodeName.equals(filterElement)) {
          return node;
        }
      }
    }
    return null;
  }

  private static Node getDeviceDataNode(Element el, String filter) {
    try {
      NodeList nodeList = el.getElementsByTagName(filter);
      if (nodeList.getLength() > 0) {
        nodeList = nodeList.item(0).getChildNodes();
        for (int i = 0; i < nodeList.getLength(); i++) {
          Node node = nodeList.item(i);
          if (node.getNodeType() == Node.ELEMENT_NODE) {
            return node;
          }
        }
      }
    } catch (Exception e) {
      logger.error("exception occured while parsing the request xml {}", e.getMessage());
    }
    return null;
  }

  private static String removeNS(String nodeName) {
    // remove name space
    int li = nodeName.lastIndexOf(':');
    nodeName = nodeName.substring(li + 1);
    return nodeName;
  }

  private static String getMOIndex(Node pNode) {
    if (null != pNode) {
      NodeList nodeList = pNode.getChildNodes();
      for (int i = 0; i < nodeList.getLength(); i++) {
        Node childNode = nodeList.item(i);
        String nodeName = removeNS(childNode.getNodeName());
        if (nodeName.equalsIgnoreCase(INDEX_STR)) {
          return childNode.getTextContent();
        }
      }
    }
    return null;
  }
}


class SortByParamterName implements Comparator<ParameterDTO>, Serializable {
  private static final long serialVersionUID = 3010693349267067945L;

  public int compare(ParameterDTO a, ParameterDTO b) {
    return a.getParamName().compareTo(b.getParamName());
  }
}
