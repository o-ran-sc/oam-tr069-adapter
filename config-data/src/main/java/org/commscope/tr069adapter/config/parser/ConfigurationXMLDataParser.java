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

package org.commscope.tr069adapter.config.parser;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.XMLConstants;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.commscope.tr069adapter.config.constants.ConfigurationServiceConstant;
import org.commscope.tr069adapter.config.dto.ConfigurationData;
import org.commscope.tr069adapter.config.exceptions.InvalidConfigurationServiceException;
import org.commscope.tr069adapter.config.model.ConfigFileContent;
import org.springframework.stereotype.Component;
import org.xml.sax.Attributes;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

@Component
public class ConfigurationXMLDataParser extends DefaultHandler {

  private static final Log logger = LogFactory.getLog(ConfigurationXMLDataParser.class);

  private List<ConfigurationData> completeConfigurationDataList;

  private ConfigurationData configurationData;
  private Map<String, String> parameterMONameValueMap;

  private String moAttr;
  private String moValue;

  public void validateFile(ConfigFileContent configFileContent)
      throws InvalidConfigurationServiceException {
    SAXParserFactory factory = SAXParserFactory.newInstance();
    factory.setValidating(true);
    factory.setNamespaceAware(true);

    String xmlFileContent = configFileContent.getFileContent();
    byte[] byteArray;

    try {
      byteArray = xmlFileContent.getBytes(StandardCharsets.UTF_8);
    } catch (Exception e) {
      logger.error("Error while parsing device configuration XML file. {}", e);
      throw new InvalidConfigurationServiceException(
          "UnsupportedEncodingException error. " + e.getMessage());
    }

    ByteArrayInputStream xmlFileContentInputStream = new ByteArrayInputStream(byteArray);

    logger.debug("Validating XML file");

    validateXmlWithSchema(xmlFileContentInputStream);

    logger.debug("XML file validation is successful");

  }

  public ConfigurationData parseFile(ConfigFileContent configFileContent)
      throws InvalidConfigurationServiceException {
    SAXParserFactory factory = SAXParserFactory.newInstance();
    factory.setValidating(true);
    factory.setNamespaceAware(true);

    try {
      String xmlFileContent = configFileContent.getFileContent();
      byte[] byteArray = xmlFileContent.getBytes(StandardCharsets.UTF_8);
      ByteArrayInputStream xmlFileContentInputStream = new ByteArrayInputStream(byteArray);

      logger.debug("Validating XML file");

      validateXmlWithSchema(xmlFileContentInputStream);

      logger.debug("XML file validation is successful");

      byteArray = xmlFileContent.getBytes(StandardCharsets.UTF_8);
      xmlFileContentInputStream = new ByteArrayInputStream(byteArray);

      SAXParser parser = null;
      parser = factory.newSAXParser();
      parser.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      parser.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
      setProperty(parser);

      parser.parse(xmlFileContentInputStream, this);

    } catch (SAXException e) {
      logger.error("Error while parsing device configuration XML file. {}", e);
      throw new InvalidConfigurationServiceException("File parsing error. " + e.getMessage());
    } catch (Exception e) {
      logger.error("Unknown error occurred while parsing device configuration XML file. {}", e);
      throw new InvalidConfigurationServiceException("UNKNOWN ERROR. " + e.getMessage());
    }

    if (null == completeConfigurationDataList || completeConfigurationDataList.isEmpty()) {
      return null;
    } else {
      return completeConfigurationDataList.get(0);
    }
  }

  @Override
  public void startElement(String uri, String localName, String qName, Attributes attributes)
      throws SAXException {
    qName = qName.trim();
    if (qName.equalsIgnoreCase(ConfigurationServiceConstant.CONFIG_DATA_FILE)) {
      completeConfigurationDataList = new ArrayList<>();
      return;
    }

    if (qName.equalsIgnoreCase(ConfigurationServiceConstant.CONFIG_DATA)) {
      moAttr = null;
      configurationData = new ConfigurationData();
      parameterMONameValueMap = new HashMap<>();

      configurationData.setParameterMONameValueMap(parameterMONameValueMap);
    } else if (qName.equalsIgnoreCase(ConfigurationServiceConstant.FILE_HEADER)) {
      logger.debug("File hearder start element parsing started");
    } else {
      createAttribute(qName, attributes);
    }
  }

  @Override
  public void endElement(String uri, String localName, String qName) throws SAXException {
    qName = qName.trim();

    if (qName.equalsIgnoreCase(ConfigurationServiceConstant.CONFIG_DATA_FILE)) {
      logger.debug("ignoreing the datafile end element");
    } else if (qName.equalsIgnoreCase(ConfigurationServiceConstant.CONFIG_DATA)) {
      completeConfigurationDataList.add(configurationData);
    } else if (qName.equalsIgnoreCase(ConfigurationServiceConstant.MANAGED_ELEMENT)
        || qName.equalsIgnoreCase(ConfigurationServiceConstant.FILE_HEADER)) {
      logger.debug("File hearder end element parsing started");
    } else {
      if (moValue != null && !moValue.equals("") && !moValue.trim().isEmpty()) {
        parameterMONameValueMap.put(moAttr, moValue);
        moValue = null;
      }
      try {
        if (moAttr.lastIndexOf(qName) > 0) {
          moAttr = moAttr.substring(0, moAttr.lastIndexOf(qName) - 1);
        }
      } catch (StringIndexOutOfBoundsException e) {
        logger.error("Error occurred while parshing XML file. Cause: {}", e);
      }
    }
  }

  private void createAttribute(String attrName, Attributes attributes) {

    if (attrName.equalsIgnoreCase(ConfigurationServiceConstant.MANAGED_ELEMENT)) {
      configurationData.setOUI(attributes.getValue("OUI"));
      configurationData.setProductClass(attributes.getValue("ProductClass"));
      configurationData.setHardwareVersion(attributes.getValue("hwVersion"));
      configurationData.setSoftwareVersion(attributes.getValue("swVersion"));
      configurationData.setLocalDn(attributes.getValue("localDn"));

      return;
    }
    if (moAttr != null) {
      moAttr = moAttr + "." + attrName;
    } else {
      moAttr = attrName;
    }

    if (attributes.getValue(ConfigurationServiceConstant.TABULAR_INDEX_NAME) != null) {
      moAttr = moAttr + "." + attributes.getValue(ConfigurationServiceConstant.TABULAR_INDEX_NAME);
    }

  }

  @Override
  public void characters(char[] ch, int start, int length) throws SAXException {
    String tmp = new String(ch, start, length);
    if (moValue == null) {
      moValue = tmp.trim();
    } else {
      moValue = moValue + tmp.trim();
    }
  }

  public void validateXmlWithSchema(ByteArrayInputStream xmlFileContentInputStream)
      throws InvalidConfigurationServiceException {
    logger.info("Validating the XML file against XSD file: "
        + ConfigurationServiceConstant.CONFIGURATION_DATA_XSD_PATH);
    try {
      SchemaFactory factory = SchemaFactory.newInstance(ConfigurationServiceConstant.XML_SCHEMA_NS);
      factory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
      factory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");

      InputStream xsdInputStream =
          getClass().getResourceAsStream(ConfigurationServiceConstant.CONFIGURATION_DATA_XSD_PATH);
      if (xsdInputStream == null) {
        throw new InvalidConfigurationServiceException("File parsing error: Unable to find XSD "
            + ConfigurationServiceConstant.CONFIGURATION_DATA_XSD_PATH);
      }

      Schema schema = factory.newSchema(new StreamSource(xsdInputStream));
      Validator validator = schema.newValidator();

      final StringBuilder exceptions = new StringBuilder();
      validator.setErrorHandler(new ErrorHandler() {
        @Override
        public void warning(SAXParseException exception) {
          handleMessage(exception);
        }

        @Override
        public void error(SAXParseException exception) {
          handleMessage(exception);
        }

        @Override
        public void fatalError(SAXParseException exception) {
          handleMessage(exception);
        }

        private void handleMessage(SAXParseException exception) {
          int lineNumber = exception.getLineNumber();
          int columnNumber = exception.getColumnNumber();
          String message = exception.getMessage();
          exceptions.append("\n" + lineNumber + ":" + columnNumber + ": " + message);
        }
      });

      validator.validate(new StreamSource(xmlFileContentInputStream));
      if (exceptions.length() > 0) {
        throw new SAXException(" Line=" + exceptions);
      }
    } catch (Exception e) {
      logger.error("Error while parsing the XML file " + e.toString());
      throw new InvalidConfigurationServiceException("File parsing error. " + e.toString());
    }

    logger.debug("File is valid.");
  }

  protected void setProperty(SAXParser parser)
      throws SAXNotRecognizedException, SAXNotSupportedException {
	  logger.debug("property added.");
  }

}
