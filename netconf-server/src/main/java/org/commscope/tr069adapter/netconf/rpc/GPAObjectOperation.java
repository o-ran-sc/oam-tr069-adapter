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

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.commscope.tr069adapter.mapper.model.ErrorCodeDetails;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.netconf.boot.NetConfServiceBooter;
import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.opendaylight.netconf.api.DocumentedException;
import org.opendaylight.netconf.api.DocumentedException.ErrorSeverity;
import org.opendaylight.netconf.api.DocumentedException.ErrorTag;
import org.opendaylight.netconf.api.DocumentedException.ErrorType;
import org.opendaylight.netconf.api.xml.XmlElement;
import org.opendaylight.netconf.api.xml.XmlNetconfConstants;
import org.opendaylight.netconf.mapping.api.NetconfOperationChainedExecution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

public class GPAObjectOperation extends GenericOperation {
  private static final Logger logger = LoggerFactory.getLogger(GPAObjectOperation.class);

  public GPAObjectOperation(String deviceID, String swVersion, String hwVersion) {
    this.deviceID = deviceID;
    this.swVersion = swVersion;
    this.hwVersion = hwVersion;
    setOpString("gpaobject");
    setOpNamespace("urn:tr069rpc:1.0");
    setOpName("get-parameter-attributes");
  }

  @Override
  public Document handle(Document requestMessage,
      NetconfOperationChainedExecution subsequentOperation) throws DocumentedException {
    logger.debug("gpaObject rpc is received in netconfserver");

    final XmlElement requestElement = XmlElement.fromDomDocument(requestMessage);
    final String msgId = requestElement.getAttribute(XmlNetconfConstants.MESSAGE_ID);

    String requestXml = XmlUtility.convertDocumentToString(requestElement);
    logger.debug("gpaObject rpc requestXml= {}", requestXml);

    NetConfServerProperties config =
        NetConfServiceBooter.getApplicationContext().getBean(NetConfServerProperties.class);

    final String baseUrl = config.getMapperPath() + "/" + getOpString();
    NetConfResponse restResponse =
        XmlUtility.invokeMapperCall(baseUrl, requestXml, deviceID, swVersion, hwVersion);
    Document document = null;

    ErrorCodeDetails errorCode = restResponse.getErrorCode();
    if (errorCode != null && errorCode.getFaultCode() != null
        && !errorCode.getFaultCode().equalsIgnoreCase("0")) {
      logger.error("Error recevied : {}", errorCode);
      throw new DocumentedException(errorCode.getErrorMessage(),
          ErrorType.from(errorCode.getErrorType()), ErrorTag.from(errorCode.getErrorTag()),
          ErrorSeverity.from(errorCode.getErrorSeverity()));
    } else if (restResponse.getNetconfResponseXml() != null) {
      logger.debug("gpaobject rpc response received from mapper {}",
          restResponse.getNetconfResponseXml());
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
      DocumentBuilder builder;
      try {
        builder = factory.newDocumentBuilder();
        document =
            builder.parse(new InputSource(new StringReader(restResponse.getNetconfResponseXml())));
        document.getDocumentElement().setAttribute("xmlns:ns1", getOpNamespace());
        document.getDocumentElement().setAttribute("xmlns",
            XmlNetconfConstants.URN_IETF_PARAMS_XML_NS_NETCONF_BASE_1_0);
        document.getDocumentElement().setAttribute(XmlNetconfConstants.MESSAGE_ID, msgId);
      } catch (Exception e) {
        logger.error("Error while contruscting the response: {}", e.getMessage());
        throw new DocumentedException("Operation Aborted", ErrorType.from("application"),
            ErrorTag.from("operation-failed"), ErrorSeverity.from("ERROR"));
      }
    }

    return document;
  }

}
