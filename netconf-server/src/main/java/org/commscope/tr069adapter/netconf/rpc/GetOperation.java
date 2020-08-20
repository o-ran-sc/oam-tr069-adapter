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

import java.io.IOException;

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
import org.opendaylight.netconf.api.xml.XmlUtil;
import org.opendaylight.netconf.test.tool.rpc.DataList;
import org.opendaylight.netconf.util.mapping.AbstractLastNetconfOperation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

public class GetOperation extends AbstractLastNetconfOperation {
  private static final Logger logger = LoggerFactory.getLogger(GetOperation.class);

  private final DataList storage;
  private String deviceID;
  private String swVersion;
  private String hwVersion;

  public GetOperation(final String netconfSessionIdForReporting, final DataList storage,
      String deviceID, String swVersion, String hwVersion) {
    super(netconfSessionIdForReporting);
    this.deviceID = deviceID;
    this.storage = storage;
    this.swVersion = swVersion;
    this.hwVersion = hwVersion;
  }

  @Override
  protected Element handleWithNoSubsequentOperations(final Document document,
      final XmlElement operationElement) throws DocumentedException {
    final Element element = document.createElement(XmlNetconfConstants.DATA_KEY);

    for (final XmlElement e : storage.getConfigList()) {
      final Element domElement = e.getDomElement();
      element.appendChild(element.getOwnerDocument().importNode(domElement, true));
    }

    String requestXml = XmlUtility.convertDocumentToString(operationElement);
    logger.debug("netconf request recevied : {}", requestXml);
    NetConfServerProperties config =
        NetConfServiceBooter.getApplicationContext().getBean(NetConfServerProperties.class);

    final String baseUrl = config.getMapperPath() + "/get";
    NetConfResponse restResponse =
        XmlUtility.invokeMapperCall(baseUrl, requestXml, deviceID, swVersion, hwVersion);

    if (restResponse != null) {
      ErrorCodeDetails errorCode = restResponse.getErrorCode();
      if (errorCode != null && errorCode.getFaultCode() != null
          && !errorCode.getFaultCode().equalsIgnoreCase("0")) {
        throw new DocumentedException(errorCode.getErrorMessage(),
            ErrorType.from(errorCode.getErrorType()), ErrorTag.from(errorCode.getErrorTag()),
            ErrorSeverity.from(errorCode.getErrorSeverity()));
      } else if (restResponse.getNetconfResponseXml() != null) {
        Element element1 = null;
        try {
          element1 = XmlUtil.readXmlToElement(restResponse.getNetconfResponseXml());
          XmlElement xmlElement = XmlElement.fromDomElement(element1);
          Element domElement = xmlElement.getDomElement();
          element.appendChild(element.getOwnerDocument().importNode(domElement, true));
        } catch (SAXException | IOException e1) {
          logger.error("Error while constructing the reponse {}", e1.toString());
        }
      }
    } else {
      logger.error("received the null response from mapper ");
      throw new DocumentedException("Unable to perform Operation", ErrorType.from("application"),
          ErrorTag.from("operation-failed"), ErrorSeverity.from("ERROR"));
    }
    return element;
  }

  @Override
  protected String getOperationName() {
    return XmlNetconfConstants.GET;
  }
}
