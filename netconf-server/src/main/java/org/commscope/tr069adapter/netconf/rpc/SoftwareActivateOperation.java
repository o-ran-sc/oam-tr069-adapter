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

import org.commscope.tr069adapter.netconf.boot.NetConfServiceBooter;
import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.opendaylight.netconf.api.DocumentedException;
import org.opendaylight.netconf.api.xml.XmlElement;
import org.opendaylight.netconf.api.xml.XmlNetconfConstants;
import org.opendaylight.netconf.mapping.api.HandlingPriority;
import org.opendaylight.netconf.mapping.api.NetconfOperation;
import org.opendaylight.netconf.mapping.api.NetconfOperationChainedExecution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;

public class SoftwareActivateOperation implements NetconfOperation {
  private static final Logger logger = LoggerFactory.getLogger(SoftwareActivateOperation.class);
  public static final String SOFT_MGMT_NAMESPACE = "urn:o-ran:software-management:1.0";

  private String deviceID;
  private String swVersion;
  private String hwVersion;

  public SoftwareActivateOperation(String deviceID, String swVersion, String hwVersion) {
    this.deviceID = deviceID;
    this.swVersion = swVersion;
    this.hwVersion = hwVersion;
  }

  @Override
  public HandlingPriority canHandle(final Document message) throws DocumentedException {
    OperationNameAndNamespace operationNameAndNamespace = null;
    operationNameAndNamespace = new OperationNameAndNamespace(message);
    return canHandle(operationNameAndNamespace.getOperationName(),
        operationNameAndNamespace.getNamespace());
  }

  @Override
  public Document handle(Document requestMessage,
      NetconfOperationChainedExecution subsequentOperation) throws DocumentedException {

    logger.debug("sw-activate rpc recevied in netconf server");
    final XmlElement requestElement = XmlElement.fromDomDocument(requestMessage);

    String requestXml = XmlUtility.convertDocumentToString(requestElement);
    logger.debug("sw-activate rpc recevied requestXml = {}", requestXml);
    NetConfServerProperties config =
        NetConfServiceBooter.getApplicationContext().getBean(NetConfServerProperties.class);

    final String baseUrl = config.getMapperPath() + "/softwareActivate";
    XmlUtility.invokeMapperCall(baseUrl, requestXml, deviceID, swVersion, hwVersion);
    return null;
  }

  protected HandlingPriority canHandle(final String operationName,
      final String operationNamespace) {
    return operationName.equals("software-activate")
        && operationNamespace.equals(SOFT_MGMT_NAMESPACE)
            ? HandlingPriority.HANDLE_WITH_DEFAULT_PRIORITY.increasePriority(1100)
            : HandlingPriority.CANNOT_HANDLE;
  }

  public static final class OperationNameAndNamespace {
    private final String operationName;
    private final String namespace;

    private final XmlElement operationElement;

    public OperationNameAndNamespace(final Document message) throws DocumentedException {
      XmlElement requestElement = null;
      requestElement = getRequestElementWithCheck(message);
      operationElement = requestElement.getOnlyChildElement();
      operationName = operationElement.getName();
      namespace = operationElement.getNamespace();
    }

    public String getOperationName() {
      return operationName;
    }

    public String getNamespace() {
      return namespace;
    }

    public XmlElement getOperationElement() {
      return operationElement;
    }
  }

  protected static XmlElement getRequestElementWithCheck(final Document message)
      throws DocumentedException {
    return XmlElement.fromDomElementWithExpected(message.getDocumentElement(),
        XmlNetconfConstants.RPC_KEY, XmlNetconfConstants.URN_IETF_PARAMS_XML_NS_NETCONF_BASE_1_0);
  }
}
