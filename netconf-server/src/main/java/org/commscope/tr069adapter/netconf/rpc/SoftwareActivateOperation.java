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
import org.opendaylight.netconf.api.xml.XmlElement;
import org.opendaylight.netconf.mapping.api.NetconfOperationChainedExecution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;

public class SoftwareActivateOperation extends GenericOperation {
  private static final Logger logger = LoggerFactory.getLogger(SoftwareActivateOperation.class);

  public SoftwareActivateOperation(String deviceID, String swVersion, String hwVersion) {
    this.deviceID = deviceID;
    this.swVersion = swVersion;
    this.hwVersion = hwVersion;
    setOpString("softwareActivate");
    setOpName("software-activate");
    setOpNamespace("urn:o-ran:software-management:1.0");
  }

  @Override
  public Document handle(Document requestMessage,
      NetconfOperationChainedExecution subsequentOperation) {

    logger.debug("sw-activate rpc recevied in netconf server");
    final XmlElement requestElement = XmlElement.fromDomDocument(requestMessage);

    String requestXml = XmlUtility.convertDocumentToString(requestElement);
    logger.debug("sw-activate rpc recevied requestXml = {}", requestXml);
    NetConfServerProperties config =
        NetConfServiceBooter.getApplicationContext().getBean(NetConfServerProperties.class);

    final String baseUrl = config.getMapperPath() + "/" + getOpString();
    XmlUtility.invokeMapperCall(baseUrl, requestXml, deviceID, swVersion, hwVersion);
    return null;
  }
}
