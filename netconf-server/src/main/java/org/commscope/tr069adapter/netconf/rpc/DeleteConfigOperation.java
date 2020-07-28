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
import org.opendaylight.netconf.test.tool.rpc.DataList;
import org.opendaylight.netconf.util.mapping.AbstractLastNetconfOperation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class DeleteConfigOperation extends AbstractLastNetconfOperation {

  private static final Logger logger = LoggerFactory.getLogger(DeleteConfigOperation.class);

  private static final String DELETE_EDIT_CONFIG = "delete";
  private static final String OPERATION = "operation";
  private static final String REMOVE_EDIT_CONFIG = "remove";
  private final DataList storage;
  private String deviceID;
  private String swVersion;
  private String hwVersion;

  public DeleteConfigOperation(final String netconfSessionIdForReporting, final DataList storage,
      String deviceID, String swVersion, String hwVersion) {
    super(netconfSessionIdForReporting);
    this.storage = storage;
    this.deviceID = deviceID;
    this.swVersion = swVersion;
    this.hwVersion = hwVersion;
  }

  @Override
  protected Element handleWithNoSubsequentOperations(final Document document,
      final XmlElement operationElement) throws DocumentedException {
    final XmlElement configElementData =
        operationElement.getOnlyChildElement(XmlNetconfConstants.CONFIG_KEY);
    containsDelete(configElementData);
    if (containsDelete(configElementData)) {
      storage.resetConfigList();
    } else {
      storage.setConfigList(configElementData.getChildElements());
    }

    String requestXml = XmlUtility.convertDocumentToString(operationElement);
    logger.debug("netconf request recevied : {}", requestXml);
    NetConfServerProperties config =
        NetConfServiceBooter.getApplicationContext().getBean(NetConfServerProperties.class);

    final String baseUrl = config.getMapperPath() + "/delConfig";
    NetConfResponse restResponse =
        XmlUtility.invokeMapperCall(baseUrl, requestXml, deviceID, swVersion, hwVersion);

    if (restResponse != null) {
      ErrorCodeDetails errorCode = restResponse.getErrorCode();

      if (errorCode != null && errorCode.getFaultCode() != null
          && !errorCode.getFaultCode().equalsIgnoreCase("0")) {
        throw new DocumentedException(errorCode.getErrorMessage(),
            ErrorType.from(errorCode.getErrorType()), ErrorTag.from(errorCode.getErrorTag()),
            ErrorSeverity.from(errorCode.getErrorSeverity()));
      } else {
        return document.createElement(XmlNetconfConstants.OK);
      }
    } else {
      logger.error("received the null response from mapper ");
      throw new DocumentedException("Unable to perform Operation", ErrorType.from("application"),
          ErrorTag.from("operation-failed"), ErrorSeverity.from("ERROR"));
    }

  }

  @Override
  protected String getOperationName() {
    return "delete-config";
  }

  private boolean containsDelete(final XmlElement element) {
    for (final Attr o : element.getAttributes().values()) {
      if (o.getLocalName().equals(OPERATION)
          && (o.getValue().equals(DELETE_EDIT_CONFIG) || o.getValue().equals(REMOVE_EDIT_CONFIG))) {
        return true;
      }
    }
    for (final XmlElement xmlElement : element.getChildElements()) {
      if (containsDelete(xmlElement)) {
        return true;
      }
    }
    return false;
  }
}
