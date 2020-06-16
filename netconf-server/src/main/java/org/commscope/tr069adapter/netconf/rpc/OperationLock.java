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

import org.opendaylight.netconf.api.xml.XmlElement;
import org.opendaylight.netconf.api.xml.XmlNetconfConstants;
import org.opendaylight.netconf.util.mapping.AbstractLastNetconfOperation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class OperationLock extends AbstractLastNetconfOperation {

  public OperationLock(final String netconfSessionIdForReporting) {
    super(netconfSessionIdForReporting);
  }

  @Override
  protected Element handleWithNoSubsequentOperations(final Document document,
      final XmlElement operationElement) {
    return document.createElement(XmlNetconfConstants.OK);
  }

  @Override
  protected String getOperationName() {
    return "lock";
  }
}
