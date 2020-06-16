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

package org.commscope.tr069adapter.netconf.operations;

import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.opendaylight.netconf.api.capability.Capability;
import org.opendaylight.netconf.impl.SessionIdProvider;
import org.opendaylight.netconf.mapping.api.NetconfOperation;

class CustomOperationsCreatorTest {

  @Test
  void testCustomOperationsCreator() {
    CustomOperationsCreator coc = getCustomOperationsCreator();
    Set<Capability> capabilities = new HashSet<Capability>();
    Set<NetconfOperation> opers =
        coc.getNetconfOperationService(capabilities, new SessionIdProvider(), "1")
            .getNetconfOperations();
    assertTrue(opers != null);
    assertTrue(!opers.isEmpty());
  }

  CustomOperationsCreator getCustomOperationsCreator() {
    CustomOperationsCreator coc = new CustomOperationsCreator("0005B9AB1");
    return coc;
  }

}
