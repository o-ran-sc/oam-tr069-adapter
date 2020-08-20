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

package org.commscope.tr069adapter.netconf.error;

import static org.junit.jupiter.api.Assertions.*;

import org.commscope.tr069adapter.mapper.model.NetconfServerManagementError;
import org.junit.jupiter.api.Test;

class ServerPortAllocationExceptionTest {

  @Test
  void testServerPortAllocationException() {
    try {
      throw new ServerPortAllocationException(NetconfServerManagementError.PORT_IN_USE);

    } catch (ServerPortAllocationException e) {
      assertTrue(true);
    }
  }

  @Test
  void testGetError() {
    try {
      throw new ServerPortAllocationException(NetconfServerManagementError.PORT_IN_USE);

    } catch (ServerPortAllocationException e) {
      assertEquals(NetconfServerManagementError.PORT_IN_USE, e.getError());
    }
  }

  @Test
  void testSetError() {
    try {
      ServerPortAllocationException se =
          new ServerPortAllocationException(NetconfServerManagementError.INTERNAL_ERROR);
      throw se;

    } catch (ServerPortAllocationException e) {
      assertEquals(NetconfServerManagementError.INTERNAL_ERROR, e.getError());
    }
  }

}
