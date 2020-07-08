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

package org.commscope.tr069adapter.netconf.server;

import org.commscope.tr069adapter.netconf.boot.NetConfServiceBooter;
import org.junit.FixMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runners.MethodSorters;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;


@FixMethodOrder(MethodSorters.NAME_ASCENDING)
@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = {NetConfServiceBooter.class},
    args = "--schemas-dir test-schemas --debug true --starting-port 17830")
@AutoConfigureMockMvc
class RestartNetconfServerHandlerTest {

  @MockBean
  NetConfServerManagerImpl manager;

  @Autowired
  RestartNetconfServerHandler restartHandler;
  /*
   * @Test void testRestart() { NetConfServerDetailsEntity entity = new
   * NetConfServerDetailsEntity(); entity.setDeviceId("0005B9AB1");
   * entity.setEnodeBName("0005B9AB1"); entity.setId(1l); entity.setListenPort("17830"); try {
   * restartHandler.restart(entity); } catch (RetryFailedException e) {
   * fail("Exception while retry."); } assertTrue(true); }
   * 
   * @Test void testRecover() { NetConfServerDetailsEntity entity = new
   * NetConfServerDetailsEntity(); entity.setDeviceId("0005B9AB1");
   * entity.setEnodeBName("0005B9AB1"); entity.setId(1l); entity.setListenPort("17830");
   * restartHandler.recover(entity); assertTrue(true); }
   */
}
