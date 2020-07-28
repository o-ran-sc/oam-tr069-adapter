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

package org.commscope.tr069adapter.netconf.restapi;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.commscope.tr069adapter.netconf.boot.NetConfServiceBooter;
import org.commscope.tr069adapter.netconf.dao.NetConfServerDetailsRepository;
import org.commscope.tr069adapter.netconf.entity.NetConfServerDetailsEntity;
import org.commscope.tr069adapter.netconf.server.NetConfServerManagerImpl;
import org.junit.FixMethodOrder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runners.MethodSorters;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = {NetConfServiceBooter.class},
    args = "--schemas-dir test-schemas --debug true --starting-port 17830")
@AutoConfigureMockMvc
public class NetConfServerManagerRestApiTest {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  NetConfServerDetailsRepository netconfDAO;

  @Autowired
  NetConfServerManagerImpl manager;

  @Test
  public void createNetconfServer() throws Exception {

    NetConfServerDetailsEntity entity = new NetConfServerDetailsEntity();
    entity.setDeviceId("0005B9AB1");
    entity.setEnodeBName("0005B9AB1");
    entity.setId(1l);
    entity.setListenPort("17830");

    MockHttpServletRequestBuilder requestBuilder =
        MockMvcRequestBuilders.post("/netConfServerManagerService/createServer")
            .param("deviceId", "0005B9AB1").param("enodeBName", "0005B9AB1")
            .param("swVersion", "4.4.3").param("hwVersion", "*").accept(MediaType.APPLICATION_JSON);
    MvcResult result = mockMvc.perform(requestBuilder).andReturn();

    MockHttpServletResponse response = result.getResponse();

    assertEquals(HttpStatus.OK.value(), response.getStatus());

  }

  @Test
  public void listNetconfServers() throws Exception {

    MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders
        .get("/netConfServerManagerService/listServers").accept(MediaType.APPLICATION_JSON);
    MvcResult result = mockMvc.perform(requestBuilder).andReturn();
    MockHttpServletResponse response = result.getResponse();

    assertEquals(HttpStatus.OK.value(), response.getStatus());

  }

  @Test
  public void restartServersOnStartup() {
    boolean result = false;
    try {
      manager.restartServers();
      result = true;
    } catch (Exception e) {
      assertEquals(false, result); // if no exception
    }
    assertEquals(true, result); // if no exception

  }
}
