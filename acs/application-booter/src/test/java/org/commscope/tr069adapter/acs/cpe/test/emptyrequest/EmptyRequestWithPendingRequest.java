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

package org.commscope.tr069adapter.acs.cpe.test.emptyrequest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import javax.servlet.http.Cookie;

import org.apache.activemq.broker.BrokerService;
import org.apache.commons.httpclient.HttpStatus;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.utils.AcsConstants;
import org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepositoryHelper;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.impl.SessionManager;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.RequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

@SpringBootTest(classes = {ACSServiceBooter.class})
@RunWith(SpringJUnit4ClassRunner.class)
@AutoConfigureMockMvc
@ContextConfiguration
public abstract class EmptyRequestWithPendingRequest {

  @Autowired
  private MockMvc mockMvc;

  @Autowired
  BrokerService broker;

  @MockBean
  private SessionManager sessionManager;

  @MockBean
  private DeviceOperationInterface deviceOperationInterface;

  @MockBean
  protected DeviceRPCRequestRepositoryHelper deviceRPCRequestRepositoryHelper;

  @Test
  public void processEmptyRequestWithPendingRequestTest() throws Exception {
    String emptyRequest = "";
    try {
      SessionDTO session = new SessionDTO();
      session.setDeviceId("0005B9519090");
      session.setSessionId("sessionId");

      TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
      tr069DeviceDetails.setDeviceId("0005B9519090");

      DeviceRPCRequest deviceRPCRequest = getDeviceRPCRequest(tr069DeviceDetails);

      Mockito.when(sessionManager.getSessionBySessionId(Mockito.anyString())).thenReturn(session);

      Mockito.when(sessionManager.getLockedSession(Mockito.anyString())).thenReturn(session);

      Mockito.when(deviceOperationInterface.getDeviceDetails(Mockito.anyString()))
          .thenReturn(tr069DeviceDetails);

      Mockito.when(deviceRPCRequestRepositoryHelper.findOldestDeviceRPCRequest(Mockito.anyString()))
          .thenReturn(deviceRPCRequest);

      Cookie cookie = new Cookie(AcsConstants.ACS_SESSIONID, "sessionId");

      RequestBuilder requestBuilder = MockMvcRequestBuilders.post("/CPEMgmt/acs")
          .accept(MediaType.TEXT_PLAIN).content(emptyRequest).header("Authorization", "basic")
          .cookie(cookie).contentType(MediaType.TEXT_PLAIN);

      MvcResult result = mockMvc.perform(requestBuilder).andReturn();

      MockHttpServletResponse response = result.getResponse();
      assertEquals(HttpStatus.SC_OK, response.getStatus());

    } catch (Exception e) {
      fail(e.getMessage());
    }
  }

  public abstract DeviceRPCRequest getDeviceRPCRequest(TR069DeviceDetails tr069DeviceDetails);


  @After
  public void stopBroker() throws Exception {
    try {
      System.out.println("Tearing down the broker");
      broker.stop();
      broker.waitUntilStopped();
      broker = null;
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
