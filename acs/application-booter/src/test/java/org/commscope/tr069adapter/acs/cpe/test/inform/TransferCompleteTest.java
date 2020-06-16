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

package org.commscope.tr069adapter.acs.cpe.test.inform;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import javax.servlet.http.Cookie;

import org.apache.activemq.broker.BrokerService;
import org.apache.commons.httpclient.HttpStatus;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.utils.AcsConstants;
import org.commscope.tr069adapter.acs.cpe.handler.DeviceValidator;
import org.commscope.tr069adapter.acs.cpe.rpc.Inform;
import org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepository;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.impl.SessionManager;
import org.commscope.tr069adapter.acs.requestprocessor.impl.TR069EventNotificationService;
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
public class TransferCompleteTest {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private DeviceValidator deviceValidator;

  @MockBean
  protected DeviceRPCRequestRepository deviceRPCRequestRepository;

  @MockBean
  private TR069EventNotificationService tr069EventNotificationService;

  @MockBean
  private DeviceOperationInterface deviceOperationInterface;

  @MockBean
  private SessionManager sessionManager;

  @Autowired
  BrokerService broker;

  @Test
  public void processInformPnPTest() throws Exception {

    String exampleInform =
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><ns3:Envelope xmlns:ns1=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns4=\"urn:dslforum-org:cwmp-1-0\" xmlns:ns3=\"http://schemas.xmlsoap.org/soap/envelope/\"><ns3:Header><ns4:ID ns3:mustUnderstand=\"1\">0</ns4:ID></ns3:Header><ns3:Body><ns4:TransferComplete><CommandKey>0005B9AAAA22</CommandKey><FaultStruct><FaultCode>0</FaultCode><FaultString></FaultString></FaultStruct><StartTime>2019-07-08T18:15:43.032Z</StartTime><CompleteTime>2019-07-08T18:15:43.032Z</CompleteTime></ns4:TransferComplete></ns3:Body></ns3:Envelope>";

    try {

      SessionDTO session = new SessionDTO();
      session.setDeviceId("0005B9AAAA22");
      session.setSessionId("sessionId");
      session.setCurrentOperationId(10000L);

      TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
      tr069DeviceDetails.setDeviceId("0005B9AAAA22");
      tr069DeviceDetails.setConnectionRequestURL("dummyURL");
      tr069DeviceDetails.setSoftwareVersion("10.2.1");
      tr069DeviceDetails.setSoftwareVersion("xxx");

      Mockito.when(sessionManager.getSessionBySessionId(Mockito.anyString())).thenReturn(session);
      Mockito.when(sessionManager.getLockedSession(Mockito.anyString())).thenReturn(session);
      Mockito.when(sessionManager.updateSession(Mockito.any(SessionDTO.class))).thenReturn(session);
      Mockito.when(sessionManager.generateUniqueSessionID()).thenReturn("newSessionId");

      Mockito.when(deviceOperationInterface.getDeviceDetails(Mockito.anyString()))
          .thenReturn(tr069DeviceDetails);

      Mockito.doNothing().when(tr069EventNotificationService)
          .sendDeviceInformToNBI(Mockito.any(DeviceInform.class));

      Mockito
          .when(deviceValidator.isDeviceAuthorized(Mockito.any(Inform.class), Mockito.anyString()))
          .thenReturn(new Boolean(true));

      Mockito.when(deviceValidator.validateDevice(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString())).thenReturn(new Boolean(true));

      Mockito.when(deviceRPCRequestRepository.findByDeviceIdAndIsProcessed(Mockito.anyString(),
          Mockito.anyInt())).thenReturn(null);

      RequestBuilder requestBuilder = MockMvcRequestBuilders.post("/CPEMgmt/acs")
          .accept(MediaType.TEXT_PLAIN).content(exampleInform).header("Authorization", "basic")
          .contentType(MediaType.TEXT_PLAIN);

      MvcResult result = mockMvc.perform(requestBuilder).andReturn();

      MockHttpServletResponse response = result.getResponse();
      assertEquals(HttpStatus.SC_OK, response.getStatus());

      Cookie cookie = response.getCookie(AcsConstants.ACS_SESSIONID);
      requestBuilder =
          MockMvcRequestBuilders.post("/CPEMgmt/acs").accept(MediaType.TEXT_PLAIN).content("")
              .header("Authorization", "basic").cookie(cookie).contentType(MediaType.TEXT_PLAIN);

      result = mockMvc.perform(requestBuilder).andReturn();

      response = result.getResponse();
      assertEquals(HttpStatus.SC_NO_CONTENT, response.getStatus());
    } catch (Exception e) {
      fail(e.getMessage());
    }
  }

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
