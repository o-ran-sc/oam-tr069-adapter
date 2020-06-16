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

package org.commscope.tr069adapter.acs.cpe.test.opresult;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.Cookie;

import org.apache.activemq.broker.BrokerService;
import org.apache.commons.httpclient.HttpStatus;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.utils.AcsConstants;
import org.commscope.tr069adapter.acs.requestprocessor.DeviceOperationInterface;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepositoryHelper;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceRPCRequestEntity;
import org.commscope.tr069adapter.acs.requestprocessor.impl.SessionManager;
import org.commscope.tr069adapter.acs.requestprocessor.impl.TR069EventNotificationService;
import org.commscope.tr069adapter.common.timer.TimerServiceManagerAPI;
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
public class GPVOpResultTest {

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

  @MockBean
  private TR069EventNotificationService tr069EventNotificationService;

  @MockBean
  private TimerServiceManagerAPI timerServiceManagerAPI;

  @Test
  public void processOpResultWithNoPendingRequestTest() throws Exception {
    String exampleOpResult =
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><ns3:Envelope xmlns:ns1=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns4=\"urn:dslforum-org:cwmp-1-0\" xmlns:ns3=\"http://schemas.xmlsoap.org/soap/envelope/\"><ns3:Header><ns4:ID ns3:mustUnderstand=\"1\">12655897</ns4:ID></ns3:Header><ns3:Body><ns4:GetParameterValuesResponse><ParameterList ns1:arrayType=\"ns4:ParameterValueStruct[1]\"><ParameterValueStruct><Name>Device.Hosts.HostNumberOfEntries</Name><Value>1</Value></ParameterValueStruct></ParameterList></ns4:GetParameterValuesResponse></ns3:Body></ns3:Envelope>";

    try {
      SessionDTO session = new SessionDTO();
      session.setDeviceId("0005B9519095");
      session.setSessionId("sessionId");
      session.setCurrentOperationId(10000L);

      TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
      tr069DeviceDetails.setDeviceId("0005B9519095");

      Mockito.when(sessionManager.getSessionBySessionId(Mockito.anyString())).thenReturn(session);
      Mockito.when(sessionManager.getLockedSession(Mockito.anyString())).thenReturn(session);
      Mockito.when(sessionManager.updateSession(Mockito.any(SessionDTO.class))).thenReturn(session);

      Mockito.when(deviceOperationInterface.getDeviceDetails(Mockito.anyString()))
          .thenReturn(tr069DeviceDetails);

      Mockito.when(deviceRPCRequestRepositoryHelper.findOldestDeviceRPCRequest(Mockito.anyString()))
          .thenReturn(null);

      List<TR069DeviceRPCRequestEntity> entityList = new ArrayList<TR069DeviceRPCRequestEntity>();
      TR069DeviceRPCRequestEntity entity = new TR069DeviceRPCRequestEntity();
      entity.setOpCode(TR069OperationCode.GET_PARAMETER_VALUES.getOperationCode());
      entity.setOperationId(10002L);
      entity.setRequestTimeOut(300000L);
      entity.setDeviceId("0005B9519095");

      entityList.add(entity);

      Mockito.when(deviceRPCRequestRepositoryHelper
          .findByDeviceIdAndOperationId(Mockito.anyString(), Mockito.anyLong()))
          .thenReturn(entityList);

      Mockito.doNothing().when(tr069EventNotificationService)
          .sendOperationResultToNBI(Mockito.any(DeviceRPCResponse.class));
      Mockito.doNothing().when(deviceRPCRequestRepositoryHelper)
          .markDeviceRPCRequestAsProcessed(Mockito.anyString(), Mockito.anyLong());
      Mockito.doNothing().when(timerServiceManagerAPI).startTimer(Mockito.anyString(),
          Mockito.anyString(), Mockito.anyLong(), Mockito.any(Serializable.class));
      Mockito.doNothing().when(timerServiceManagerAPI).stopTimer(Mockito.anyString());

      Cookie cookie = new Cookie(AcsConstants.ACS_SESSIONID, "sessionId");
      RequestBuilder requestBuilder =
          MockMvcRequestBuilders.post("/CPEMgmt/acs").accept(MediaType.TEXT_PLAIN)
              .content(exampleOpResult).cookie(cookie).contentType(MediaType.TEXT_PLAIN);

      MvcResult result = mockMvc.perform(requestBuilder).andReturn();

      MockHttpServletResponse response = result.getResponse();
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
