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

import java.io.Serializable;

import javax.servlet.http.Cookie;

import org.apache.activemq.broker.BrokerService;
import org.apache.commons.httpclient.HttpStatus;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.utils.AcsConstants;
import org.commscope.tr069adapter.acs.cpe.handler.DeviceValidator;
import org.commscope.tr069adapter.acs.cpe.rpc.Inform;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepository;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
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
public class PeriodicInformTest {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private DeviceValidator deviceValidator;

  @MockBean
  protected DeviceRPCRequestRepository deviceRPCRequestRepository;

  @MockBean
  private SessionManager sessionManager;

  @MockBean
  private TR069EventNotificationService tr069EventNotificationService;

  @MockBean
  private TimerServiceManagerAPI timerServiceManagerAPI;

  @Autowired
  BrokerService broker;

  @Test
  public void processInformPnPTest() throws Exception {

    String exampleInform =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:cwmp=\"urn:dslforum-org:cwmp-1-0\"><SOAP-ENV:Header><cwmp:ID SOAP-ENV:mustUnderstand=\"1\">1</cwmp:ID></SOAP-ENV:Header><SOAP-ENV:Body SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><cwmp:Inform><DeviceId><Manufacturer>Airvana</Manufacturer><OUI>0005B9</OUI><ProductClass>LTE_Enterprise_C-RANSC_Cntrl</ProductClass><SerialNumber>0005B9519093</SerialNumber></DeviceId><Event xsi:type=\"SOAP-ENC:Array\" SOAP-ENC:arrayType=\"cwmp:EventStruct[1]\"><EventStruct><EventCode>2 PERIODIC</EventCode><CommandKey></CommandKey></EventStruct></Event><MaxEnvelopes>1</MaxEnvelopes><CurrentTime>2018-04-27T07:09:16</CurrentTime><RetryCount>0</RetryCount><ParameterList xsi:type=\"SOAP-ENC:Array\" SOAP-ENC:arrayType=\"cwmp:ParameterValueStruct[12]\"><ParameterValueStruct><Name>Device.ManagementServer.ParameterKey</Name><Value xsi:type=\"xsd:string\">None</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.ManagementServer.ConnectionRequestURL</Name><Value xsi:type=\"xsd:string\">http://10.210.37.1/acscall</Value></ParameterValueStruct>"
            + "<ParameterValueStruct><Name>Device.DeviceInfo.Manufacturer</Name><Value xsi:type=\"xsd:string\">Airvana</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.ManufacturerOUI</Name><Value xsi:type=\"xsd:string\">0005B9</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.ProductClass</Name><Value xsi:type=\"xsd:string\">LTE_Enterprise_C-RANSC_Cntrl</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.SerialNumber</Name><Value xsi:type=\"xsd:string\">0005B9519093</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.HardwareVersion</Name><Value xsi:type=\"xsd:string\">750742.00.04</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.SoftwareVersion</Name><Value xsi:type=\"xsd:string\">3.0.00.073</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.ProvisioningCode</Name><Value xsi:type=\"xsd:string\"></Value></ParameterValueStruct><ParameterValueStruct><Name>Device.FAP.X_0005B9_RUWhiteList</Name><Value xsi:type=\"xsd:string\"></Value></ParameterValueStruct><ParameterValueStruct><Name>Device.IP.Interface.1.IPv4Enable</Name><Value xsi:type=\"xsd:boolean\">1</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.IP.Interface.1.IPv4Address.1.IPAddress</Name><Value xsi:type=\"xsd:string\">10.210.37.1</Value></ParameterValueStruct>"
            + "</ParameterList></cwmp:Inform></SOAP-ENV:Body></SOAP-ENV:Envelope>";

    try {

      Mockito.doNothing().when(tr069EventNotificationService)
          .sendDeviceInformToNBI(Mockito.any(DeviceInform.class));

      Mockito
          .when(deviceValidator.isDeviceAuthorized(Mockito.any(Inform.class), Mockito.anyString()))
          .thenReturn(new Boolean(true));

      Mockito.when(deviceValidator.validateDevice(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString())).thenReturn(new Boolean(true));

      Mockito.doNothing().when(timerServiceManagerAPI).startTimer(Mockito.anyString(),
          Mockito.anyString(), Mockito.anyLong(), Mockito.any(Serializable.class));
      Mockito.doNothing().when(timerServiceManagerAPI).stopTimer(Mockito.anyString());

      SessionDTO session = new SessionDTO();
      session.setDeviceId("0005B9519093");
      session.setSessionId("sessionId3");
      session.setCurrentOperationId(10000L);

      Mockito.when(sessionManager.getSessionBySessionId(Mockito.anyString())).thenReturn(session);
      Mockito.when(sessionManager.getLockedSession(Mockito.anyString())).thenReturn(session);
      Mockito.when(sessionManager.updateSession(Mockito.any(SessionDTO.class))).thenReturn(session);
      Mockito.when(sessionManager.generateUniqueSessionID()).thenReturn("sessionid10");

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
