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
import org.commscope.tr069adapter.acs.common.utils.AcsConstants;
import org.commscope.tr069adapter.acs.cpe.handler.DeviceValidator;
import org.commscope.tr069adapter.acs.cpe.rpc.Inform;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepository;
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
public class BootstrapInformTest {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private DeviceValidator deviceValidator;

  @MockBean
  protected DeviceRPCRequestRepository deviceRPCRequestRepository;

  @MockBean
  private TR069EventNotificationService tr069EventNotificationService;

  @Autowired
  BrokerService broker;

  @Test
  public void processInformPnPTest() throws Exception {

    String exampleInform =
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><ns2:Envelope xmlns:ns2=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns1=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:ns4=\"urn:dslforum-org:cwmp-1-0\"><ns2:Header><ns4:ID ns2:mustUnderstand=\"1\">1</ns4:ID></ns2:Header><ns2:Body><ns4:Inform><ns4:DeviceId><Manufacturer></Manufacturer><OUI>0005B9</OUI><ProductClass>LTE_Enterprise_C-RANSC_Cntrl</ProductClass><SerialNumber>0005B9423910</SerialNumber></ns4:DeviceId><Event ns1:arrayType=\"EventStruct[1]\"><EventStruct><EventCode>0 BOOTSTRAP</EventCode><CommandKey></CommandKey></EventStruct></Event><MaxEnvelopes>1</MaxEnvelopes><CurrentTime>2020-06-10T11:27:26.054Z</CurrentTime><RetryCount>0</RetryCount><ParameterList ns1:arrayType=\"ParameterValueStruct[7]\"><ParameterValueStruct><Name>Device.DeviceInfo.HardwareVersion</Name><Value>750742.00.13</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.SoftwareVersion</Name><Value>4.3.00.229</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.DeviceInfo.ProvisioningCode</Name><Value></Value></ParameterValueStruct><ParameterValueStruct><Name>Device.ManagementServer.ConnectionRequestURL</Name><Value>http://172.20.0.9:30150/ConnectionRequest?command=cr&amp;sn=0005B9423910</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.ManagementServer.ParameterKey</Name><Value>zzzz</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.WANDevice.1.WANConnectionDevice.1.WANIPConnection.1.ExternalIPAddress</Name><Value>172.17.19.193</Value></ParameterValueStruct><ParameterValueStruct><Name>Device.Services.FAPService.1.FAPControl.LTE.AdminState</Name><Value>0</Value></ParameterValueStruct></ParameterList></ns4:Inform></ns2:Body></ns2:Envelope>";
    try {

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
