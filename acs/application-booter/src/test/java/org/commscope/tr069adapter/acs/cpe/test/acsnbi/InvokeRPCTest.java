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

package org.commscope.tr069adapter.acs.cpe.test.acsnbi;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import org.apache.activemq.broker.BrokerService;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.cpe.test.RequestProcessorTestsUtils;
import org.commscope.tr069adapter.acs.nbi.mapper.service.MapperRequestRESTService;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@SpringBootTest(classes = {ACSServiceBooter.class})
@RunWith(SpringJUnit4ClassRunner.class)
@AutoConfigureMockMvc
@ContextConfiguration
public class InvokeRPCTest {


  @MockBean
  private JmsTemplate jmsTemplate;

  @Autowired
  private MapperRequestRESTService mapperRequestRestService;

  @Autowired
  BrokerService broker;

  @Test
  public void processInformPnPTest() throws Exception {


    try {
      Mockito.doNothing().when(jmsTemplate).convertAndSend(Mockito.anyString(),
          Mockito.any(Object.class));

      DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
      deviceRPCRequest.setOperationId(10L);
      deviceRPCRequest.setDeviceDetails(RequestProcessorTestsUtils.getDeviceDetails());
      deviceRPCRequest.setOpDetails(
          RequestProcessorTestsUtils.getOperationDetails(TR069OperationCode.SET_PARAMETER_VALUES));
      deviceRPCRequest.setOptions(RequestProcessorTestsUtils.getOperationOptions(60000L));
      Long operationId = mapperRequestRestService.initiateDeviceOperation(deviceRPCRequest);

      assertNotNull(operationId);
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
