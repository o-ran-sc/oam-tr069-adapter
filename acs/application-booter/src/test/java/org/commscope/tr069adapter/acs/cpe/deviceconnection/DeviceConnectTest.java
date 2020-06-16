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

package org.commscope.tr069adapter.acs.cpe.deviceconnection;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.Serializable;

import org.apache.activemq.broker.BrokerService;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.utils.AcsConstants;
import org.commscope.tr069adapter.acs.cpe.handler.ConnectionReqEventHandler;
import org.commscope.tr069adapter.acs.cpe.utils.DeviceConnector;
import org.commscope.tr069adapter.common.timer.TimerServiceManagerAPI;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@SpringBootTest(classes = {ACSServiceBooter.class})
@RunWith(SpringJUnit4ClassRunner.class)
@AutoConfigureMockMvc
@ContextConfiguration
public class DeviceConnectTest {

  @Autowired
  ConnectionReqEventHandler connectionReqEventHandler;

  @MockBean
  private DeviceConnector deviceConnector;

  @MockBean
  private TimerServiceManagerAPI timerServiceManagerAPI;

  @Autowired
  BrokerService broker;

  @Test
  public void deviceConnect() {
    try {
      Mockito.doNothing().when(timerServiceManagerAPI).startTimer(Mockito.anyString(),
          Mockito.anyString(), Mockito.anyLong(), Mockito.any(Serializable.class));

      DeviceRPCResponse response = new DeviceRPCResponse();
      OperationResponse operationResponse = new OperationResponse();
      operationResponse.setOperationCode(TR069OperationCode.INITIATE_CR);
      operationResponse.setStatus(AcsConstants.HTTP_STATUS_OK);
      response.setOperationResponse(operationResponse);
      Mockito.when(deviceConnector.requestConnectionHttp(Mockito.any(TR069DeviceDetails.class)))
          .thenReturn(response);

      TR069DeviceDetails deviceDetails = new TR069DeviceDetails();
      deviceDetails.setDeviceId("0005B9AAAA22");
      deviceDetails.setConnectionRequestURL("http://10.10.10.10:8888/connect/device");

      connectionReqEventHandler.onMessage(deviceDetails);

      assertTrue(true);
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
