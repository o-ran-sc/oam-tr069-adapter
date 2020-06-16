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

package org.commscope.tr069adapter.acs.cpe.test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.apache.activemq.broker.BrokerService;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRepository;
import org.commscope.tr069adapter.acs.requestprocessor.dao.SessionRepository;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionState;
import org.commscope.tr069adapter.acs.requestprocessor.handler.DeviceRPCRequestHandler;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.test.context.junit4.SpringRunner;



/**
 * 
 * @version 1.0
 * @since May 28, 2020
 * @author Prashant Kumar
 */

@RunWith(SpringRunner.class)
@SpringBootTest(classes = {ACSServiceBooter.class})
public class DeviceRPCRequestHandlerTests {

  // @Autowired
  // private MockMvc mockMvc;

  @Autowired
  BrokerService broker;

  @Autowired
  DeviceRPCRequestHandler deviceRPCRequestHandler;

  @MockBean
  DeviceRepository deviceRepository;

  @MockBean
  SessionRepository sessionRepository;

  @MockBean
  private JmsTemplate jmsTemplate;


  @Test
  public void spvSuccessTest() {

    Mockito.when(deviceRepository.findByDeviceId(RequestProcessorTestsUtils.macId))
        .thenReturn(RequestProcessorTestsUtils.getTR069DeviceEntity());

    Mockito.when(sessionRepository.findByDeviceId(RequestProcessorTestsUtils.macId))
        .thenReturn(RequestProcessorTestsUtils.getSessionManagerEntity());

    Mockito.doNothing().when(jmsTemplate).convertAndSend(Mockito.anyString(),
        Mockito.any(Object.class));

    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    deviceRPCRequest.setOperationId(10L);
    deviceRPCRequest.setDeviceDetails(RequestProcessorTestsUtils.getDeviceDetails());
    deviceRPCRequest.setOpDetails(
        RequestProcessorTestsUtils.getOperationDetails(TR069OperationCode.SET_PARAMETER_VALUES));
    deviceRPCRequest.setOptions(RequestProcessorTestsUtils.getOperationOptions(60000L));

    try {
      deviceRPCRequestHandler.onMessage(deviceRPCRequest);
      assertTrue(true);
    } catch (Exception e) {
      fail(e.getMessage());
    }
  }

  @Test
  public void spvWithTerminatedSessionTest() {

    Mockito.when(deviceRepository.findByDeviceId(RequestProcessorTestsUtils.macId))
        .thenReturn(RequestProcessorTestsUtils.getTR069DeviceEntity());

    Mockito.when(sessionRepository.findByDeviceId(RequestProcessorTestsUtils.macId))
        .thenReturn(RequestProcessorTestsUtils.getSessionManagerEntity(SessionState.TERMINATED));

    Mockito.doNothing().when(jmsTemplate).convertAndSend(Mockito.anyString(),
        Mockito.any(Object.class));

    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    deviceRPCRequest.setOperationId(10L);
    deviceRPCRequest.setDeviceDetails(RequestProcessorTestsUtils.getDeviceDetails());
    deviceRPCRequest.setOpDetails(
        RequestProcessorTestsUtils.getOperationDetails(TR069OperationCode.SET_PARAMETER_VALUES));
    deviceRPCRequest.setOptions(RequestProcessorTestsUtils.getOperationOptions(60000L));

    try {
      deviceRPCRequestHandler.onMessage(deviceRPCRequest);
      assertTrue(true);
    } catch (Exception e) {
      fail(e.getMessage());
    }
  }


  //
  // @Test
  // public void concurrentSessionExceptionTest() throws SessionConcurrentAccessException {
  //
  // Mockito.when(deviceRepository.findByDeviceId(RequestProcessorTestsUtils.macId))
  // .thenReturn(RequestProcessorTestsUtils.getTR069DeviceEntity());
  //
  // Mockito.when(sessionManager.getLockedSession(RequestProcessorTestsUtils.macId))
  // .thenThrow(new SessionConcurrentAccessException("concurrent session modification"));
  //
  //
  // DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
  // deviceRPCRequest.setOperationId(10L);
  // deviceRPCRequest.setDeviceDetails(RequestProcessorTestsUtils.getDeviceDetails());
  // deviceRPCRequest.setOpDetails(RequestProcessorTestsUtils.getOperationDetails(TR069OperationCode.SetParameterValues));
  // deviceRPCRequest.setOptions(RequestProcessorTestsUtils.getOperationOptions(60000L));
  //
  // try {
  // deviceRPCRequestHandler.onMessage(deviceRPCRequest);
  // fail("concurrent session modification use case failed");
  // } catch (Exception e) {
  // assertTrue(true);
  // }
  // }

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
