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
import java.util.Date;

import org.apache.activemq.broker.BrokerService;
import org.commscope.tr069adapter.acs.booter.ACSServiceBooter;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.utils.AcsConstants;
import org.commscope.tr069adapter.acs.cpe.handler.ConnectionReqEventHandler;
import org.commscope.tr069adapter.acs.cpe.utils.DeviceConnector;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRepository;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceEntity;
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

  @MockBean
  private DeviceRepository deviceRepository;


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

      TR069DeviceEntity tr069DeviceEntity = new TR069DeviceEntity();
      tr069DeviceEntity.setDeviceId(deviceDetails.getDeviceId());
      tr069DeviceEntity.setUserName(deviceDetails.getUsername());
      tr069DeviceEntity.setPassword(deviceDetails.getPassword());
      tr069DeviceEntity.setLastUpdatedTime(new Date());
      tr069DeviceEntity.setConnStatus(true);
      tr069DeviceEntity.setErrorMsg(null);

      if (deviceDetails.getSoftwareVersion() != null) {
        tr069DeviceEntity.setSwVersion(deviceDetails.getSoftwareVersion());
      }
      if (deviceDetails.getHardwareVersion() != null) {
        tr069DeviceEntity.setHwVersion(deviceDetails.getHardwareVersion());
      }
      Mockito.when(deviceRepository.findByDeviceId(Mockito.anyString()))
          .thenReturn(tr069DeviceEntity);

      Mockito.when(deviceRepository.save(Mockito.any(TR069DeviceEntity.class)))
          .thenReturn(tr069DeviceEntity);

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
