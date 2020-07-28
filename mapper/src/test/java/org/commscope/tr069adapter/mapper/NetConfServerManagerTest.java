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

package org.commscope.tr069adapter.mapper;

import java.util.ArrayList;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.InformType;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.mapper.boot.MapperServiceBooter;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.mapper.netconf.NetConfNotificationSender;
import org.commscope.tr069adapter.mapper.netconf.NetConfServerManager;
import org.commscope.tr069adapter.mapper.ves.VESNotificationSender;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(classes = {MapperServiceBooter.class})
@RunWith(SpringRunner.class)
public class NetConfServerManagerTest {

  @Autowired
  NetConfServerManager server;

  @Autowired
  NetConfNotificationSender nfSender;

  @Autowired
  VESNotificationSender vesSender;

  @MockBean
  RestTemplate restTemplate;

  @Test
  public void createServerTest() {
    ResponseEntity<Object> res = new ResponseEntity<Object>(
        new NetConfServerDetails("0005B9A1", "10.221.55.14", "17830"), HttpStatus.ACCEPTED);
    Mockito.when(
        restTemplate.postForEntity(Mockito.anyString(), Mockito.anyObject(), Mockito.anyObject()))
        .thenReturn(res);
    NetConfServerDetails nfDetails = server.createNetconfServer("0005B9A1", "EnodB1", "4.3.0.0", "*");

    Assert.assertNotNull(nfDetails);
    Assert.assertEquals("17830", nfDetails.getListenPort());
  }

  @Test
  public void createServerFailureWithRestCallTest() {
    NetConfServerDetails nfDetails = server.createNetconfServer("0005B9A1", "EnodB1", "4.3.0.0", "*");
    Assert.assertNull(nfDetails);
  }

  @Test
  public void sendNotificaionTest() {
    ResponseEntity res = new ResponseEntity<>(HttpStatus.ACCEPTED);
    Mockito.when(
        restTemplate.postForObject(Mockito.anyString(), Mockito.anyObject(), Mockito.anyObject()))
        .thenReturn(res);
    DeviceInform inform = new BootstrapInform();
    inform.setDeviceDetails(getDeviceDetails());
    ArrayList<InformType> list = new ArrayList<>();
    list.add(TR069InformType.BOOTSTRAP);
    inform.setInformTypeList(list);
    inform.setParameters(getGeneralParams());
    ResponseEntity response = nfSender.sendNotification(inform);
    Assert.assertNotNull(response);
    Assert.assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
  }

  @Test
  public void sendNotificaionInformNullTest() {
    ResponseEntity res = new ResponseEntity<>(HttpStatus.ACCEPTED);
    Mockito.when(
        restTemplate.postForObject(Mockito.anyString(), Mockito.anyObject(), Mockito.anyObject()))
        .thenReturn(res);
    ResponseEntity response = nfSender.sendNotification(null);
    Assert.assertNull(response);
  }

  @Test
  public void sendVesEventTest() {

    Mockito.when(
        restTemplate.postForObject(Mockito.anyString(), Mockito.anyObject(), Mockito.anyObject()))
        .thenReturn(new VESNotificationResponse(202, "sucess"));

    DeviceInform inform = new BootstrapInform();
    inform.setDeviceDetails(getDeviceDetails());
    ArrayList<InformType> list = new ArrayList<InformType>();
    list.add(TR069InformType.BOOTSTRAP);
    inform.setInformTypeList(list);
    inform.setParameters(getGeneralParams());
    VESNotificationResponse response = vesSender.sendNotification(inform,
        new NetConfServerDetails("0005B9A1", "10.221.55.14", "17830"));
    Assert.assertNotNull(response);
    Assert.assertEquals(202, response.getStatusCode());
  }

  private ArrayList<ParameterDTO> getGeneralParams() {
    ArrayList<ParameterDTO> params = new ArrayList<>();
    params.add(new ParameterDTO("Device.DeviceInfo.ManufacturerOUI", "0005B9"));
    params.add(new ParameterDTO("Device.DeviceInfo.ProductClass", "LTE_Enterprise_C-RANSC_Cntrl"));
    params.add(new ParameterDTO("Device.Services.FAPService.1.CellConfig.LTE.EPC.PLMNList.1.PLMNID",
        "30324"));
    params.add(new ParameterDTO(
        "Device.Services.FAPService.1.CellConfig.LTE.EPC.PLMNList.1.IsPrimary", "1"));
    params.add(
        new ParameterDTO("Device.Services.FAPService.1.CellConfig.LTE.EPC.PLMNList.1.Enable", "0"));
    return params;
  }

  private DeviceDetails getDeviceDetails() {
    DeviceDetails nf = new DeviceDetails();
    nf.setDeviceId("00005B9A1");
    nf.setDeviceTypeId(50);
    nf.setOui("0005B9");
    nf.setProductClass("LTE_Enterprise_C-RANSC_Cntrl");
    return nf;
  }
}
