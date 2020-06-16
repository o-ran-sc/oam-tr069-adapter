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

package org.commscope.tr069adapter.vesagent.test;

import com.fasterxml.jackson.core.JsonProcessingException;

import java.util.ArrayList;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.InformType;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.acs.common.inform.PeriodicInform;
import org.commscope.tr069adapter.acs.common.inform.ValueChangeInform;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.vesagent.boot.VESAgentServiceBooter;
import org.commscope.tr069adapter.vesagent.controller.VESAgentService;
import org.commscope.tr069adapter.vesagent.exception.InvalidFaultOperationException;
import org.commscope.tr069adapter.vesagent.http.HttpRequestSender;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit4.SpringRunner;

@SpringBootTest(classes = {VESAgentServiceBooter.class})
@RunWith(SpringRunner.class)
// @WebMvcTest(VESAgentService.class)
public class VESAgentServiceRestTest {

  @Autowired
  VESAgentService agent;

  @MockBean
  HttpRequestSender sender;

  @Test
  public void processPnfEventTest() throws JsonProcessingException, InvalidFaultOperationException {
    VESNotificationResponse res =
        new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
    Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
    VESNotification ves = new VESNotification();
    ves.seteNodeBName("0005B9A1");
    ves.setNetconfDetails(getNetConfDetails());
    DeviceInform inform = new BootstrapInform();
    inform.setDeviceDetails(getDeviceDetails());
    ArrayList<InformType> list = new ArrayList<>();
    list.add(TR069InformType.BOOTSTRAP);
    inform.setInformTypeList(list);
    inform.setParameters(getGeneralParams());
    ves.setDevnotification(inform);

    VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
    Assert.assertNotNull(vesResponse);
    Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());

  }

  @Test
  public void processPnfEventWhenEnbNullTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName(null);
      ves.setNetconfDetails(getNetConfDetails());
      DeviceInform inform = new BootstrapInform();
      inform.setDeviceDetails(getDeviceDetails());
      ArrayList<InformType> list = new ArrayList<>();
      list.add(TR069InformType.BOOTSTRAP);
      inform.setInformTypeList(list);
      inform.setParameters(getGeneralParams());
      ves.setDevnotification(inform);

      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
      res.getStatusCode();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processFaultEventCriticalTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName("0005B9A1");
      ves.setNetconfDetails(getNetConfDetails());
      DeviceInform inform = new ValueChangeInform();
      inform.setDeviceDetails(getDeviceDetails());
      ArrayList<InformType> list = new ArrayList<>();
      list.add(TR069InformType.VALUECHANGE);
      inform.setInformTypeList(list);
      inform.setParameters(getFaultCritcalParams());
      ves.setDevnotification(inform);

      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processFaultEventWhenEnbNullTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName(null);
      ves.setNetconfDetails(getNetConfDetails());
      DeviceInform inform = new ValueChangeInform();
      inform.setDeviceDetails(getDeviceDetails());
      ArrayList<InformType> list = new ArrayList<>();
      list.add(TR069InformType.VALUECHANGE);
      inform.setInformTypeList(list);
      inform.setParameters(getFaultCritcalParams());
      ves.setDevnotification(inform);

      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);

      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processFaultEventClearTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName("0005B9A1");
      ves.setNetconfDetails(getNetConfDetails());
      DeviceInform inform = new ValueChangeInform();
      inform.setDeviceDetails(getDeviceDetails());
      ArrayList<InformType> list = new ArrayList<>();
      list.add(TR069InformType.VALUECHANGE);
      inform.setInformTypeList(list);
      inform.setParameters(getFaultClearParams());
      ves.setDevnotification(inform);

      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
      res.getStatusCode();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processFaultEventTimeZoneTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName("0005B9A1");
      ves.setNetconfDetails(getNetConfDetails());
      DeviceInform inform = new ValueChangeInform();
      inform.setDeviceDetails(getDeviceDetails());
      ArrayList<InformType> list = new ArrayList<>();
      list.add(TR069InformType.VALUECHANGE);
      inform.setInformTypeList(list);
      inform.setParameters(getFaultClearParams());
      ves.setDevnotification(inform);

      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
      res.getStatusCode();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processHBEventTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName("0005B9A1");
      ves.setNetconfDetails(getNetConfDetails());
      DeviceInform inform = new PeriodicInform();
      inform.setDeviceDetails(getDeviceDetails());
      ArrayList<InformType> list = new ArrayList<>();
      list.add(TR069InformType.PERIODIC);
      inform.setInformTypeList(list);
      inform.setParameters(getGeneralParams());
      ves.setDevnotification(inform);

      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
      res.getStatusCode();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processHBEventWhenEnbNullTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName(null);
      ves.setNetconfDetails(getNetConfDetails());
      DeviceInform inform = new PeriodicInform();
      inform.setDeviceDetails(getDeviceDetails());
      ArrayList<InformType> list = new ArrayList<>();
      list.add(TR069InformType.PERIODIC);
      inform.setInformTypeList(list);
      inform.setParameters(getGeneralParams());
      ves.setDevnotification(inform);

      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
      res.getStatusCode();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processOnRestartEventTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName("0005B9A1");
      ves.setNetconfDetails(getNetConfDetails());
      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  public void processOnRestartEventWhenEnbNullTest() {
    try {
      VESNotificationResponse res =
          new VESNotificationResponse(HttpStatus.ACCEPTED.value(), "Sucess");
      Mockito.when(sender.postRequest(Mockito.anyString(), Mockito.anyString())).thenReturn(res);
      VESNotification ves = new VESNotification();
      ves.seteNodeBName("0005B9A1");
      ves.setNetconfDetails(getNetConfDetails());
      ves.getNetconfDetails().setEnodeBName(null);
      VESNotificationResponse vesResponse = agent.processDeviceNotificationAsVESEvent(ves);
      Assert.assertNotNull(vesResponse);
      Assert.assertEquals(HttpStatus.ACCEPTED.value(), vesResponse.getStatusCode());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }


  private ArrayList<ParameterDTO> getFaultClearParams() {
    ArrayList<ParameterDTO> params = new ArrayList<>();
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.EventTime", ""));
    params
        .add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.AlarmIdentifier", "0005B9B5-100"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.NotificationType", "NewAlarm"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.ManagedObjectInstance",
        "Device.Services.FAPService.{i}.FAPControl.LTE"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.EventType", "S1 Connection"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.ProbableCause",
        "S1 connection failure"));
    params.add(
        new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.SpecificProblem", "Unsupported PLMN"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.PerceivedSeverity", "CLEAR"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.AdditionalText",
        "S1 connection setup FAILED Addional Text"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.AdditionalInformation",
        "S1 connection setup FAILED Aditional Inf"));
    return params;
  }

  private ArrayList<ParameterDTO> getFaultCritcalParams() {
    ArrayList<ParameterDTO> params = new ArrayList<>();
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.EventTime", ""));
    params
        .add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.AlarmIdentifier", "0005B9B5-100"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.NotificationType", "NewAlarm"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.ManagedObjectInstance",
        "Device.Services.FAPService.{i}.FAPControl.LTE"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.EventType", "S1 Connection"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.ProbableCause",
        "S1 connection failure"));
    params.add(
        new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.SpecificProblem", "Unsupported PLMN"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.PerceivedSeverity", "CRITICAL"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.AdditionalText",
        "S1 connection setup FAILED Addional Text"));
    params.add(new ParameterDTO("Device.FaultMgmt.ExpeditedEvent.1.AdditionalInformation",
        "S1 connection setup FAILED Aditional Inf | TZD=UTC+00.00"));
    return params;
  }



  private ArrayList<ParameterDTO> getGeneralParams() {
    ArrayList<ParameterDTO> params = new ArrayList<>();
    params.add(new ParameterDTO("Device.DeviceInfo.ManufacturerOUI", "0005B9"));
    params.add(new ParameterDTO("Device.DeviceInfo.ProductClass", "LTE_Enterprise_C-RANSC_Cntrl"));
    params.add(new ParameterDTO("Device.DeviceInfo.HardwareVersion", "750742.00.04"));
    params.add(new ParameterDTO("Device.DeviceInfo.SoftwareVersion", "4.3.00.231"));
    params.add(new ParameterDTO("Device.DeviceInfo.Manufacturer", "ORAN"));
    params.add(new ParameterDTO("Device.DeviceInfo.SerialNumber", "00005B9A1"));
    params.add(new ParameterDTO("Device.IP.Interface.1.IPv4Address.1.IPAddress", "10.211.5.55"));
    return params;
  }

  private NetConfServerDetails getNetConfDetails() {
    NetConfServerDetails nf = new NetConfServerDetails();
    nf.setDeviceId("00005B9A1");
    nf.setEnodeBName("0005B9A1");
    nf.setListenAddress("10.211.5.27");
    nf.setListenPort("17830");
    return nf;
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
