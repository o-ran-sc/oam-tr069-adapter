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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.inform.BootInform;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.acs.common.inform.ValueChangeInform;
import org.commscope.tr069adapter.mapper.acs.impl.ACSNotificationHandlerImpl;
import org.commscope.tr069adapter.mapper.boot.MapperServiceBooter;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.NetconfServerManagementError;
import org.commscope.tr069adapter.mapper.netconf.NetConfServerManager;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

@SpringBootTest(classes = {MapperServiceBooter.class})
@RunWith(SpringRunner.class)
public class ACSNotificationHandlerImplTest {

  @Autowired
  ACSNotificationHandlerImpl aCSNotificationHandlerImpl;

  @Mock
  NetConfServerManager netConfServerManager;

  @Test
  public void testHandleBootStrapNotification() {

    BootstrapInform notification = new BootstrapInform();
    List<ParameterDTO> params = new ArrayList<ParameterDTO>();
    ParameterDTO param = new ParameterDTO();
    param.setParamName("ExpeditedEvent");
    params.add(param);
    notification.setDeviceDetails(getDeviceDetails());
    notification.setParameters(params);
    NetConfServerDetails serverInfo = new NetConfServerDetails();
    serverInfo.setError(NetconfServerManagementError.SUCCESS);
    Mockito.when(netConfServerManager.createNetconfServer(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(serverInfo);
    /*
     * String URI = "http://tr069adapter-netconf-server:8181/netConfServerManagerService";
     * Mockito.when(netConfServerManager.getNetconfServerManagerRestUri()). thenReturn(value)
     */
    aCSNotificationHandlerImpl.handleNotification(notification);
    assertNull(notification.getValueChangeNotification());
  }

  @Test
  public void testHandleBootStrapNotificationNegativeScenario() {

    BootstrapInform notification = new BootstrapInform();
    List<ParameterDTO> params = new ArrayList<ParameterDTO>();
    ParameterDTO param = new ParameterDTO();
    param.setParamName("Test");
    params.add(param);
    notification.setDeviceDetails(getDeviceDetails());
    notification.setParameters(params);
    // Mockito.doReturn(null).when(netConfServerManager.createNetconfServer(Mockito.anyString(),
    // Mockito.anyString()));
    aCSNotificationHandlerImpl.handleNotification(notification);
    assertNull(notification.getValueChangeNotification());
  }

  @Test
  public void testHandleBootNotification() {

    BootInform notification = new BootInform();
    List<ParameterDTO> params = new ArrayList<ParameterDTO>();
    ParameterDTO param = new ParameterDTO();
    param.setParamName("Test");
    params.add(param);
    notification.setDeviceDetails(getDeviceDetails());
    notification.setParameters(params);
    aCSNotificationHandlerImpl.handleNotification(notification);
    assertNull(notification.getValueChangeNotification());
  }

  @Test
  public void testGetDeviceBootNotification() {
    DeviceInform deviceInform = new DeviceInform();
    deviceInform.setDeviceDetails(getDeviceDetails());
    deviceInform.setParameters(getGeneralParams());

    BootInform bInform =
        ACSNotificationHandlerImpl.getDeviceBootNotification(deviceInform, TR069InformType.BOOT);
    assertEquals("00005B9A1", bInform.getDeviceDetails().getDeviceId());
    assertNotNull(bInform.getInformTypeList());
    assertEquals(TR069InformType.BOOT, bInform.getInformTypeList().get(0));
  }

  @Test
  public void testGetDeviceBooStraptNotification() {
    DeviceInform deviceInform = new DeviceInform();
    deviceInform.setDeviceDetails(getDeviceDetails());
    deviceInform.setParameters(getGeneralParams());

    BootstrapInform bsInform = ACSNotificationHandlerImpl
        .getDeviceBootStrapNotification(deviceInform, TR069InformType.BOOTSTRAP);
    assertEquals("00005B9A1", bsInform.getDeviceDetails().getDeviceId());
    assertNotNull(bsInform.getInformTypeList());
    assertEquals(TR069InformType.BOOTSTRAP, bsInform.getInformTypeList().get(0));
  }

  @Test
  public void testGetDeviceValueChangeNotification() {
    DeviceInform deviceInform = new DeviceInform();
    deviceInform.setDeviceDetails(getDeviceDetails());
    deviceInform.setParameters(getGeneralParams());

    ValueChangeInform vInform = ACSNotificationHandlerImpl
        .getDeviceValueChangeNotification(deviceInform, TR069InformType.VALUECHANGE);
    assertEquals("00005B9A1", vInform.getDeviceDetails().getDeviceId());
    assertNotNull(vInform.getInformTypeList());
    assertEquals(TR069InformType.VALUECHANGE, vInform.getInformTypeList().get(0));
  }

  private DeviceDetails getDeviceDetails() {
    DeviceDetails nf = new DeviceDetails();
    nf.setDeviceId("00005B9A1");
    nf.setDeviceTypeId(50);
    nf.setOui("0005B9");
    nf.setProductClass("LTE_Enterprise_C-RANSC_Cntrl");
    return nf;
  }

  private DeviceDetails getDeviceDetailsNegativeCase() {
    DeviceDetails nf = new DeviceDetails();
    // nf.setDeviceId("00005B9A1");
    nf.setDeviceTypeId(50);
    nf.setOui("0005B9");
    nf.setProductClass("LTE_Enterprise_C-RANSC_Cntrl");
    return nf;
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
}
