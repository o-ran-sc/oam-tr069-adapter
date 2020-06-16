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

import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.ConfigurationData;
import org.commscope.tr069adapter.acs.common.inform.BootInform;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.mapper.acs.impl.PnPPreProvisioningHandler;
import org.commscope.tr069adapter.mapper.boot.MapperServiceBooter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

@SpringBootTest(classes = {MapperServiceBooter.class})
@RunWith(SpringRunner.class)
public class PnPPreProvisioningHandlerTest {

  @Autowired
  PnPPreProvisioningHandler pnPPreProvisioningHandler;

  @Mock
  RestTemplate restTemplate;

  @Test
  public void testGetEnodeBName() {
    ConfigurationData configData = new ConfigurationData();
    Map<String, String> paramMap = new HashMap<String, String>();
    paramMap.put("X_0005B9_eNBName", "Enodb1");
    configData.setParameterMONameValueMap(paramMap);
    Mockito.when(restTemplate.getForObject(Mockito.anyString(), Mockito.any()))
        .thenReturn(configData);
    String eNodeBName = pnPPreProvisioningHandler.getEnodeBName("00005B9A1");
    assertNull(eNodeBName);
  }

  @Test
  public void testOnDeviceBootStrapNotification() {
    BootstrapInform notification = new BootstrapInform();
    List<ParameterDTO> params = new ArrayList<ParameterDTO>();
    ParameterDTO param = new ParameterDTO();
    param.setParamName("ExpeditedEvent");
    params.add(param);
    notification.setDeviceDetails(getDeviceDetails());
    notification.setParameters(params);
    pnPPreProvisioningHandler.onDeviceNotification(notification);
    assertNull(notification.getValueChangeNotification());
  }

  @Test
  public void testOnDeviceBootNotification() {
    BootInform notification = new BootInform();
    List<ParameterDTO> params = new ArrayList<ParameterDTO>();
    ParameterDTO param = new ParameterDTO();
    param.setParamName("ExpeditedEvent");
    params.add(param);
    notification.setDeviceDetails(getDeviceDetails());
    notification.setParameters(params);
    pnPPreProvisioningHandler.onDeviceNotification(notification);
    assertNull(notification.getValueChangeNotification());
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
