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
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.boot.MapperServiceBooter;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.mapper.util.NetconfToTr069MapperUtil;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

@SpringBootTest(classes = {MapperServiceBooter.class})
@RunWith(SpringRunner.class)
public class NetconfToTr069MapperUtilTest {

  @Autowired
  NetconfToTr069MapperUtil mapUtil;

  @Test
  public void getNetconfResponseTest() {
    DeviceRPCResponse opResult = new DeviceRPCResponse();
    opResult.setFaultKey("0");
    opResult.setDeviceDetails(new DeviceDetails());
    OperationResponse opr = new OperationResponse();
    opr.setParameterDTOs(getGeneralParams());
    opResult.setOperationResponse(opr);
    NetConfResponse netConfRes = mapUtil.getNetconfResponse(opResult, "4.3.0.0", "*", false);
    Assert.assertNotNull(netConfRes);
    Assert.assertEquals("0", netConfRes.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", netConfRes.getErrorCode().getErrorMessage());
  }

  @Test
  public void getNetconfResponseErrorTest() {
    DeviceRPCResponse opResult = new DeviceRPCResponse();
    opResult.setFaultKey("9001");
    opResult.setFaultString("Request denied");
    opResult.setDeviceDetails(new DeviceDetails());
    OperationResponse opr = new OperationResponse();
    opr.setParameterDTOs(getGeneralParams());
    opResult.setOperationResponse(opr);
    NetConfResponse netConfRes = mapUtil.getNetconfResponse(opResult, "4.3.0.0", "*", false);
    Assert.assertNotNull(netConfRes);
    Assert.assertEquals("9001", netConfRes.getErrorCode().getFaultCode());
    Assert.assertEquals("Request denied", netConfRes.getErrorCode().getErrorMessage());
  }

  @Test
  public void getNetconfResponseWithoutParamtersTest() {
    DeviceRPCResponse opResult = new DeviceRPCResponse();
    opResult.setFaultKey("0");
    opResult.setDeviceDetails(new DeviceDetails());
    OperationResponse opr = new OperationResponse();
    opResult.setOperationResponse(opr);
    NetConfResponse netConfRes = mapUtil.getNetconfResponse(opResult, "4.3.0.0", "*", false);
    Assert.assertNotNull(netConfRes);
    Assert.assertEquals("0", netConfRes.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", netConfRes.getErrorCode().getErrorMessage());
    Assert.assertNull(netConfRes.getNetconfResponseXml());
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
}
