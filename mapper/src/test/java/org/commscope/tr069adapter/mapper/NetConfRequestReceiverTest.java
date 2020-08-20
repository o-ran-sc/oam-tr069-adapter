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

import static org.junit.Assert.assertFalse;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.Semaphore;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.acs.ACSRequestSender;
import org.commscope.tr069adapter.mapper.boot.MapperServiceBooter;
import org.commscope.tr069adapter.mapper.model.NetConfRequest;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.netconf.controller.NetConfRequestReceiver;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

@SpringBootTest(classes = {MapperServiceBooter.class})
@RunWith(SpringRunner.class)
public class NetConfRequestReceiverTest {

  @Autowired
  NetConfRequestReceiver receiver;

  @MockBean
  ACSRequestSender tr069RequestSender;

  @MockBean
  Semaphore semaphore;

  @MockBean
  SynchronizedRequestHandler sync;

  @MockBean
  Map<Long, DeviceRPCResponse> opResultMap;

  @Test
  public void getConfigRequestTest() {
    Mockito.when(tr069RequestSender.sendRequest(Mockito.anyObject())).thenReturn(10001L);
    Mockito.when(sync.performDeviceOperation(Mockito.anyObject()))
        .thenReturn(prepareDeviceResponseSucess());

    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc/></lte></cell-config></fap-service></services></device></filter></get-config>");
    NetConfResponse nfres = receiver.getConfigRequest(req);
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertNotNull(nfres.getNetconfResponseXml());
  }

  @Test
  public void getConfigRequestTestTimeOut() throws InterruptedException {
    Mockito.when(tr069RequestSender.sendRequest(Mockito.anyObject())).thenReturn(10001L);
    Mockito.when(semaphore.tryAcquire(Mockito.anyInt(), Mockito.anyObject())).thenReturn(true);
    Mockito.when(opResultMap.remove(Mockito.anyLong())).thenReturn(prepareDeviceResponseSucess());

    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc/></lte></cell-config></fap-service></services></device></filter></get-config>");
    NetConfResponse nfres = receiver.getConfigRequest(req);
    Assert.assertEquals("8006", nfres.getErrorCode().getFaultCode());
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("Operation Timed out", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void getConfigRequestWithNoParamTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"></filter></get-config>");
    NetConfResponse nfres = receiver.getConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());

  }

  @Test
  public void getConfigRequestWithUnknownParamTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"><Intergateway xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc/></lte></cell-config></fap-service></services></Intergateway></filter></get-config>");
    NetConfResponse nfres = receiver.getRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void getRequestTest() {
    Mockito.when(tr069RequestSender.sendRequest(Mockito.anyObject())).thenReturn(10001L);
    Mockito.when(sync.performDeviceOperation(Mockito.anyObject()))
        .thenReturn(prepareDeviceResponseSucess());
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc/></lte></cell-config></fap-service></services></device></filter></get>");
    NetConfResponse nfres = receiver.getRequest(req);
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertNotNull(nfres.getNetconfResponseXml());
  }

  @Test
  public void getRequestWithNoParamTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"></filter></get>");
    NetConfResponse nfres = receiver.getRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void getRequestWithUnknownParamTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><get xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><source><running /></source><filter xmlns:ns0=\"urn:ietf:params:xml:ns:netconf:base:1.0\" ns0:type=\"subtree\"><Intergateway xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc/></lte></cell-config></fap-service></services></Intergateway></filter></get>");
    NetConfResponse nfres = receiver.getRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void setConfigRequestTest() {
    Mockito.when(tr069RequestSender.sendRequest(Mockito.anyObject())).thenReturn(10001L);
    Mockito.when(sync.performDeviceOperation(Mockito.anyObject()))
        .thenReturn(prepareSPVDeviceResponseSucess());
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><target><candidate/></target><config><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc xmlns:a=\"urn:ietf:params:xml:ns:netconf:base:1.0\" a:operation=\"replace\"><plmn-list><index>1</index><plmnid>310767</plmnid><is-primary>false</is-primary><cell-reserved-for-operator-use>true</cell-reserved-for-operator-use><alias>cpe-131</alias><enable>true</enable></plmn-list></epc></lte></cell-config></fap-service></services></device></config></edit-config>");
    NetConfResponse nfres = receiver.setConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void setConfigRequestWithErrorResTest() {
    Mockito.when(tr069RequestSender.sendRequest(Mockito.anyObject())).thenReturn(10001L);
    Mockito.when(sync.performDeviceOperation(Mockito.anyObject()))
        .thenReturn(prepareDeviceResponseError());
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><target><candidate/></target><config><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc xmlns:a=\"urn:ietf:params:xml:ns:netconf:base:1.0\" a:operation=\"replace\"><plmn-list><index>1</index><plmnid>310767</plmnid><is-primary>false</is-primary><cell-reserved-for-operator-use>true</cell-reserved-for-operator-use><alias>cpe-131</alias><enable>true</enable></plmn-list></epc></lte></cell-config></fap-service></services></device></config></edit-config>");
    NetConfResponse nfres = receiver.setConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("9001", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Request denied", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void setConfigRequestWithNoParamTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><target><candidate/></target><config></config></edit-config>");
    NetConfResponse nfres = receiver.setConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void setConfigRequestWithUnknowParatmerTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><target><candidate/></target><config><InternetGateway xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc xmlns:a=\"urn:ietf:params:xml:ns:netconf:base:1.0\" a:operation=\"replace\"><plmn-list><index>1</index><plmnid>310767</plmnid><is-primary>false</is-primary><cell-reserved-for-operator-use>true</cell-reserved-for-operator-use><alias>cpe-131</alias><enable>true</enable></plmn-list></epc></lte></cell-config></fap-service></services></InternetGateway></config></edit-config>");
    NetConfResponse nfres = receiver.setConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void setConfigRequestInvalidRequestXMLTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><target><candidate/></target><confg><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc xmlns:a=\"urn:ietf:params:xml:ns:netconf:base:1.0\" a:operation=\"replace\"><plmn-list><index>1</index><plmnid>310767</plmnid><is-primary>false<is-primary><cell-reserved-for-operator-use>true<cell-reserved-for-operator-use><name>cpe-131</name><trust>true</trust></plmn-list></epc></lte></cell-config></fap-service></services></device></config></edit-config>");
    NetConfResponse nfres = receiver.setConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
  }

  @Test
  public void delConfigRequestTest() {
    Mockito.when(tr069RequestSender.sendRequest(Mockito.anyObject())).thenReturn(10001L);
    Mockito.when(sync.performDeviceOperation(Mockito.anyObject()))
        .thenReturn(prepareSPVDeviceResponseSucess());
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><target><candidate/></target><config><device xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc xmlns:a=\"urn:ietf:params:xml:ns:netconf:base:1.0\" a:operation=\"replace\"><plmn-list><index>1</index><plmnid>310767</plmnid><is-primary>false</is-primary><cell-reserved-for-operator-use>true</cell-reserved-for-operator-use><name>cpe-131</name><trust>true</trust></plmn-list></epc></lte></cell-config></fap-service></services></device></config></edit-config>");
    NetConfResponse nfres = receiver.delConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void delConfigwithUnknownParamterRequestTest() {
    NetConfRequest req = prepareNetConfRequest(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\"><target><candidate/></target><config><InternetGateway xmlns=\"urn:onf:otcc:wireless:yang:radio-access-186\"><services><fap-service xmlns=\"urn:onf:otcc:wireless:yang:radio-access\"><index>1</index><cell-config><lte><epc xmlns:a=\"urn:ietf:params:xml:ns:netconf:base:1.0\" a:operation=\"replace\"><plmn-list><index>1</index><plmnid>310767</plmnid><is-primary>false</is-primary><cell-reserved-for-operator-use>true</cell-reserved-for-operator-use><name>cpe-131</name><trust>true</trust></plmn-list></epc></lte></cell-config></fap-service></services></InternetGateway></config></edit-config>");
    NetConfResponse nfres = receiver.delConfigRequest(req);
    Assert.assertNull(nfres.getNetconfResponseXml());
    Assert.assertEquals("0", nfres.getErrorCode().getFaultCode());
    Assert.assertEquals("Success", nfres.getErrorCode().getErrorMessage());
  }

  @Test
  public void handelRegisterEventTest() {
    try {
      assertFalse(receiver
          .handelRegisterEvent(new NetConfServerDetails("00005BA1", "10.211.55.14", "17830")));
    } catch (Exception e) {
    }

  }

  private NetConfRequest prepareNetConfRequest(String requestXml) {
    NetConfRequest req = new NetConfRequest();
    req.setDeviceId("0005B9A1");
    req.setRequestXml(requestXml);
    req.setSwVersion("4.3.0.0");
    req.setHwVersion("*");
    return req;
  }

  private DeviceRPCResponse prepareDeviceResponseError() {
    DeviceRPCResponse opResult = new DeviceRPCResponse();
    opResult.setFaultKey("9001");
    opResult.setFaultString("Request denied");
    opResult.setDeviceDetails(new DeviceDetails());
    OperationResponse opr = new OperationResponse();
    // opr.setParameterDTOs(getGeneralParams());
    opResult.setOperationResponse(opr);
    return opResult;
  }

  private DeviceRPCResponse prepareDeviceResponseSucess() {
    DeviceRPCResponse opResult = new DeviceRPCResponse();
    opResult.setFaultKey("0");
    opResult.setDeviceDetails(new DeviceDetails());
    OperationResponse opr = new OperationResponse();
    opr.setParameterDTOs(getGeneralParams());
    opResult.setOperationResponse(opr);
    return opResult;
  }

  private DeviceRPCResponse prepareSPVDeviceResponseSucess() {
    DeviceRPCResponse opResult = new DeviceRPCResponse();
    opResult.setFaultKey("0");
    opResult.setDeviceDetails(new DeviceDetails());
    OperationResponse opr = new OperationResponse();
    opResult.setOperationResponse(opr);
    return opResult;
  }

  private ArrayList<ParameterDTO> getGeneralParams() {
    ArrayList<ParameterDTO> params = new ArrayList<>();
    params.add(new ParameterDTO("Device.DeviceInfo.ManufacturerOUI", "0005B9"));
    params.add(new ParameterDTO("Device.DeviceInfo.ProductClass", "LTE_Enterprise_C-RANSC_Cntrl"));
    params.add(new ParameterDTO("Device.Services.FAPService.1.CellConfig.LTE.EPC.PLMNList.1.PLMNID",
        "30324"));
    return params;
  }
}
