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

package org.commscope.tr069adapter.netconf.restapi;

import java.util.ArrayList;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.InformType;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.netconf.boot.NetConfServiceBooter;
import org.junit.FixMethodOrder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runners.MethodSorters;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = {NetConfServiceBooter.class},
    args = "--schemas-dir test-schemas --debug true --starting-port 17830")
@AutoConfigureMockMvc
public class NotificationServerRestApiTest {

  @Autowired
  NotificationReceiverService service;

  @Test
  public void createNetconfServer() {

    DeviceInform notification = new DeviceInform();

    DeviceDetails deviceDetails = new TR069DeviceDetails();
    notification.setDeviceDetails(deviceDetails);
    List<InformType> notificationTypeList = new ArrayList<>();
    InformType itype = TR069InformType.BOOTSTRAP;
    notificationTypeList.add(itype);
    notification.setInformTypeList(notificationTypeList);
    List<ParameterDTO> parameters = new ArrayList<>();
    ParameterDTO param1 = new ParameterDTO("Device.Info", "info-details");
    parameters.add(param1);
    notification.setParameters(parameters);
    service.processNotification(notification);

  }
}
