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

package org.commscope.tr069adapter.mapper.ves;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.acs.impl.PnPPreProvisioningHandler;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class VESNotificationSender {

  private static final Logger LOG = LoggerFactory.getLogger(VESNotificationSender.class);

  @Autowired
  MapperConfigProperties config;

  @Autowired
  PnPPreProvisioningHandler pnpPreProvisioningHandler;

  @Autowired
  VESNotificationSender vesnotiSender;

  @Autowired
  RestTemplate restTemplate;

  public VESNotificationResponse sendNotification(DeviceInform deviceInform,
      NetConfServerDetails serverInfo) {
    final String uri = getUri();
    LOG.debug("Posting ves event to ves notifier {}", uri);

    VESNotification vesNotifi = new VESNotification();
    if (deviceInform != null) {
      vesNotifi.seteNodeBName(
          pnpPreProvisioningHandler.getEnodeBName(deviceInform.getDeviceDetails().getDeviceId(),
              deviceInform.getDeviceDetails().getSoftwareVersion(),
              deviceInform.getDeviceDetails().getHardwareVersion()));
    } else {
      vesNotifi.seteNodeBName(serverInfo.getEnodeBName());
    }
    vesNotifi.setNetconfDetails(serverInfo);
    vesNotifi.setDevnotification(deviceInform);

    return restTemplate.postForObject(uri, vesNotifi, VESNotificationResponse.class);
  }

  public DeviceRPCResponse sendEditConfigNotification(DeviceRPCRequest deviceRPCRequest) {
    final String uri = config.getVerConfigUri() + "/editConfig";
    LOG.debug("Posting edit config request to ves agent {}", uri);
    return restTemplate.postForObject(uri, deviceRPCRequest, DeviceRPCResponse.class);
  }

  public DeviceRPCResponse sendGetConfigNotification(DeviceRPCRequest deviceRPCRequest) {
    final String uri = config.getVerConfigUri() + "/getConfig";
    LOG.debug("Posting get config request to ves agent {}", uri);
    return restTemplate.postForObject(uri, deviceRPCRequest, DeviceRPCResponse.class);
  }


  private String getUri() {
    return config.getVesUri();
  }

}
