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

package org.commscope.tr069adapter.netconf.server.ves;

import org.commscope.tr069adapter.mapper.model.VESNotification;
import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class VESNotificationSender {

  private static final Logger LOG = LoggerFactory.getLogger(VESNotificationSender.class);
  private RestTemplate restTemplate = new RestTemplate();

  @Autowired
  NetConfServerProperties config;

  public VESNotificationResponse sendDeleteConfigNotification(VESNotification vesNotification) {
    final String uri = getUri() + "/deleteConfig";
    LOG.debug("Posting delete-config request to ves agent {}", uri);
    return restTemplate.postForObject(uri, vesNotification, VESNotificationResponse.class);
  }

  private String getUri() {
    return config.getVesURI();
  }
}
