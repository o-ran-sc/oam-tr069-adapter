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

package org.commscope.tr069adapter.mapper.netconf;

import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

@Component
public class NetConfServerManager {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfServerManager.class);

  private static String createServerSVC = "createServer";

  @Autowired
  MapperConfigProperties config;

  @Autowired
  RestTemplate restTemplate;

  public NetConfServerDetails createNetconfServer(String deviceID, String enodeBName,
      String swVersion, String hwVersion) {

    NetConfServerDetails result = null;
    // handle exception
    final String uri = getNetconfServerManagerRestUri() + "/" + createServerSVC;
    LOG.debug("Sending create netconf server request for device id {}", deviceID);
    try {
      MultiValueMap<String, String> uriParams = new LinkedMultiValueMap<>();
      uriParams.add("deviceId", deviceID);
      uriParams.add("enodeBName", enodeBName);
      uriParams.add("swVersion", swVersion);
      uriParams.add("hwVersion", hwVersion);
      HttpHeaders headers = new HttpHeaders();
      headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
      final HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(uriParams, headers);
      ResponseEntity<NetConfServerDetails> res =
          restTemplate.postForEntity(uri, entity, NetConfServerDetails.class);
      result = res.getBody();
      LOG.debug("Successfully created netconf server for device id. {} , response= {}", deviceID,
          result);
    } catch (Exception e) {
      LOG.error("Exception while creating netconf server request for device id {}", deviceID, e);
    }
    return result;
  }

  private String getNetconfServerManagerRestUri() {
    return config.getNbiServerManagerUri();
  }

}
