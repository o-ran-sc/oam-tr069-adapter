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
