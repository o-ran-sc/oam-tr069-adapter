package org.commscope.tr069adapter.netconf.restapi;

import org.commscope.tr069adapter.mapper.model.NetConfNotificationDTO;
import org.commscope.tr069adapter.netconf.notification.NotificationHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/netConfNotificationService")
public class NotificationReceiverService {

  private static final Logger LOG = LoggerFactory.getLogger(NotificationReceiverService.class);

  @Autowired
  NotificationHandler handler;

  @PostMapping("/notification")
  public void processNotification(@RequestBody NetConfNotificationDTO netConNotifDTO) {
    System.out.println("Received NetConf Notification :" + netConNotifDTO);
    LOG.debug("Received NetConf Notification :" + netConNotifDTO);
    handler.handleNetConfNotification(netConNotifDTO);
    LOG.debug("Processed NetConf Notification for :" + netConNotifDTO);
    System.out.println("Processed NetConf Notification for :" + netConNotifDTO);
  }
}
