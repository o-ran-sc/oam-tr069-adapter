package org.commscope.tr069adapter.vesagent.util;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.vesagent.VesConfiguration;
import org.commscope.tr069adapter.vesagent.model.CommonEventHeader;

public class EventUtil {
  private EventUtil() {

  }

  public static void populateEventHeaderFields(CommonEventHeader eventHeader,
      DeviceInform notification, String eNodeBName, VesConfiguration config) {
    eventHeader.setLastEpochMicrosec(System.currentTimeMillis());
    eventHeader.setSequence(0);
    populateEnodeBName(eventHeader, notification, eNodeBName);

    eventHeader.setStartEpochMicrosec(System.currentTimeMillis());
    eventHeader.setVersion(config.getEventVersion());
    eventHeader.setNfNamingCode("");
    eventHeader.setNfcNamingCode("");
    eventHeader.setNfVendorName(config.getVendorName());
    eventHeader.setVesEventListenerVersion(config.getVesVersion());
  }

  public static void populateEnodeBName(CommonEventHeader eventHeader, DeviceInform notification,
      String eNodeBName) {
    if (eNodeBName == null) {
      eventHeader.setReportingEntityName(notification.getDeviceDetails().getDeviceId());
      eventHeader.setReportingEntityId(notification.getDeviceDetails().getDeviceId());
      eventHeader.setSourceId(notification.getDeviceDetails().getDeviceId());
      eventHeader.setSourceName(notification.getDeviceDetails().getDeviceId());
    } else {
      eventHeader.setReportingEntityName(eNodeBName);
      eventHeader.setSourceName(eNodeBName);
      eventHeader.setReportingEntityId(notification.getDeviceDetails().getDeviceId());
      eventHeader.setSourceId(notification.getDeviceDetails().getDeviceId());
    }
  }

}
