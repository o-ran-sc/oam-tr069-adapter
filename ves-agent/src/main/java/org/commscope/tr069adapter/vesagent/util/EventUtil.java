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
