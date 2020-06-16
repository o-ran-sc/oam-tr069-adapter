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

package org.commscope.tr069adapter.acs.common.dto;

import org.commscope.tr069adapter.acs.common.InformType;

public enum TR069InformType implements InformType {

  BOOTSTRAP("0 BOOTSTRAP"), BOOT("1 BOOT"), PERIODIC("2 PERIODIC"), VALUECHANGE(
      "4 VALUE CHANGE"), CONNECTIONREQUEST("6 CONNECTION REQUEST"), TRANSFER_COMPLETE(
          "7 TRANSFER COMPLETE"), AUTONOMOUS_TRANSFER_COMPLETE("10 AUTONOMOUS TRANSFER COMPLETE");

  private String notificationCode;

  private TR069InformType(String notificationCode) {
    this.notificationCode = notificationCode;
  }

  public String getNotificationCode() {
    return notificationCode;
  }

  public static TR069InformType getTR069NotificationType(String notificationCode) {
    TR069InformType result = null;
    for (TR069InformType notificationTypeEnum : TR069InformType.values()) {
      if (notificationTypeEnum.notificationCode.equals(notificationCode)) {
        result = notificationTypeEnum;
        break;
      }
    }
    return result;
  }
}
