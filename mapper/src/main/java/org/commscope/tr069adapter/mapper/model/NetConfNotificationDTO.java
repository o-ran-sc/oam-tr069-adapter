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

package org.commscope.tr069adapter.mapper.model;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

import java.io.Serializable;
import java.util.List;

import org.commscope.tr069adapter.acs.common.ParameterDTO;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
public class NetConfNotificationDTO implements Serializable {

  private static final long serialVersionUID = 4928942484595767042L;
  private boolean isCustomNotification;
  private String notificationType;
  private List<ParameterDTO> parameters;
  private String uri;
  private String deviceID;

  public NetConfNotificationDTO() {
    super();
  }

  public NetConfNotificationDTO(String deviceID, String notificationType,
      boolean isCustomNotification) {
    super();
    this.isCustomNotification = isCustomNotification;
    this.notificationType = notificationType;
    this.deviceID = deviceID;
  }

  public String getDeviceID() {
    return deviceID;
  }

  public void setDeviceID(String deviceID) {
    this.deviceID = deviceID;
  }

  public String getNotificationType() {
    return notificationType;
  }

  public void setNotificationType(String notificationType) {
    this.notificationType = notificationType;
  }

  public List<ParameterDTO> getParameters() {
    return parameters;
  }

  public void setParameters(List<ParameterDTO> parameters) {
    this.parameters = parameters;
  }

  public String getUri() {
    return uri;
  }

  public void setUri(String uri) {
    this.uri = uri;
  }

  public boolean isCustomNotification() {
    return isCustomNotification;
  }

  public void setCustomNotification(boolean isCustomNotification) {
    this.isCustomNotification = isCustomNotification;
  }

}
