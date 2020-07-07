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
