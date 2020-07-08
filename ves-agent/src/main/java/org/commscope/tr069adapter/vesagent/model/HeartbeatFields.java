package org.commscope.tr069adapter.vesagent.model;

import com.fasterxml.jackson.annotation.JsonInclude;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class HeartbeatFields {
  public String heartbeatFieldsVersion;
  public int heartbeatInterval;

  public String getHeartbeatFieldsVersion() {
    return heartbeatFieldsVersion;
  }

  public void setHeartbeatFieldsVersion(String heartbeatFieldsVersion) {
    this.heartbeatFieldsVersion = heartbeatFieldsVersion;
  }

  public int getHeartbeatInterval() {
    return heartbeatInterval;
  }

  public void setHeartbeatInterval(int heartbeatInterval) {
    this.heartbeatInterval = heartbeatInterval;
  }
}
