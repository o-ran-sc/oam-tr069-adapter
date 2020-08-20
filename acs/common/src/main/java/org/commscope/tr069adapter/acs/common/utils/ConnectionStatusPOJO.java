package org.commscope.tr069adapter.acs.common.utils;

import java.util.Date;

public class ConnectionStatusPOJO {

  private boolean status;
  private Date lastContactTime;
  private Date lastFailedAttemptTime;
  private String errorMessage;

  public boolean isStatus() {
    return status;
  }

  public void setStatus(boolean status) {
    this.status = status;
  }

  public Date getLastContactTime() {
    return lastContactTime;
  }

  public void setLastContactTime(Date lastContactTime) {
    this.lastContactTime = lastContactTime;
  }

  public Date getLastFailedAttemptTime() {
    return lastFailedAttemptTime;
  }

  public void setLastFailedAttemptTime(Date lastFailedAttemptTime) {
    this.lastFailedAttemptTime = lastFailedAttemptTime;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  @Override
  public String toString() {
    return "ConnectionStatusPOJO [status=" + status + ", lastContactTime=" + lastContactTime
        + ", lastFailedAttemptTime=" + lastFailedAttemptTime + "]";
  }

}
