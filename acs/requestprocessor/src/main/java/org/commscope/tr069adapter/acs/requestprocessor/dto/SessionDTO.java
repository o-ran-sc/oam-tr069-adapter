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

package org.commscope.tr069adapter.acs.requestprocessor.dto;

import java.io.Serializable;
import java.util.Date;

public class SessionDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String deviceId;

  private String sessionId;

  private Long currentOperationId;

  private SessionState sessionState;

  private Date sessionStartTime;

  /**
   * 
   */
  public SessionDTO() {}

  /**
   * @param deviceId
   * @param sessionId
   * @param currentOperationId
   */
  public SessionDTO(String deviceId, String sessionId, Long currentOperationId) {
    this.deviceId = deviceId;
    this.sessionId = sessionId;
    this.currentOperationId = currentOperationId;
  }

  public String getDeviceId() {
    return deviceId;
  }

  public void setDeviceId(String deviceId) {
    this.deviceId = deviceId;
  }

  public String getSessionId() {
    return sessionId;
  }

  public void setSessionId(String sessionId) {
    this.sessionId = sessionId;
  }

  public Long getCurrentOperationId() {
    return currentOperationId;
  }

  public void setCurrentOperationId(Long currentOperationId) {
    this.currentOperationId = currentOperationId;
  }

  public SessionState getSessionState() {
    return sessionState;
  }

  public void setSessionState(SessionState sessionState) {
    this.sessionState = sessionState;
  }

  public Date getSessionStartTime() {
    return sessionStartTime;
  }

  public void setSessionStartTime(Date sessionStartTime) {
    this.sessionStartTime = sessionStartTime;
  }
}
