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

package org.commscope.tr069adapter.acs.requestprocessor.entity;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

@Entity
@Table(name = "TR069_SESSION", uniqueConstraints = @UniqueConstraint(columnNames = {"DEVICE_ID"}))
public class SessionManagerEntity implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  @Column(name = "DEVICE_ID", length = 30)
  private String deviceId;

  @Column(name = "SESSION_ID", length = 64)
  private String sessionId;

  @Column(name = "CURRENT_OPERATION_ID", length = 30)
  private Long currentOperationId;

  @Column(name = "STATE", length = 2)
  private Integer state;

  @Column(name = "SESSION_START_TIME")
  private Date sessionStartTime;

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

  public Integer getState() {
    return state;
  }

  public void setState(Integer state) {
    this.state = state;
  }

  public Date getSessionStartTime() {
    return sessionStartTime;
  }

  public void setSessionStartTime(Date sessionStartTime) {
    this.sessionStartTime = sessionStartTime;
  }
}
