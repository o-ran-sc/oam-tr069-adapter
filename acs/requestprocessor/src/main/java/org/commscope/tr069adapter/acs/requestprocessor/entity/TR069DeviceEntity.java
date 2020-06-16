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
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

/**
 * The persistent class for the TR069_DEVICE_ database table.
 * 
 */
@Entity
@Table(name = "TR069_DEVICE", uniqueConstraints = @UniqueConstraint(columnNames = {"DEVICE_ID"}))
public class TR069DeviceEntity implements Serializable {

  private static final long serialVersionUID = -2696584252283707720L;

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  @Column(name = "ID")
  private Long id;

  @Column(name = "DEVICE_ID", length = 30)
  private String deviceId;

  @Column(name = "USER_NAME", length = 256)
  private String userName;

  @Column(name = "PASSWORD", length = 256)
  private String password;

  @Column(name = "HW_VERSION", length = 20)
  private String hwVersion;

  @Column(name = "SW_VERSION", length = 20)
  private String swVersion;

  @Column(name = "CONNECTION_REQUEST_URL", length = 1024)
  private String connectionReqURL;

  @Column(name = "LAST_UPDATED_TIME")
  private Date lastUpdatedTime;

  public TR069DeviceEntity() {
    super();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getDeviceId() {
    return deviceId;
  }

  public void setDeviceId(String deviceID) {
    this.deviceId = deviceID;
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(String userName) {
    this.userName = userName;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public String getHwVersion() {
    return hwVersion;
  }

  public void setHwVersion(String hwVersion) {
    this.hwVersion = hwVersion;
  }

  public String getSwVersion() {
    return swVersion;
  }

  public void setSwVersion(String swVersion) {
    this.swVersion = swVersion;
  }

  public String getConnectionReqURL() {
    return connectionReqURL;
  }

  public void setConnectionReqURL(String connectionReqURL) {
    this.connectionReqURL = connectionReqURL;
  }

  public Date getLastUpdatedTime() {
    return lastUpdatedTime;
  }

  public void setLastUpdatedTime(Date rowUpdatedTime) {
    this.lastUpdatedTime = rowUpdatedTime;
  }

}
