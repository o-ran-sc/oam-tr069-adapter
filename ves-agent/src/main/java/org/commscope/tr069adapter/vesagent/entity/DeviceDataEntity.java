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

package org.commscope.tr069adapter.vesagent.entity;

import com.google.gson.Gson;

import java.util.Date;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

@Entity
@Table(name = "VES_DEVICE_DATA",
    uniqueConstraints = @UniqueConstraint(columnNames = {"DEVICE_ID", "ATTR_GROUP"}))
public class DeviceDataEntity {

  @Id
  @Column(name = "ID")
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  @Column(name = "DEVICE_ID", length = 30)
  private String deviceId;

  @Column(name = "ENODEB_NAME", length = 100)
  private String eNodeBName;

  @Column(name = "OUI", length = 30)
  private String oui;

  @Column(name = "PRODUCT_CLASS", length = 100)
  private String productClass;

  @Column(name = "ATTR_JSON", length = 4000)
  private String attrJson;

  @Column(name = "ATTR_GROUP", length = 255)
  private String attrGroup;

  @Column(name = "LAST_UPDATED_TIME")
  private Date lastUpdateTime = new Date();

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getDeviceId() {
    return deviceId;
  }

  public void setDeviceId(String deviceId) {
    this.deviceId = deviceId;
  }

  public String geteNodeBName() {
    return eNodeBName;
  }

  public void seteNodeBName(String eNodeBName) {
    this.eNodeBName = eNodeBName;
  }

  public String getOui() {
    return oui;
  }

  public void setOui(String oui) {
    this.oui = oui;
  }

  public String getProductClass() {
    return productClass;
  }

  public void setProductClass(String productClass) {
    this.productClass = productClass;
  }

  public String getAttrJson() {
    return attrJson;
  }

  public void setAttrJson(String attrJson) {
    this.attrJson = attrJson;
  }

  public String getAttrGroup() {
    return attrGroup;
  }

  public void setAttrGroup(String attrGroup) {
    this.attrGroup = attrGroup;
  }

  public Date getLastUpdateTime() {
    return lastUpdateTime;
  }

  public void setLastUpdateTime(Date lastUpdateTime) {
    this.lastUpdateTime = lastUpdateTime;
  }
  
  @Transient
  private Long startEpochMicrosec;

  public Long getStartEpochMicrosec() {
    return startEpochMicrosec;
  }

  public void setStartEpochMicrosec(Long startEpochMicrosec) {
    this.startEpochMicrosec = startEpochMicrosec;
  }

  @Transient
  public void setAttributesMap(Map<String, String> attributesMap) {
    if (null == attributesMap || attributesMap.isEmpty()) {
      return;
    }

    this.attrJson = new Gson().toJson(attributesMap);

  }

  @Transient
  public Map<String, String> getAttributesMap() {
    Map<String, String> attributesMap = null;
    if (null != this.attrJson && !this.attrJson.isEmpty()) {
      attributesMap = new Gson().fromJson(this.attrJson, Map.class);
    }
    return attributesMap;
  }
}
