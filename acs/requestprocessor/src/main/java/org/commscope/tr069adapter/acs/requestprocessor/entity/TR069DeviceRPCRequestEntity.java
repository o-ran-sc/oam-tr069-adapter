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

@Entity
@Table(name = "TR069_NBI_REQUEST")
public class TR069DeviceRPCRequestEntity implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  @Column(name = "DEVICE_ID", length = 30)
  private String deviceId;

  @Column(name = "OPERATION_ID", length = 30)
  private Long operationId;

  @Column(name = "OPERATION_CODE", length = 3)
  private Integer opCode;

  @Column(name = "ATTRIBUTE_JSON1", length = 4000)
  private String attributeJson1;

  @Column(name = "ATTRIBUTE_JSON2", length = 4000)
  private String attributeJson2;

  @Column(name = "ATTRIBUTE_JSON3", length = 4000)
  private String attributeJson3;

  @Column(name = "REQUEST_TIME_OUT", length = 5)
  private Long requestTimeOut;

  @Column(name = "CREATE_TIME")
  private Date createTime;

  @Column(name = "IS_PROCESSED", length = 2)
  private Integer isProcessed;

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

  public Long getOperationId() {
    return operationId;
  }

  public void setOperationId(Long operationId) {
    this.operationId = operationId;
  }

  public Integer getOpCode() {
    return opCode;
  }

  public void setOpCode(Integer opCode) {
    this.opCode = opCode;
  }

  public String getAttributeJson1() {
    return attributeJson1;
  }

  public void setAttributeJson1(String attributeJson1) {
    this.attributeJson1 = attributeJson1;
  }

  public String getAttributeJson2() {
    return attributeJson2;
  }

  public void setAttributeJson2(String attributeJson2) {
    this.attributeJson2 = attributeJson2;
  }

  public String getAttributeJson3() {
    return attributeJson3;
  }

  public void setAttributeJson3(String attributeJson3) {
    this.attributeJson3 = attributeJson3;
  }

  public Long getRequestTimeOut() {
    return requestTimeOut;
  }

  public void setRequestTimeOut(Long requestTimeOut) {
    this.requestTimeOut = requestTimeOut;
  }

  public Date getCreateTime() {
    return createTime;
  }

  public void setCreateTime(Date createTime) {
    this.createTime = createTime;
  }

  public Integer getIsProcessed() {
    return isProcessed;
  }

  public void setIsProcessed(Integer isProcessed) {
    this.isProcessed = isProcessed;
  }

}
