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


package org.commscope.tr069adapter.config.model;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

@Entity
@Table(name = "config_file_content",
    uniqueConstraints = @UniqueConstraint(columnNames = {"MACID", "SW_VERSION", "HW_VERSION"}))
public class ConfigFileContent implements Serializable {
  private static final long serialVersionUID = -5435735270835950132L;

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  @Column(name = "ID")
  private Long id;

  @Column(name = "MACID", length = 255)
  private String macId;

  @Column(name = "FILE_CONTENT", columnDefinition = "MEDIUMTEXT")
  private String fileContent;

  @Column(name = "SW_VERSION", length = 64)
  private String swVersion;

  @Column(name = "HW_VERSION", length = 64)
  private String hwVersion;

  public ConfigFileContent() {
    super();
  }

  public String getMacId() {
    return macId;
  }

  public void setMacId(String macId) {
    this.macId = macId;
  }

  public String getFileContent() {
    return fileContent;
  }

  public void setFileContent(String fileContent) {
    this.fileContent = fileContent;
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getSwVersion() {
    return swVersion;
  }

  public void setSwVersion(String swVersion) {
    this.swVersion = swVersion;
  }

  public String getHwVersion() {
    return hwVersion;
  }

  public void setHwVersion(String hwVersion) {
    this.hwVersion = hwVersion;
  }
}
