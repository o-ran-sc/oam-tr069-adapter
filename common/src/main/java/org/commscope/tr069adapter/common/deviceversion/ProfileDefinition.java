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

package org.commscope.tr069adapter.common.deviceversion;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({"profile-id", "software-version", "hardware-version", "netconf-schema-dir-path",
    "csdm-mapping-dir-path", "extends-from"})
public class ProfileDefinition {
  @JsonProperty("profile-id")
  private String profileId;
  @JsonProperty("software-version")
  private String softwareVersion;
  @JsonProperty("hardware-version")
  private String hardwareVersion;
  @JsonProperty("netconf-schema-dir-path")
  private String netConfSchemaPath;
  @JsonProperty("csdm-mapping-dir-path")
  private String csdmMappingPath;
  @JsonProperty("extends-from")
  private String parent;

  public ProfileDefinition() {}

  public ProfileDefinition(String profileId, String software, String hardware, String schemaPath,
      String mappingFile, String parent) {
    super();
    this.profileId = profileId;
    this.softwareVersion = software;
    this.hardwareVersion = hardware;
    this.netConfSchemaPath = schemaPath;
    this.csdmMappingPath = mappingFile;
    this.parent = parent;
  }

  public String getProfileId() {
    return profileId;
  }

  public void setProfileId(String profileId) {
    this.profileId = profileId;
  }

  public String getSoftwareVersion() {
    return softwareVersion;
  }

  public void setSoftwareVersion(String softwareVersion) {
    this.softwareVersion = softwareVersion;
  }

  public String getHardwareVersion() {
    return hardwareVersion;
  }

  public void setHardwareVersion(String hardwareVersion) {
    this.hardwareVersion = hardwareVersion;
  }

  public String getNetConfSchemaPath() {
    return netConfSchemaPath;
  }

  public void setNetConfSchemaPath(String netConfSchemaPath) {
    this.netConfSchemaPath = netConfSchemaPath;
  }

  public String getCsdmMappingPath() {
    return csdmMappingPath;
  }

  public void setCsdmMappingPath(String csdmMappingPath) {
    this.csdmMappingPath = csdmMappingPath;
  }

  public String getParent() {
    return parent;
  }

  public void setParent(String parent) {
    this.parent = parent;
  }

}
