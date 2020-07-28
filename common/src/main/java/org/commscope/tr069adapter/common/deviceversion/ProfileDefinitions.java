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
import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({"profileDefinition"})
public class ProfileDefinitions {
  @JsonProperty("profileDefinition")
  private List<ProfileDefinition> profileDefinition = null;

  /**
   * No args constructor for use in serialization
   *
   */
  public ProfileDefinitions() {}

  /**
   *
   * @param profileDefinition
   */
  public ProfileDefinitions(List<ProfileDefinition> profileDefinition) {
    super();
    this.profileDefinition = profileDefinition;
  }

  @JsonProperty("profileDefinition")
  public List<ProfileDefinition> getProfileDefinition() {
    return profileDefinition;
  }

  @JsonProperty("profileDefinition")
  public void setProfileDefinition(List<ProfileDefinition> profileDefinition) {
    this.profileDefinition = profileDefinition;
  }

}
