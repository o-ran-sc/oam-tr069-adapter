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

package org.commscope.tr069adapter.vesagent.model;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.io.Serializable;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class HeartbeatFields implements Serializable {

  private static final long serialVersionUID = -4654513718975538805L;

  private String heartbeatFieldsVersion;
  private int heartbeatInterval;

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
