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

package org.commscope.tr069adapter.acs.common.dto;

import org.commscope.tr069adapter.acs.common.DeviceDetails;

public class TR069DeviceDetails extends DeviceDetails {

  private static final long serialVersionUID = 8067939584181214835L;

  // Device credentials
  private String username;
  private String password;

  // Device connection request URL
  private String connectionRequestURL;

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public String getConnectionRequestURL() {
    return connectionRequestURL;
  }

  public void setConnectionRequestURL(String connectionRequestURL) {
    this.connectionRequestURL = connectionRequestURL;
  }

}
