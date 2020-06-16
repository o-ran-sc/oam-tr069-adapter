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

package org.commscope.tr069adapter.acs.cpe.utils;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "config")
public class FactorySrvcDependencyConfig {

  private String basicAuthorizationURL;

  private String digestAuthorizationURL;

  private String deviceValidationURL;

  public String getBasicAuthorizationURL() {
    return basicAuthorizationURL;
  }

  public void setBasicAuthorizationURL(String basicAuthorizationURL) {
    this.basicAuthorizationURL = basicAuthorizationURL;
  }

  public String getDigestAuthorizationURL() {
    return digestAuthorizationURL;
  }

  public void setDigestAuthorizationURL(String digestAuthorizationURL) {
    this.digestAuthorizationURL = digestAuthorizationURL;
  }

  public String getDeviceValidationURL() {
    return deviceValidationURL;
  }

  public void setDeviceValidationURL(String deviceValidationURL) {
    this.deviceValidationURL = deviceValidationURL;
  }

}
