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

package org.commscope.tr069adapter.netconf.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "config")
public class NetConfServerProperties {

  private Integer defaultNetconfStartPort = 17830;
  private Integer defaultMaxServers = 200;

  private String requestTimeOut;
  private String mapperPath;
  private String netconfServerRestPort;
  private String netconfServersStartPort;
  private String netconfServersEndPort;
  private String maxNumOfNetconfServers;
  private String netconfServerIP;
  private String schemaDirPath;

  public String getRequestTimeOut() {
    return requestTimeOut;
  }

  public void setRequestTimeOut(String requestTimeOut) {
    this.requestTimeOut = requestTimeOut;
  }

  public String getMapperPath() {
    return mapperPath;
  }

  public void setMapperPath(String mapperPath) {
    this.mapperPath = mapperPath;
  }

  public String getNetconfServerRestPort() {
    return netconfServerRestPort;
  }

  public void setNetconfServerRestPort(String netconfServerRestPort) {
    this.netconfServerRestPort = netconfServerRestPort;
  }

  public String getMaxNumOfNetconfServers() {
    return maxNumOfNetconfServers;
  }

  public void setMaxNumOfNetconfServers(String maxNumOfNetconfServers) {
    this.maxNumOfNetconfServers = maxNumOfNetconfServers;
  }

  public String getNetconfServersStartPort() {
    return netconfServersStartPort;
  }

  public void setNetconfServersStartPort(String netconfServersStartPort) {
    this.netconfServersStartPort = netconfServersStartPort;
  }

  public String getNetconfServersEndPort() {
    return netconfServersEndPort;
  }

  public void setNetconfServersEndPort(String netconfServersEndPort) {
    this.netconfServersEndPort = netconfServersEndPort;
  }

  public Integer getDefaultNetconfStartPort() {
    return defaultNetconfStartPort;
  }

  public void setDefaultNetconfStartPort(Integer defaultPort) {
    defaultNetconfStartPort = defaultPort;
  }

  public Integer getDefaultMaxServers() {
    return defaultMaxServers;
  }

  public void setDefaultMaxServers(Integer maxServers) {
    defaultMaxServers = maxServers;
  }

  public String getNetconfServerIP() {
    return netconfServerIP;
  }

  public void setNetconfServerIP(String netconfServerIP) {
    this.netconfServerIP = netconfServerIP;
  }

  public String getSchemaDirPath() {
    return schemaDirPath;
  }

  public void setSchemaDirPath(String schemaDirPath) {
    this.schemaDirPath = schemaDirPath;
  }

}
