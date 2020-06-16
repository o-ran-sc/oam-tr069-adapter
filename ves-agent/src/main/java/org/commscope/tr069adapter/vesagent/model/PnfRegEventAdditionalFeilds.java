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
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class PnfRegEventAdditionalFeilds implements Serializable {
  /**
   * 
   */
  private static final long serialVersionUID = 5368863118681551950L;
  private String oamPort;
  private String protocol;
  private String username;
  private String password;
  private String reconnectOnChangedSchema;
  @JsonProperty("sleep-factor")
  private String sleepfactor;
  private String tcpOnly;
  private String connectionTimeout;
  private String maxConnectionAttempts;
  private String betweenAttemptsTimeout;
  private String keepaliveDelay;

  public String getOamPort() {
    return oamPort;
  }

  public void setOamPort(String oamPort) {
    this.oamPort = oamPort;
  }

  public String getProtocol() {
    return protocol;
  }

  public void setProtocol(String protocol) {
    this.protocol = protocol;
  }

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

  public String getReconnectOnChangedSchema() {
    return reconnectOnChangedSchema;
  }

  public void setReconnectOnChangedSchema(String reconnectOnChangedSchema) {
    this.reconnectOnChangedSchema = reconnectOnChangedSchema;
  }

  public String getSleepfactor() {
    return sleepfactor;
  }

  public void setSleepfactor(String sleepfactor) {
    this.sleepfactor = sleepfactor;
  }

  public String getTcpOnly() {
    return tcpOnly;
  }

  public void setTcpOnly(String tcpOnly) {
    this.tcpOnly = tcpOnly;
  }

  public String getConnectionTimeout() {
    return connectionTimeout;
  }

  public void setConnectionTimeout(String connectionTimeout) {
    this.connectionTimeout = connectionTimeout;
  }

  public String getMaxConnectionAttempts() {
    return maxConnectionAttempts;
  }

  public void setMaxConnectionAttempts(String maxConnectionAttempts) {
    this.maxConnectionAttempts = maxConnectionAttempts;
  }

  public String getBetweenAttemptsTimeout() {
    return betweenAttemptsTimeout;
  }

  public void setBetweenAttemptsTimeout(String betweenAttemptsTimeout) {
    this.betweenAttemptsTimeout = betweenAttemptsTimeout;
  }

  public String getKeepaliveDelay() {
    return keepaliveDelay;
  }

  public void setKeepaliveDelay(String keepaliveDelay) {
    this.keepaliveDelay = keepaliveDelay;
  }
}
