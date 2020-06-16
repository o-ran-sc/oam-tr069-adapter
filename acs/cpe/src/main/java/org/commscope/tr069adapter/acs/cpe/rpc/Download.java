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

package org.commscope.tr069adapter.acs.cpe.rpc;

import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

import org.commscope.tr069adapter.acs.cpe.TR069RPC;

public class Download extends TR069RPC {

  private static final long serialVersionUID = 8989750644171761861L;

  /** Creates a new instance of Download */
  public Download() {
    name = "Download";
  }

  public Download(String commandKey, String url, String fileType) {
    name = "Download";
    this.commandKey = commandKey;
    this.url = url;
    this.fileType = fileType;
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    body.addChildElement(COMMAND_KEY).setValue(commandKey);
    body.addChildElement("FileType").setValue(fileType);
    body.addChildElement("URL").setValue(url);
    body.addChildElement("Username").setValue(userName);
    body.addChildElement("Password").setValue(password);
    body.addChildElement("FileSize").setValue(String.valueOf(fileSize));
    body.addChildElement("TargetFileName").setValue(targetFileName);
    body.addChildElement("DelaySeconds").setValue(String.valueOf(delaySeconds));
    body.addChildElement("SuccessURL").setValue(successUrl);
    body.addChildElement("FailureURL").setValue(failureUrl);
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException {
    body.getAllAttributes();
  }

  private String commandKey = "";
  private String fileType = "";
  private String url = "";
  private String userName = "";
  private String password = "";
  private long fileSize = 0;
  private String targetFileName = "";
  private int delaySeconds = 0;
  private String successUrl = "";
  private String failureUrl = "";
  public static final String FT_FIRMWARE = "1 Firmware Upgrade Image";
  public static final String FT_WEBCONTENT = "2 Web Content";
  public static final String FT_CONFIG = "3 Vendor Configuration File";

  public String getCommandKey() {
    return commandKey;
  }

  public void setCommandKey(String commandKey) {
    this.commandKey = commandKey;
  }

  public String getFileType() {
    return fileType;
  }

  public void setFileType(String fileType) {
    this.fileType = fileType;
  }

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(String userName) {
    this.userName = userName;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public long getFileSize() {
    return fileSize;
  }

  public void setFileSize(long fileSize) {
    this.fileSize = fileSize;
  }

  public String getTargetFileName() {
    return targetFileName;
  }

  public void setTargetFileName(String targetFileName) {
    this.targetFileName = targetFileName;
  }

  public int getDelaySeconds() {
    return delaySeconds;
  }

  public void setDelaySeconds(int delaySeconds) {
    this.delaySeconds = delaySeconds;
  }

  public String getSuccessUrl() {
    return successUrl;
  }

  public void setSuccessUrl(String successUrl) {
    this.successUrl = successUrl;
  }

  public String getFailureUrl() {
    return failureUrl;
  }

  public void setFailureUrl(String failureUrl) {
    this.failureUrl = failureUrl;
  }
}
