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

public class Upload extends TR069RPC {

  private static final long serialVersionUID = -3096728959585163928L;

  /** Creates a new instance of Upload */
  public Upload() {
    name = "Upload";
    username = "";
    password = "";
    delaySeconds = 0;
    fileType = FT_CONFIG;
    url = "http://localhost:8080/acs-war/upload/tst.cfg";
    commandKey = "default.command.key";
  }

  protected void createBody(SOAPBodyElement body, SOAPFactory spf) throws SOAPException {
    body.addChildElement(COMMAND_KEY).setValue(commandKey);
    body.addChildElement("FileType").setValue(fileType);

    body.addChildElement("URL").setValue(url);
    body.addChildElement("Username").setValue(username);
    body.addChildElement("Password").setValue(password);
    body.addChildElement("DelaySeconds").setValue(String.valueOf(delaySeconds));
  }

  protected void parseBody(SOAPBodyElement body, SOAPFactory f) throws SOAPException {
    logger.isDebugEnabled();
  }

  private String commandKey;
  private String fileType;
  private String url;
  private String username;
  private String password;
  private int delaySeconds;
  public static final String FT_CONFIG = "1 Vendor Configuration File";
  public static final String FT_LOG = "2 Vendor Log File";
}
