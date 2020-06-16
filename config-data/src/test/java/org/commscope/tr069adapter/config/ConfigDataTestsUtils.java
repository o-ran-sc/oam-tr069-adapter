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

package org.commscope.tr069adapter.config;

import org.commscope.tr069adapter.config.model.ConfigFileContent;

public class ConfigDataTestsUtils {

  public static final String SERVER_URI = "http://localhost:9000/";
  public static final String macId = "testMacId";
  public static final String OUI = "0005B9";
  public static final String PRODUCT_CLASS = "LTE_Enterprise_C-RANSC_Cntrl";
  public static final String HAEDWARE_VERSION = "1.1.1.1";
  public static final String SW_VERSION = "4.5.00.001";

  public static final String CONFIG_FILE_NAME = macId + ".xml";

  // public static ConfigurationData getConfigurationData() {
  // ConfigurationData configData = new ConfigurationData();
  //
  // configData.setHardwareVersion(HAEDWARE_VERSION);
  // configData.setSoftwareVersion(SW_VERSION);
  // configData.setLocalDn(macId);
  // configData.setOUI(OUI);
  // configData.setProductClass(PRODUCT_CLASS);
  //
  //// Map<String, String> map = new HashMap<String, String>();
  //// map.put(", value)
  //// configData.set
  // return configData;
  // }


  public static ConfigFileContent getConfigFileContent() {
    ConfigFileContent configFileContent = new ConfigFileContent();

    configFileContent.setFileContent(getFileContent());
    configFileContent.setMacId(macId);

    return configFileContent;
  }


  public static String getFileContent() {
    String fileContent = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n" + "<configDataFile>\r\n"
        + "  <fileHeader fileFormatVersion=\"32.594 V14.0.0\" vendorName=\"Commscope\"/>\r\n"
        + "  <configData>\r\n"
        + "    <managedElement swVersion=\"4.3.00.038\" localDn=\"0005B95196D0\" hwVersion=\"750742.00.04\" ProductClass=\"LTE_Enterprise_C-RANSC_Cntrl\" OUI=\"0005B9\"/>\r\n"
        + "    <Device>\r\n" + "      <FAP>\r\n" + "        <GPS>\r\n"
        + "          <AGPSServerConfig>\r\n" + "            <Enable>1</Enable>\r\n"
        + "            <Password>dmsuser</Password>\r\n"
        + "            <ServerPort>7001</ServerPort>\r\n"
        + "            <ServerURL>NONE</ServerURL>\r\n"
        + "            <Username>dmsuser</Username>\r\n" + "          </AGPSServerConfig>\r\n"
        + "          <GPSReset>0</GPSReset>\r\n" + "        </GPS>\r\n" + "		</FAP>\r\n"
        + "    </Device>\r\n" + "  </configData>\r\n"
        + "  <fileFooter dateTime=\"2019-07-16T17:32:35+05:30\"/>\r\n" + "</configDataFile>";

    return fileContent;
  }
}
