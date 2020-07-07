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

package com.commscope.tr069adapter.config;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.commscope.tr069adapter.acs.common.dto.ConfigurationData;
import org.commscope.tr069adapter.config.model.ConfigFileContent;
import org.springframework.web.client.RestTemplate;

public class ConfigDataApplicationTests {

  public static final String SERVER_URI = "http://localhost:9000/";
  public static final String macId = "mac2";

  public static void main(String args[]) {

    testSaveConfigurationFileContent();
    testGetConfigurationData();
    System.out.println("SUCCESSFUL");
  }

  private static void testSaveConfigurationFileContent() {
    System.out.println("\n*******Strating test testSaveConfigurationFileContent*******\n");
    RestTemplate restTemplate = new RestTemplate();

    String fileContent = readFile("ss.xml");
    ConfigFileContent configFleContent = new ConfigFileContent();

    configFleContent.setMacId(macId);
    configFleContent.setFileContent(fileContent);

    String response =
        restTemplate.postForObject(SERVER_URI + "/create", configFleContent, String.class);
    System.out.println("Saved");
    // printEmpData(response);
    System.out.println("\n*******Test testGetConfigurationData completed*******\n");
  }

  private static void testGetConfigurationData() {
    System.out.println("\n*******Strating test testSaveConfigurationFileContent*******\n");
    RestTemplate restTemplate = new RestTemplate();

    ConfigurationData configData =
        restTemplate.getForObject(SERVER_URI + "/getConfig/" + macId, ConfigurationData.class);

    if (null == configData) {
      System.out.println("No configuration data exist for device " + macId);
    } else {
      System.out.println(
          "\n***************Configuration data for device " + macId + "***************\n\n");
      System.out.println(configData.getParameterMONameValueMap());
    }
    // printConfigData(configData);
    System.out.println("\n*******Test testGetConfigurationData completed*******\n");
  }

  public static String readFile(String completeFilePath) {
    BufferedReader reader = null;
    String line = null;
    StringBuilder stringBuilder = new StringBuilder();
    String ls = "\n";

    try {
      reader = new BufferedReader(new FileReader(completeFilePath));

      while ((line = reader.readLine()) != null) {
        stringBuilder.append(line);
        stringBuilder.append(ls);
      }

      return stringBuilder.toString();
    } catch (Exception ex) {
      return null;
    } finally {
      if (null != reader) {
        try {
          reader.close();
        } catch (IOException e) {
        }
      }
    }
  }
}
