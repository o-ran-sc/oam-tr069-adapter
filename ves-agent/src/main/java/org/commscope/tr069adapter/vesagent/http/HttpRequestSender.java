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

package org.commscope.tr069adapter.vesagent.http;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.commscope.tr069adapter.mapper.model.VESNotificationResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

@Component
public class HttpRequestSender {

  private HttpRequestSender() {}

  private static final String CONTENT = "Content-Type";
  private static final String ACCEPT = "Accept";
  private static final String JSON = "application/json";
  private static final Logger logger = LoggerFactory.getLogger(HttpRequestSender.class);

  public VESNotificationResponse postRequest(String requestUrl, String requestBody) {
    logger.debug("Event lister URL : {}", requestUrl);
    logger.debug("Event Data : {}", requestBody);
    String response = "";
    int responseCode = 0;
    HttpURLConnection connection = null;
    BufferedReader br = null;
    try {
      URL url = new URL(requestUrl);
      connection = (HttpURLConnection) url.openConnection();
      connection.setDoOutput(true);
      connection.setDoInput(true);
      connection.setRequestProperty(CONTENT, JSON);
      connection.setRequestProperty(ACCEPT, JSON);
      connection.setRequestMethod("POST");
      OutputStreamWriter writer =
          new OutputStreamWriter(connection.getOutputStream(), StandardCharsets.UTF_8);
      writer.write(requestBody);
      writer.close();
      br = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      String temp = null;
      responseCode = connection.getResponseCode();
      logger.debug("Response received from ves collector : {}", responseCode);
      while ((temp = br.readLine()) != null) {
        response = response.concat(temp);
      }

      if (response == null || response.length() <= 0) {
        response = String.valueOf(responseCode);
      }

    } catch (Exception e) {
      logger.error("Exception occurred while posting the message {}", e.getMessage());
      response = "posting message failed";
      return new VESNotificationResponse(HttpStatus.INTERNAL_SERVER_ERROR.value(), response);
    } finally {
      try {
        if (br != null)
          br.close();
        if (connection != null)
          connection.disconnect();
      } catch (Exception exToIgnore) {
        logger.debug("Exception occurred while closing the connection {}", exToIgnore.toString());
      }
    }
    return new VESNotificationResponse(HttpStatus.valueOf(responseCode).value(), response);
  }
}
