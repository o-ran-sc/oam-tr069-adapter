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

package org.commscope.tr069adapter.acs.cpe.handler;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.commscope.tr069adapter.acs.common.dto.DeviceData;
import org.commscope.tr069adapter.acs.cpe.rpc.Inform;
import org.commscope.tr069adapter.acs.cpe.utils.FactorySrvcDependencyConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class DeviceValidator {

  private static final Logger logger = LoggerFactory.getLogger(DeviceValidator.class);

  @Autowired
  FactorySrvcDependencyConfig factorySrvcDependencyConfig;

  @Autowired
  RestTemplate restTemplate;

  public void setFactorySrvcDependencyConfig(
      FactorySrvcDependencyConfig factorySrvcDependencyConfig) {
    this.factorySrvcDependencyConfig = factorySrvcDependencyConfig;
  }

  /**
   * @param inform
   * @param authorization
   * @return
   */
  public Boolean isDeviceAuthorized(Inform inform, String authorization) {
    if (authorization == null) {
      logger.debug("HTTP Challenge failed as Authorization header does not exist");
      return false;
    }

    Boolean isAuthorized = true;
    if (authorization.toLowerCase().startsWith("basic")) {
      isAuthorized = performBasicAuthentication(inform, authorization);
    } else if (authorization.toLowerCase().startsWith("digest")) {
      isAuthorized = performDigestAuthentication(inform, authorization);
    }

    return isAuthorized;
  }

  /**
   * @param deviceId
   * @param oui
   * @param pc
   * @return
   */
  public Boolean validateDevice(String deviceId, String oui, String pc) {
    if (oui == null || pc == null) {
      logger.error(
          "OUI or Product Class cannot be null, Device has not sent the OUI or Product class in the Inform!");
      return false;
    }

    Boolean isValid = true;
    try {
      if (factorySrvcDependencyConfig.getDeviceValidationURL() == null) {
        logger.debug(
            "Device Validation URL is not configured, hence not performing device validation against factory data");
        return isValid;
      }

      DeviceData deviceData = new DeviceData();
      deviceData.setSerialNumber(deviceId);
      deviceData.setOui(oui);
      deviceData.setProductClass(pc);
      ResponseEntity<Boolean> restResponse = restTemplate.postForEntity(
          factorySrvcDependencyConfig.getDeviceValidationURL(), deviceData, Boolean.class);
      isValid = restResponse.getBody();
      logger.debug("Is Device valid : {}", isValid);
    } catch (Exception e) {
      logger.error("An error occurred while validating the device with Factory data, Reason: {}",
          e.getMessage());
      isValid = false;
    }

    return isValid;
  }

  /**
   * @param inform
   * @param authorization
   * @return
   */
  private Boolean performBasicAuthentication(Inform inform, String authorization) {
    Boolean isAuthorized = false;
    // Authorization: Basic base64credentials
    String base64Credentials = authorization.substring("Basic".length()).trim();
    logger.debug("Authorizing by basic authentication");
    DeviceData deviceData = buildAuthorizationRequest(inform.getSn(), base64Credentials);

    logger.debug("Doing authentication from rest service: {}",
        factorySrvcDependencyConfig.getBasicAuthorizationURL());
    try {
      if (factorySrvcDependencyConfig.getBasicAuthorizationURL() == null) {
        logger.debug(
            "Device Basic Authentication URL is not configured, hence not performing device authentication against factory data");
        isAuthorized = true;
      } else {
        ResponseEntity<Boolean> restResponse = restTemplate.postForEntity(
            factorySrvcDependencyConfig.getBasicAuthorizationURL(), deviceData, Boolean.class);
        isAuthorized = restResponse.getBody();
        if (isAuthorized.booleanValue()) {
          logger.debug("Updating the username and password");
          byte[] credDecoded = Base64.getDecoder().decode(base64Credentials);
          String credentials = new String(credDecoded, StandardCharsets.UTF_8);
          // credentials = username:password
          final String[] values = credentials.split(":", 2);
          inform.getParams().put(inform.getRoot() + ".ManagementServer.ConnectionRequestUsername",
              values[0]);
          inform.getParams().put(inform.getRoot() + ".ManagementServer.ConnectionRequestPassword",
              values[1]);
        }
      }
    } catch (Exception e) {
      logger.error("Unable to authenticate the HTTP request, Reason: {}", e.getMessage());
    }

    return isAuthorized;
  }

  /**
   * @param inform
   * @param authorization
   * @return
   */
  private Boolean performDigestAuthentication(Inform inform, String authorization) {
    Boolean isAuthorized = false;
    // Authorization: Basic base64credentials
    String authenticationString = authorization.substring("Digest".length()).trim();
    logger.debug("Authorizing by digest authentication");
    DeviceData deviceData = buildAuthorizationRequest(inform.getSn(), authenticationString);

    logger.debug("Doing authentication from rest service: {}",
        factorySrvcDependencyConfig.getDigestAuthorizationURL());
    try {
      if (factorySrvcDependencyConfig.getDigestAuthorizationURL() == null) {
        logger.debug(
            "Device Digest Authentication URL is not configured, hence not performing device authentication against factory data");
        isAuthorized = true;
      } else {
        ResponseEntity<Boolean> restResponse = restTemplate.postForEntity(
            factorySrvcDependencyConfig.getDigestAuthorizationURL(), deviceData, Boolean.class);
        isAuthorized = restResponse.getBody();
      }
    } catch (Exception e) {
      logger.error("Unable to authenticate the HTTP request, Reason: {}", e.getMessage());
    }

    return isAuthorized;
  }

  /**
   * @param serialNumber
   * @param base64Credentials
   * @return
   */
  private DeviceData buildAuthorizationRequest(String serialNumber, String base64Credentials) {
    DeviceData deviceData = new DeviceData();
    deviceData.setSerialNumber(serialNumber);
    deviceData.setAutenticationString(base64Credentials);
    return deviceData;
  }
}
