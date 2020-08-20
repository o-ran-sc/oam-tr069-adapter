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


package org.commscope.tr069adapter.config.controllers;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.commscope.tr069adapter.acs.common.dto.ConfigurationData;
import org.commscope.tr069adapter.config.constants.ConfigurationServiceConstant;
import org.commscope.tr069adapter.config.exceptions.InvalidConfigurationServiceException;
import org.commscope.tr069adapter.config.service.ConfigurationDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@RestController
public class ConfugurationDataController {
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  @Autowired
  ConfigurationDataService configDataService;

  @GetMapping("/isActive")
  public String getMessage() {
    return "Application is running";
  }

  @PostMapping("/getConfig")
  public ConfigurationData viewConfigurationData(@RequestParam String macId,
      @RequestParam String swVersion, @RequestParam String hwVersion)
      throws InvalidConfigurationServiceException {
    macId = macId.replaceAll("[\n|\r|\t]", "_");
    logger.info("Processing request to get configuration data for device {}", macId);
    Optional<ConfigurationData> configData =
        configDataService.getConfigurationData(macId, swVersion, hwVersion);
    if (configData.isPresent()) {
      return configData.get();
    }
    logger.info("Configuration data doesn't exist for device {}", macId);
    return null;
  }

  @PostMapping("/importConfig")
  public String uploadMultipleFiles(@RequestParam("files") MultipartFile[] files)
      throws InvalidConfigurationServiceException {

    if (null == files || files.length == 0) {
      logger.info("No file given for import");
      return "No file given for import";
    }

    List<MultipartFile> fileList = Arrays.asList(files);

    MultipartFile file = fileList.get(0);
    String fileName = StringUtils.cleanPath(file.getOriginalFilename());

    logger.debug("Importing file {}", fileName);
    if (!fileName.endsWith(ConfigurationServiceConstant.CONFIG_FILE_EXTENSION)) {
      throw new InvalidConfigurationServiceException("Unsupported file format for file " + fileName
          + ". Only XML file is supported. Ignoring file import for file " + fileName);
    }

    try {
      configDataService.saveConfigFileContents(file);
    } catch (InvalidConfigurationServiceException ex) {
      throw new InvalidConfigurationServiceException(
          "Error occurred while import file " + fileName + ". Cause : " + ex.getMessage());
    } catch (Exception ex) {
      throw new InvalidConfigurationServiceException(
          "UNKNOWN error occurred while import file " + fileName + ". Cause : " + ex.getMessage());
    }

    return "File " + fileName + " imported successfully";
  }

}
