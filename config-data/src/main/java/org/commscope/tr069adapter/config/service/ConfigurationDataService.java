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

package org.commscope.tr069adapter.config.service;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

import org.commscope.tr069adapter.acs.common.dto.ConfigurationData;
import org.commscope.tr069adapter.config.constants.Utility;
import org.commscope.tr069adapter.config.exceptions.InvalidConfigurationServiceException;
import org.commscope.tr069adapter.config.model.ConfigFileContent;
import org.commscope.tr069adapter.config.parser.ConfigurationXMLDataParser;
import org.commscope.tr069adapter.config.repository.ConfigurationDataRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

@Service
public class ConfigurationDataService {
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  @Autowired
  ConfigurationDataRepository configDataRepository;

  @Autowired
  ConfigurationXMLDataParser configurationXMLDataParser;

  public void saveConfigFileContent(ConfigFileContent configFileContent) {
    configDataRepository.save(configFileContent);
  }

  public Iterable<ConfigFileContent> getAllConfigFileContent() {
    return configDataRepository.findAll();
  }

  public Optional<ConfigFileContent> getConfigFileContent(String macId) {
    return configDataRepository.findById(macId);
  }

  public Optional<ConfigurationData> getConfigurationData(String macId)
      throws InvalidConfigurationServiceException {
    ConfigurationData configurationData = null;
    Optional<ConfigFileContent> configFileContent = configDataRepository.findById(macId);

    if (configFileContent.isPresent()) {
      logger.debug("Parsing configuration file for device {}", macId);
      configurationData = configurationXMLDataParser.parseFile(configFileContent.get());
      logger.debug("Parsing of device configuration file is completed");
    } else {
      logger.error("Configuration file is not available for device {}", macId);
      return Optional.ofNullable(configurationData);
    }
    return Optional.ofNullable(configurationData);
  }

  public void saveConfigFileContents(MultipartFile file)
      throws InvalidConfigurationServiceException {
    String fileName = StringUtils.cleanPath(file.getOriginalFilename());

    if (fileName.contains("..")) {
      throw new InvalidConfigurationServiceException(
          "Filename contains invalid path sequence " + fileName);
    }

    ConfigFileContent configFileContent = new ConfigFileContent();

    try {
      configFileContent.setFileContent(new String(file.getBytes(), StandardCharsets.UTF_8));
      configFileContent.setMacId(Utility.getMacId(fileName));

      if (!configFileContent.getFileContent().contains("<configDataFile>")) {
        logger.error(
            "File {} is not a valid configuration file as it doesn't contain tag \"<configDataFile>\"",
            fileName);
        throw new InvalidConfigurationServiceException(
            "File is not a valid configuration file as it doesn't contain tag \"<configDataFile>\"");
      }

    } catch (Exception e) {
      throw new InvalidConfigurationServiceException(
          "Error occurred while reading file content. Reason: " + e.getMessage());
    }

    configurationXMLDataParser.validateFile(configFileContent);

    logger.debug("Saving configuration file {} content for device of macId {}", fileName,
        Utility.getMacId(fileName));
    saveConfigFileContent(configFileContent);
    logger.debug("Configuration file content saved successfully");
  }

}
