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
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.TreeMap;
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

  public Optional<ConfigurationData> getConfigurationData(String macId, String swVersion,
      String hwVersion) throws InvalidConfigurationServiceException {
    ConfigurationData configurationData = null;
    List<ConfigFileContent> configFileContentList = configDataRepository.findByMacId(macId);
    TreeMap<DeviceVersion, ConfigurationData> configDataMap = new TreeMap<>();
    if (!configFileContentList.isEmpty()) {
      logger.debug("Parsing configuration file for device {}", macId);
      for (ConfigFileContent configFileContent : configFileContentList) {
        ConfigurationData cfgData = configurationXMLDataParser.parseFile(configFileContent);
        DeviceVersion dVersion =
            new DeviceVersion(cfgData.getSoftwareVersion(), cfgData.getHardwareVersion());
        configDataMap.put(dVersion, cfgData);
      }
      DeviceVersion inputVersion = new DeviceVersion(swVersion, hwVersion);
      Entry<DeviceVersion, ConfigurationData> floorEntry = configDataMap.floorEntry(inputVersion);

      if (null == floorEntry) {
        logger.error("Configuration file is not available for device {}", macId);
        return Optional.ofNullable(configurationData);
      }

      DeviceVersion floor = floorEntry.getKey();
      configurationData = configDataMap.get(floor);
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

    ConfigurationData configurationData = configurationXMLDataParser.parseFile(configFileContent);
    configFileContent.setSwVersion(configurationData.getSoftwareVersion());
    configFileContent.setHwVersion(configurationData.getHardwareVersion());

    logger.debug("Saving configuration file {} content for device of macId {}", fileName,
        Utility.getMacId(fileName));
    ConfigFileContent configFileContentEntity =
        configDataRepository.findByMacIdAndSwVersionAndHwVersion(configFileContent.getMacId(),
            configFileContent.getSwVersion(), configFileContent.getHwVersion());

    if (configFileContentEntity != null) {
      configFileContentEntity.setFileContent(configFileContent.getFileContent());
      saveConfigFileContent(configFileContentEntity);
    } else {
      saveConfigFileContent(configFileContent);
    }

    logger.debug("Configuration file content saved successfully");
  }

  class DeviceVersion implements Comparable<DeviceVersion> {
    private static final long serialVersionUID = -7251276716604249440L;
    private int svMajorVersion = 0;
    private int svMinorVersion = 0;
    private int svPatchVersion = 0;
    private boolean isGenericVersion = false;

    public DeviceVersion(String swVersion, String hwVersion) {
      super();
      setSwVersion(swVersion);
      this.hwVersion = hwVersion;
    }

    public String getSwVersion() {
      return svMajorVersion + "." + svMinorVersion + "." + svPatchVersion;
    }

    public void setSwVersion(String swVersion) {
      // TODO: conversion to integers

      if (swVersion.indexOf(".") > 0) {
        String[] verArray = swVersion.split("\\.");


        for (int i = 0; i < verArray.length; i++) {

          if (verArray[i].equals("*")) {
            verArray[i] = "0";
          }
        }
        svMajorVersion = Integer.parseInt(verArray[0]);
        svMinorVersion = Integer.parseInt(verArray[1]);
        svPatchVersion = Integer.parseInt(verArray[2]);

      } else if (swVersion.indexOf("x") > 0) {
        swVersion = "*";
      } else if (swVersion.equals("*")) {
        isGenericVersion = true;
      }
    }

    public String getHwVersion() {
      return hwVersion;
    }

    public void setHwVersion(String hwVersion) {
      this.hwVersion = hwVersion;
    }

    private String hwVersion;

    public int getSvMajorVersion() {
      return svMajorVersion;
    }

    public void setSvMajorVersion(int svMajorVersion) {
      this.svMajorVersion = svMajorVersion;
    }

    public int getSvMinorVersion() {
      return svMinorVersion;
    }

    public void setSvMinorVersion(int svMinorVersion) {
      this.svMinorVersion = svMinorVersion;
    }

    public int getSvPatchVersion() {
      return svPatchVersion;
    }

    public void setSvPatchVersion(int svPatchVersion) {
      this.svPatchVersion = svPatchVersion;
    }

    public boolean isGenericVersion() {
      return isGenericVersion;
    }

    public void setGenericVersion(boolean isGenericVersion) {
      this.isGenericVersion = isGenericVersion;
    }

    @Override
    public int compareTo(DeviceVersion o) {

      if (svMajorVersion != o.svMajorVersion) {
        return (svMajorVersion - o.svMajorVersion);
      } else if (svMinorVersion != o.svMinorVersion) {
        return svMinorVersion - o.svMinorVersion;
      } else {
        return svPatchVersion - o.svPatchVersion;
      }
    }
  }

}
