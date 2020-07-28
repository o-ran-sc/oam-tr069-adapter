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

package org.commscope.tr069adapter.mapper.util;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.annotation.PostConstruct;
import org.apache.commons.io.FileUtils;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.common.deviceversion.DeviceVersionManager;
import org.commscope.tr069adapter.common.deviceversion.ProfileDefinition;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MOMetaDataUtil {

  private static final String STRING_I = ".{i}.";
  private static final Logger LOG = LoggerFactory.getLogger(MOMetaDataUtil.class);
  private static final String GENERIC_INDEX_REGEX = "\\.\\{[i-l]\\}\\.";
  private static final String INDEX_REGEX = "\\.[0-9]{1,}\\.";
  private static final String MO_META_DATA_FILE_LOCATION = "mapper-schema";
  private Map<String, Map<String, MOMetaData>> metaDataMap;
  private Map<String, Map<String, String>> metaDataReverseMap;
  public static final String ORAN_SW_MGMT_URI = "urn:o-ran:software-management:1.0";

  @Autowired
  DeviceVersionManager versionManager;

  @PostConstruct
  public void loadMetaData() {

    try {
      LOG.info("Loading mapper schema");
      if (metaDataMap != null)
        metaDataMap.clear();
      if (metaDataReverseMap != null)
        metaDataReverseMap.clear();
      getMetaDataAsMap(MO_META_DATA_FILE_LOCATION);
      LOG.info("Loading mapper schema successfully completed");
    } catch (IOException e) {
      LOG.error("Exception : {}", e.getMessage());
    }
  }

  private void getMetaDataAsMap(String fileLocation) throws IOException {
    metaDataMap = new HashMap<>();
    metaDataReverseMap = new HashMap<>();

    List<ProfileDefinition> profiles = versionManager.getSupportedProfileDefinitions();
    for (ProfileDefinition profileDefinition : profiles) {
      Map<String, MOMetaData> profileMetaDataMap = new HashMap<>();
      Map<String, String> profileMetaDataReverseMap = new HashMap<>();

      Collection<File> files = FileUtils.listFiles(
          new File(fileLocation + "/" + profileDefinition.getCsdmMappingPath()), null, false);
      for (File file : files) {
        LOG.info("Loading mapper schema from {}", file.getName());
        List<String> lines = FileUtils.readLines(file, StandardCharsets.UTF_8);
        for (String line : lines) {
          if (line != null && line.startsWith("#")) {
            continue;
          }
          if (line != null && line.split(",").length >= 3) {
            parseMetaDataLine(line, profileMetaDataMap, profileMetaDataReverseMap);
          }
        }
      }
      metaDataMap.put(profileDefinition.getProfileId(), profileMetaDataMap);
      metaDataReverseMap.put(profileDefinition.getProfileId(), profileMetaDataReverseMap);
    }
  }

  private static void parseMetaDataLine(String line, Map<String, MOMetaData> profileMetaDataMap,
      Map<String, String> profileMetaDataReverseMap) {
    String[] split = line.split(",");
    boolean isReadOnly = false;
    boolean isTabluar = false;
    boolean isTabObject = false;
    if (split[2].contains("-")) {
      String[] dataAttr = split[2].split("-");
      if ("Tabular".equalsIgnoreCase(dataAttr[0]))
        isTabluar = true;
      if ("ReadOnly".equalsIgnoreCase(dataAttr[1]))
        isReadOnly = true;
    } else if ("TabularObject".equalsIgnoreCase(split[2])) {
      isTabObject = true;
    }
    prepareMOMetaData(isReadOnly, isTabluar, split, isTabObject, profileMetaDataMap,
        profileMetaDataReverseMap);
  }

  private static void prepareMOMetaData(boolean isReadOnly, boolean isTabluar, String[] split,
      boolean isTabObject, Map<String, MOMetaData> profileMetaDataMap,
      Map<String, String> profileMetaDataReverseMap) {
    String dataType = "";
    if (split.length > 3) {
      dataType = split[3];
    }
    if (isTabObject) {
      String logMessage = split[1].substring(0, split[1].length() - 5);
      LOG.info("Adding Parent Objects {}", logMessage);
      String substring = split[0].substring(0, split[0].length() - 4);
      MOMetaData metaTabData =
          new MOMetaData(substring, dataType, isReadOnly, isTabluar, isTabObject);
      if ((split.length > 4 && split[4] != null) && split[4].trim().length() > 0) {
        metaTabData.setURI(split[4]);
      }
      profileMetaDataMap.put(logMessage, metaTabData);
    }
    MOMetaData metaData = new MOMetaData(split[0], dataType, isReadOnly, isTabluar, isTabObject);
    if ((split.length > 4 && split[4] != null) && split[4].trim().length() > 0) {
      metaData.setURI(split[4]);
    }
    profileMetaDataMap.put(split[1], metaData);
    profileMetaDataReverseMap.put(split[0], split[1]);
  }

  public MOMetaData getMetaDataByNetConfName(String moName, String swVersion, String hwVersion) {
    String moNameInGnrForm = moName.replaceAll(INDEX_REGEX, STRING_I);

    String profileId = versionManager.getAssociatedProfileId(swVersion, hwVersion);
    Map<String, MOMetaData> metaData = metaDataMap.get(profileId);

    return metaData.get(moNameInGnrForm);
  }

  public MOMetaData getMetaDataByTR69Name(String moName, String swVersion, String hwVersion) {
    String moNameInGnrForm = moName.replaceAll(INDEX_REGEX, STRING_I);

    String profileId = versionManager.getAssociatedProfileId(swVersion, hwVersion);
    Map<String, String> profileReverseMetaData = metaDataReverseMap.get(profileId);
    Map<String, MOMetaData> profileMetaData = metaDataMap.get(profileId);

    String netconfMoName = profileReverseMetaData.get(moNameInGnrForm);
    return profileMetaData.get(netconfMoName);
  }

  public String getNetconfNameByTR69NameWithIndexes(String moName, String swVersion,
      String hwVersion) {
    String moNameInGnrForm = moName.replaceAll(INDEX_REGEX, STRING_I);
    String profileId = versionManager.getAssociatedProfileId(swVersion, hwVersion);
    Map<String, String> reverseMetaData = metaDataReverseMap.get(profileId);
    String netConfNMoName = reverseMetaData.get(moNameInGnrForm);
    return netConfNMoName != null ? getNetConfMOByReplacingIndexes(netConfNMoName, moName)
        : netConfNMoName;
  }

  public List<ParameterDTO> getSupportedChildParameters(List<ParameterDTO> parameters,
      String swVersion, String hwVersion) {
    List<ParameterDTO> result = new ArrayList<>();
    Set<MOMetaData> allMatchedChilds = new HashSet<>();
    String profileId = versionManager.getAssociatedProfileId(swVersion, hwVersion);
    Map<String, MOMetaData> profileMetaData = metaDataMap.get(profileId);
    for (ParameterDTO param : parameters) {
      String parentMONameInGnrc = param.getParamName().replaceAll(INDEX_REGEX, STRING_I);
      MOMetaData moData = profileMetaData.get(parentMONameInGnrc);
      if (moData != null) {
        allMatchedChilds.add(new MOMetaData(
            getTR69MOByReplacingIndexes(param.getParamName(), moData.getMoName()),
            moData.getDataType(), moData.isReadOnly(), moData.isTabluar(), moData.isTabluarObj()));
      }
    }
    for (MOMetaData metaData : allMatchedChilds) {
      ParameterDTO param = new ParameterDTO();
      String paramName = metaData.getMoName();

      param.setParamName(paramName);
      param.setDataType(metaData.getDataType());
      result.add(param);
    }
    return result;
  }

  public List<ParameterDTO> getDeviceSupportedChildParameters() {
    List<ParameterDTO> result = new ArrayList<>();

    ParameterDTO param1 = new ParameterDTO();
    param1.setParamName("Device.DeviceInfo.Description");
    param1.setParamValue("Internal");
    param1.setDataType("string");
    result.add(param1);
    return result;
  }

  public static String getTR69MOByReplacingIndexes(String netconfMo, String tr69Mo) {

    String[] split = netconfMo.split("\\.");
    for (int i = 0; i < split.length; i++) {
      if (split[i].matches("[0-9]{1,}")) {
        tr69Mo = tr69Mo.replaceFirst(GENERIC_INDEX_REGEX, "." + split[i] + ".");
      }
    }
    return tr69Mo;
  }

  public static String getNetConfMOByReplacingIndexes(String netconfMo, String tr69Mo) {

    String[] split = tr69Mo.split("\\.");
    for (int i = 0; i < split.length; i++) {
      if (split[i].matches("[0-9]{1,}")) {
        netconfMo = netconfMo.replaceFirst(GENERIC_INDEX_REGEX, "." + split[i] + ".");
      }
    }
    return netconfMo;
  }
}
