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
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class MOMetaDataUtil {

  private static final String STRING_I = ".{i}.";
  private static final Logger LOG = LoggerFactory.getLogger(MOMetaDataUtil.class);
  private static final String GENERIC_INDEX_REGEX = "\\.\\{[i-l]\\}\\.";
  private static final String INDEX_REGEX = "\\.[0-9]{1,}\\.";
  private static final String MO_META_DATA_FILE_LOCATION = "mapper-schema";
  private Map<String, MOMetaData> metaDataMap;
  private Map<String, String> metaDataReverseMap;
  public static final String ORAN_SW_MGMT_URI = "urn:o-ran:software-management:1.0";

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
    Collection<File> files = FileUtils.listFiles(new File(fileLocation), null, false);

    for (File file : files) {
      LOG.info("Loading mapper schema from {}", file.getName());
      List<String> lines = FileUtils.readLines(file, StandardCharsets.UTF_8);
      for (String line : lines) {
        if (line != null && line.startsWith("#")) {
          continue;
        }
        if (line != null && line.split(",").length >= 3) {

          parseMetaDataLine(line, metaDataMap, metaDataReverseMap);

        }
      }
    }
  }

  private static void parseMetaDataLine(String line, Map<String, MOMetaData> metaDataMap,
      Map<String, String> metaDataReverseMap) {
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
    prepareMOMetaData(isReadOnly, isTabluar, split, isTabObject, metaDataMap, metaDataReverseMap);
  }

  private static void prepareMOMetaData(boolean isReadOnly, boolean isTabluar, String[] split,
      boolean isTabObject, Map<String, MOMetaData> metaDataMap,
      Map<String, String> metaDataReverseMap) {
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
      metaDataMap.put(logMessage, metaTabData);
    }
    MOMetaData metaData = new MOMetaData(split[0], dataType, isReadOnly, isTabluar, isTabObject);
    if ((split.length > 4 && split[4] != null) && split[4].trim().length() > 0) {
      metaData.setURI(split[4]);
    }
    metaDataMap.put(split[1], metaData);
    metaDataReverseMap.put(split[0], split[1]);
  }

  public MOMetaData getMetaDataByNetConfName(String moName) {
    String moNameInGnrForm = moName.replaceAll(INDEX_REGEX, STRING_I);
    return metaDataMap.get(moNameInGnrForm);
  }

  public MOMetaData getMetaDataByTR69Name(String moName) {
    String moNameInGnrForm = moName.replaceAll(INDEX_REGEX, STRING_I);
    String netconfMoName = metaDataReverseMap.get(moNameInGnrForm);
    return metaDataMap.get(netconfMoName);
  }

  public String getNetconfNameByTR69Name(String moName) {
    String moNameInGnrForm = moName.replaceAll(INDEX_REGEX, STRING_I);
    return metaDataReverseMap.get(moNameInGnrForm);
  }

  public String getNetconfNameByTR69NameWithIndexes(String moName) {
    String moNameInGnrForm = moName.replaceAll(INDEX_REGEX, STRING_I);
    String netConfNMoName = metaDataReverseMap.get(moNameInGnrForm);
    return netConfNMoName != null ? getNetConfMOByReplacingIndexes(netConfNMoName, moName)
        : netConfNMoName;
  }

  public Set<String> getAllMONames() {
    return metaDataMap.keySet();
  }

  public List<ParameterDTO> getSupportedChildParameters(List<ParameterDTO> parameters) {
    List<ParameterDTO> result = new ArrayList<>();
    Set<MOMetaData> allMatchedChilds = new HashSet<>();
    for (ParameterDTO param : parameters) {
      String parentMONameInGnrc = param.getParamName().replaceAll(INDEX_REGEX, STRING_I);
      MOMetaData moData = metaDataMap.get(parentMONameInGnrc);
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
