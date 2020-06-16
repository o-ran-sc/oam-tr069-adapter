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

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;

import org.apache.commons.io.FileUtils;
import org.commscope.tr069adapter.mapper.ErrorCodeMetaData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class ErrorCodeUtil {


  private static final Logger LOG = LoggerFactory.getLogger(ErrorCodeUtil.class);

  private static final String ERROR_CODE_FILE = "error-code-mapping.json";
  private static Map<String, ErrorCodeMetaData> errorCodeMap;

  @PostConstruct
  public static void loadErrorCodeData() {
    File file = new File(ERROR_CODE_FILE);
    try {
      errorCodeMap = getMetaDataAsMap(file);
    } catch (IOException e) {
      LOG.error("Exception : {}", e.getMessage());
    }
  }

  public void printErrorCodeMap() {
    for (Entry<String, ErrorCodeMetaData> entry : errorCodeMap.entrySet()) {
      LOG.debug("KEY= {}", entry.getKey());
      LOG.debug("VALUE= {}", entry.getValue());
    }
  }

  private static Map<String, ErrorCodeMetaData> getMetaDataAsMap(File file) throws IOException {

    String json = FileUtils.readFileToString(file, StandardCharsets.UTF_8);

    Map<String, ErrorCodeMetaData> result = null;
    ObjectMapper mapper = new ObjectMapper();
    try {
      result = mapper.readValue(json, new TypeReference<Map<String, ErrorCodeMetaData>>() {});
    } catch (IOException e) {
      LOG.error("IOException while loading device model meta data {}", e.toString());
      LOG.error("Exception : {}", e.getMessage());
    }
    return result;
  }

  public ErrorCodeMetaData getErrorCodeMetaData(String errorCode) {
    return errorCodeMap.get(errorCode);
  }

}
