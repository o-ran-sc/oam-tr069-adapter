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

package org.commscope.tr069adapter.mapper.netconf;

import java.util.ArrayList;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class NetConfNotificationSender {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfNotificationSender.class);
  private static final String BOOLEAN_TRUE_VALUE = "1";
  private static final String BOOLEAN_FALSE_VALUE = "0";
  private static final String BOOLEAN_DATA_TYPE = "boolean";

  @Autowired
  MapperConfigProperties config;

  @Autowired
  MOMetaDataUtil metaDataUtil;

  @Autowired
  RestTemplate restTemplate;

  public ResponseEntity sendNotification(DeviceInform deviceInform) {
    ResponseEntity response = null;
    final String uri = getUri();
    LOG.debug("Posting notification to netconf server {}", uri);

    try {
      LOG.debug("deviceInform : {} {}", deviceInform.getInformTypeList(),
          deviceInform.getParameters());
      convertTR069ToNetConfParams(deviceInform);
      LOG.debug("Posting notification to netconf server");
      response = restTemplate.postForObject(uri, deviceInform, ResponseEntity.class);
      LOG.debug("Posting notification to netconf server completed ");
    } catch (Exception e) {
      LOG.error("Exception while sending the notification.", e);
    }
    return response;
  }

  private void convertTR069ToNetConfParams(DeviceInform deviceInform) {
    List<ParameterDTO> removeList = new ArrayList<>();
    if (null != deviceInform) {
      for (ParameterDTO param : deviceInform.getParameters()) {
        if (param.getParamValue() == null || param.getParamValue().trim().length() <= 0) {
          continue;
        }
        handleBooleanParameters(param);
        if (null != param.getParamName()) {
          String netConfMOName =
              metaDataUtil.getNetconfNameByTR69NameWithIndexes(param.getParamName());
          if (null != netConfMOName)
            param.setParamName(netConfMOName);
          else
            removeList.add(param); // unknown parameter found.
        }
      }
      deviceInform.getParameters().removeAll(removeList); // remove unknown
      // parameters
    }
  }

  private void handleBooleanParameters(ParameterDTO param) {
    MOMetaData metaData = metaDataUtil.getMetaDataByTR69Name(param.getParamName());
    if (null != metaData && BOOLEAN_DATA_TYPE.equalsIgnoreCase(metaData.getDataType())) {
      if (BOOLEAN_TRUE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
        param.setParamValue(Boolean.TRUE.toString());
      } else if (BOOLEAN_FALSE_VALUE.equalsIgnoreCase(param.getParamValue().trim())) {
        param.setParamValue(Boolean.FALSE.toString());
      }
    }
  }

  private String getUri() {
    return config.getNbiNotificationUri();
  }

}
