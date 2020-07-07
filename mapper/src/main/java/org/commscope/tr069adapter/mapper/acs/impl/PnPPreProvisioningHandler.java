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

package org.commscope.tr069adapter.mapper.acs.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.ConfigurationData;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.acs.common.inform.BootInform;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.commscope.tr069adapter.mapper.util.NetconfToTr069MapperUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestTemplate;

@Component
public class PnPPreProvisioningHandler {

  private static final Logger logger = LoggerFactory.getLogger(PnPPreProvisioningHandler.class);

  private static String clientString = "client";
  private static final String ADMIN_STATE = "AdminState";
  private static final String ENODEB_NAME = "X_0005B9_eNBName";

  @Autowired
  SynchronizedRequestHandler syncHandler;

  @Autowired
  MOMetaDataUtil moMetaDataUtil;

  @Autowired
  MapperConfigProperties config;

  private RestTemplate restTemplate = new RestTemplate();

  /**
   * @param notification
   */
  public void onDeviceNotification(DeviceInform notification) {
    String deviceId = null;
    try {
      if (notification instanceof BootstrapInform || notification instanceof BootInform) {
        logger.debug("Checking whether the PnP pre-configuration is enabled");
        if (isPreConfigureOnPnPEnabled()) {
          logger.info(
              "Pre-configuration during Bootstrap or Boot is enabled, hence the configuration imported in Config DB shall be provisioned to the device");
        } else {
          logger.debug("Pre-configuration on PnP is disabled!!!");
          return;
        }
      }

      if (notification instanceof BootstrapInform) {
        BootstrapInform bootstrapNotification = (BootstrapInform) notification;
        deviceId = bootstrapNotification.getDeviceDetails().getDeviceId();
        MDC.put(clientString, deviceId);
        logger.info("Bootstrap notification received");
        performPreProvisioning(deviceId, false);

      } else if (notification instanceof BootInform) {
        BootInform bootNotification = (BootInform) notification;
        deviceId = bootNotification.getDeviceDetails().getDeviceId();
        MDC.put(clientString, deviceId);
        logger.info("Boot notification received");

        performPreProvisioning(deviceId, true);
      }

      logger.debug("Successfully completed provisioning of PnP mandatory parameters");
    } finally {
      MDC.remove(clientString);
    }
  }

  /**
   * @param deviceId
   * @param isBoot
   */
  private void performPreProvisioning(String deviceId, boolean isBoot) {
    List<DeviceRPCRequest> deviceRPCRequestList =
        prepareNBIDeviceOperationrequest(deviceId, isBoot);
    if (deviceRPCRequestList.isEmpty()) {
      logger.debug("No Operation requests exists to perform pre provision on the device");
      return;
    }

    boolean isMandatoryProvFailed = false;
    for (DeviceRPCRequest deviceRPCRequest : deviceRPCRequestList) {
      logger.info("Performing PROVISION operation");
      DeviceRPCResponse deviceRPCResponse = syncHandler.performDeviceOperation(deviceRPCRequest);
      logger.debug("Received Provisioning Operation result");
      if (deviceRPCResponse == null || !StringUtils.isEmpty(deviceRPCResponse.getFaultString())) {
        logger.error("Device operation failed, Reason: {}",
            ((deviceRPCResponse == null) ? "Null Operation result"
                : deviceRPCResponse.getFaultString()));
        isMandatoryProvFailed = true;
        break;
      }

      logger.debug("Provisioning is successful");
    }

    if (isMandatoryProvFailed) {
      logger
          .debug("Mandatory provisioning has failed, hence provisioning Admin down on the device");
      provisionAdminDown(deviceRPCRequestList);
      logger.debug("AdminDown Provisioning is successful");
    }
  }

  private void provisionAdminDown(List<DeviceRPCRequest> deviceRPCRequestList) {
    DeviceRPCRequest adminDownOpRequest = null;
    for (DeviceRPCRequest nbiDeviceOperationRequest : deviceRPCRequestList) {
      ParameterDTO param = nbiDeviceOperationRequest.getOpDetails().getParmeters().get(0);
      if (param.getParamName().endsWith(ADMIN_STATE)) {
        adminDownOpRequest = nbiDeviceOperationRequest;
        break;
      }
    }
    if (adminDownOpRequest != null) {
      List<ParameterDTO> adminDownParams = adminDownOpRequest.getOpDetails().getParmeters();
      for (ParameterDTO adminDownParam : adminDownParams) {
        adminDownParam.setParamValue("0");
      }
      DeviceRPCResponse deviceRPCResponse = syncHandler.performDeviceOperation(adminDownOpRequest);
      if (deviceRPCResponse == null || !StringUtils.isEmpty(deviceRPCResponse.getFaultString())) {
        logger.error("Device operation failed, Reason: {}",
            ((deviceRPCResponse == null) ? "Null Operation result"
                : deviceRPCResponse.getFaultString()));
      }
    }
  }

  /**
   * @param deviceId
   * @param isBoot
   * @return
   */
  private List<DeviceRPCRequest> prepareNBIDeviceOperationrequest(String deviceId, boolean isBoot) {
    logger.debug("Preparing the NBI Device Operation Request");
    List<DeviceRPCRequest> deviceRPCRequestList = new ArrayList<>();

    ConfigurationData configData = getDeviceConfigurationData(deviceId);
    if (configData == null || configData.getParameterMONameValueMap().isEmpty()) {
      logger.debug("No configuration exists for the device");
      return deviceRPCRequestList;
    }

    List<ParameterDTO> configParams = new ArrayList<>();
    List<ParameterDTO> adminStateParams = new ArrayList<>();
    Map<String, String> paramNameValueMap = configData.getParameterMONameValueMap();
    Iterator<String> iter = paramNameValueMap.keySet().iterator();
    while (iter.hasNext()) {
      String paramName = iter.next();
      String paramValue = paramNameValueMap.get(paramName);
      MOMetaData moMetaData = moMetaDataUtil.getMetaDataByTR69Name(paramName);
      if ((isBoot && !paramName.endsWith(ADMIN_STATE)) || moMetaData == null)
        continue;
      ParameterDTO parameterDTO = getParameterDTO(paramName, paramValue, moMetaData);

      if (paramName.endsWith(ADMIN_STATE)) {
        adminStateParams.add(parameterDTO);
      } else {
        configParams.add(parameterDTO);
      }
      logger.debug("Param -> {} Param Value: {}", paramName, paramValue);
    }

    if (configParams.isEmpty() && adminStateParams.isEmpty()) {
      logger.debug("Empty parameters list from config db, hence not performing pre-provision");
      return deviceRPCRequestList;
    }

    if (!configParams.isEmpty()) {
      deviceRPCRequestList.add(createNBIOperationRequest(deviceId, configParams));
    }

    if (!adminStateParams.isEmpty()) {
      deviceRPCRequestList.add(createNBIOperationRequest(deviceId, adminStateParams));
    }

    return deviceRPCRequestList;
  }

  private ParameterDTO getParameterDTO(String paramName, String paramValue, MOMetaData moMetaData) {
    String dataType = moMetaData.getDataType();
    if (dataType.equals("boolean")) {
      if (paramValue.equalsIgnoreCase("true")) {
        paramValue = "1";
      } else if (paramValue.equalsIgnoreCase("false")) {
        paramValue = "0";
      }
    }
    ParameterDTO parameterDTO = new ParameterDTO(paramName, paramValue);
    parameterDTO.setDataType(dataType);

    return parameterDTO;
  }

  /**
   * @param deviceId
   * @param params
   * @return
   */
  private DeviceRPCRequest createNBIOperationRequest(String deviceId, List<ParameterDTO> params) {
    TR069OperationDetails opDetails = new TR069OperationDetails();
    opDetails.setOpCode(TR069OperationCode.SET_PARAMETER_VALUES);

    return NetconfToTr069MapperUtil.handleParamsOperation(params, opDetails, deviceId);

  }

  /**
   * @param deviceId
   * @return
   */
  private ConfigurationData getDeviceConfigurationData(String deviceId) {
    String configDBURI = getConfigDBURI();
    logger.debug("Device Configuration to be fetched from Config DB URI: {}", configDBURI);
    ConfigurationData configData = null;
    try {
      configData = restTemplate.getForObject(configDBURI + deviceId, ConfigurationData.class);
    } catch (Exception e) {
      logger.error("An exception occurred to get the initial device configuration, Reason: {}",
          e.getMessage());
    }
    return configData;
  }

  /**
   * @return
   */
  private boolean isPreConfigureOnPnPEnabled() {
    boolean isEnabled = false;
    String preConfigureOnPnP = config.getPreConfigureOnPNP();
    if (preConfigureOnPnP != null && ("true".equalsIgnoreCase(preConfigureOnPnP)
        || "false".equalsIgnoreCase(preConfigureOnPnP))) {
      isEnabled = Boolean.valueOf(preConfigureOnPnP);
    }

    return isEnabled;
  }

  /**
   * @return
   */
  private String getConfigDBURI() {
    return config.getConfigDBUri();

  }

  /**
   * it will return the eNodBName if the configuration is imported
   * 
   * @return String
   */
  public String getEnodeBName(String deviceId) {
    String eNodeBName = null;
    if (isPreConfigureOnPnPEnabled()) {
      ConfigurationData configData = getDeviceConfigurationData(deviceId);
      if (configData == null || configData.getParameterMONameValueMap().isEmpty()) {
        logger.debug("No configuration exists for the device");
        return eNodeBName;
      }

      Map<String, String> paramNameValueMap = configData.getParameterMONameValueMap();
      Iterator<String> iter = paramNameValueMap.keySet().iterator();
      while (iter.hasNext()) {
        String paramName = iter.next();
        if (paramName.endsWith(ENODEB_NAME)) {
          eNodeBName = paramNameValueMap.get(paramName);
          break;
        }
      }
    }
    return eNodeBName;
  }
}
