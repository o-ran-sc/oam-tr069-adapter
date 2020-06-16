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
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceInform;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.InformType;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.inform.BootInform;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.acs.common.inform.PeriodicInform;
import org.commscope.tr069adapter.acs.common.inform.ValueChangeInform;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.acs.ACSNotificationHandler;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.NetconfServerManagementError;
import org.commscope.tr069adapter.mapper.netconf.NetConfNotificationSender;
import org.commscope.tr069adapter.mapper.netconf.NetConfServerManager;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.commscope.tr069adapter.mapper.ves.VESNotificationSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ACSNotificationHandlerImpl implements ACSNotificationHandler {

  private static final Logger logger = LoggerFactory.getLogger(ACSNotificationHandlerImpl.class);

  @Autowired
  SynchronizedRequestHandler syncHandler;

  @Autowired
  MOMetaDataUtil metaDataUtil;

  @Autowired
  PnPPreProvisioningHandler pnpPreProvisioningHandler;

  @Autowired
  VESNotificationSender vesnotiSender;

  @Autowired
  NetConfNotificationSender notiSender;

  @Autowired
  MapperConfigProperties config;

  @Autowired
  NetConfServerManager netconfManager;

  @Override
  public void handleOperationResponse(DeviceRPCResponse opResult) {
    opResult.getOperationResponse().setParameterDTOs(
        filterUnsupportedParameters(opResult.getOperationResponse().getParameterDTOs()));
    syncHandler.notifyResult(opResult);
  }

  @Override
  public void handleNotification(DeviceInform notification) {
    boolean isAlarmVC = isAlarmVC(notification);

    if (notification instanceof BootstrapInform) {
      logger.info("BootStrap notification received");
      BootstrapInform bootstrapNotification = (BootstrapInform) notification;
      // send request to create the netconf server instance for the bootstrap device
      // id
      NetConfServerDetails serverInfo = createNtConfServer(bootstrapNotification);
      if (serverInfo == null)
        return;

      vesnotiSender.sendNotification(bootstrapNotification, serverInfo);
      BootstrapInform bsInform =
          getDeviceBootStrapNotification(bootstrapNotification, TR069InformType.BOOTSTRAP);
      if (bootstrapNotification.getValueChangeNotification() != null) {
        logger.info("Bootstrap notification received along with VC");
        ValueChangeInform vcInform =
            getDeviceValueChangeNotification(bootstrapNotification, TR069InformType.VALUECHANGE);
        processVCNotification(vcInform, isAlarmVC);
      }
      notiSender.sendNotification(bsInform);
    } else if (notification instanceof BootInform) {
      logger.info("Boot notification received");
      BootInform bootNotification = (BootInform) notification;
      BootInform bInform = getDeviceBootNotification(bootNotification, TR069InformType.BOOT);
      if (bootNotification.getValueChangeNotification() != null) {
        logger.info("Boot notification received along with VC");
        ValueChangeInform vcInform =
            getDeviceValueChangeNotification(bootNotification, TR069InformType.VALUECHANGE);
        processVCNotification(vcInform, isAlarmVC);
      }
      notiSender.sendNotification(bInform);
    } else if (notification instanceof PeriodicInform) {
      PeriodicInform pINotificaiton = (PeriodicInform) notification;
      vesnotiSender.sendNotification(pINotificaiton, null);
      notiSender.sendNotification(pINotificaiton);
      logger.info("VC notification received");
    } else if (notification instanceof ValueChangeInform) {
      ValueChangeInform valueChgNotificaiton = (ValueChangeInform) notification;
      processVCNotification(valueChgNotificaiton, isAlarmVC);
    }

    pnpPreProvisioningHandler.onDeviceNotification(notification);
  }

  private NetConfServerDetails createNtConfServer(BootstrapInform bootstrapNotification) {
    String eNodeBName = pnpPreProvisioningHandler
        .getEnodeBName(bootstrapNotification.getDeviceDetails().getDeviceId());
    if (eNodeBName == null)
      eNodeBName = bootstrapNotification.getDeviceDetails().getDeviceId();
    NetConfServerDetails serverInfo = netconfManager
        .createNetconfServer(bootstrapNotification.getDeviceDetails().getDeviceId(), eNodeBName);
    if (serverInfo != null && !NetconfServerManagementError.SUCCESS.equals(serverInfo.getError())) {
      logger.error("Failed to handle bootstrap notification. Server INFO: {}", serverInfo);
      logger.error("Failed to create the netconf server for device ID: {}  Error: {}",
          bootstrapNotification.getDeviceDetails().getDeviceId(), serverInfo.getError());
      return null;
    } else if (serverInfo == null) {
      logger.error(
          "Failed to handle bootstrap notification. Failed to create netconf server. serverInfo is null");
      return null;
    }
    return serverInfo;
  }

  private void processVCNotification(ValueChangeInform valueChgNotificaiton, boolean isAlarmVC) {
    if (isAlarmVC) {
      logger.debug("Alarm VC received forwarding to VES Collector");
      vesnotiSender.sendNotification(valueChgNotificaiton, null);
    } else {
      logger.info("VC notification received");
      notiSender.sendNotification(valueChgNotificaiton);
    }
  }

  private boolean isAlarmVC(DeviceInform notification) {
    if (null != notification && null != notification.getParameters()) {
      for (ParameterDTO param : notification.getParameters()) {
        if (param.getParamName().matches(config.getAlarmMORegex())) {
          logger.debug("This VC contains alarm MOs");
          return true;
        }
      }
    }
    return false;
  }

  public List<ParameterDTO> filterUnsupportedParameters(List<ParameterDTO> parameters) {
    List<ParameterDTO> result = new ArrayList<>();
    if (null != parameters) {
      for (ParameterDTO param : parameters) {
        MOMetaData metaData = metaDataUtil.getMetaDataByTR69Name(param.getParamName());
        if (null != metaData) {
          result.add(param);
        }
      }
    }
    return result;
  }

  public static BootstrapInform getDeviceBootStrapNotification(DeviceInform devNotification,
      TR069InformType notificationType) {
    BootstrapInform bsInform = new BootstrapInform();
    List<InformType> informTypeList = new ArrayList<>();
    informTypeList.add(notificationType);
    bsInform.setDeviceDetails(devNotification.getDeviceDetails());
    bsInform.setInformTypeList(informTypeList);
    List<ParameterDTO> paramList = new ArrayList<>();
    for (ParameterDTO param : devNotification.getParameters()) {
      paramList.add(new ParameterDTO(param.getParamName(), param.getParamValue()));
    }
    bsInform.setParameters(paramList);
    return bsInform;
  }

  public static BootInform getDeviceBootNotification(DeviceInform devNotification,
      TR069InformType notificationType) {
    BootInform bInform = new BootInform();
    List<InformType> informTypeList = new ArrayList<>();
    informTypeList.add(notificationType);
    bInform.setDeviceDetails(devNotification.getDeviceDetails());
    bInform.setInformTypeList(informTypeList);
    List<ParameterDTO> paramList = new ArrayList<>();
    for (ParameterDTO param : devNotification.getParameters()) {
      paramList.add(new ParameterDTO(param.getParamName(), param.getParamValue()));
    }
    bInform.setParameters(paramList);
    return bInform;
  }

  public static ValueChangeInform getDeviceValueChangeNotification(DeviceInform devNotification,
      TR069InformType notificationType) {
    ValueChangeInform devValChangeNotif = new ValueChangeInform();
    List<InformType> informTypeList = new ArrayList<>();
    informTypeList.add(notificationType);
    devValChangeNotif.setDeviceDetails(devNotification.getDeviceDetails());
    devValChangeNotif.setInformTypeList(informTypeList);
    List<ParameterDTO> paramList = new ArrayList<>();
    for (ParameterDTO param : devNotification.getParameters()) {
      paramList.add(new ParameterDTO(param.getParamName(), param.getParamValue()));
    }
    devValChangeNotif.setParameters(paramList);
    devValChangeNotif.setExternalIPAddress(getExternalIPAddress(devNotification.getParameters()));
    return devValChangeNotif;
  }

  private static String getExternalIPAddress(List<ParameterDTO> params) {
    ParameterDTO[] nbiParam = params.toArray(new ParameterDTO[params.size()]);
    String externalIpAddress = "";
    boolean isIPv6 = isIPv6Enabled(nbiParam);

    for (int index1 = 0; index1 < nbiParam.length; index1++) {

      if (isIPv6) {
        if (nbiParam[index1].getParamName().contains("IPv6Address")
            || nbiParam[index1].getParamName().contains(".1.IPInterfaceIPAddress")) {
          externalIpAddress = nbiParam[index1].getParamValue();
          logger.debug("device communicating is with IPV6 address");
        }
      } else {
        if (nbiParam[index1].getParamName().contains("IPv4Address")
            || nbiParam[index1].getParamName().contains("ExternalIPAddress")
            || nbiParam[index1].getParamName().contains(".1.IPInterfaceIPAddress")) {
          externalIpAddress = nbiParam[index1].getParamValue();
        }
      }
      if (externalIpAddress.trim().length() > 0)
        break;
    }
    return externalIpAddress;
  }

  private static boolean isIPv6Enabled(ParameterDTO[] nbiParam) {
    boolean isIPv6 = false;
    for (int index1 = 0; index1 < nbiParam.length; index1++) {
      if (nbiParam[index1].getParamName().contains("IPv6Enable")
          && nbiParam[index1].getParamValue().equalsIgnoreCase("1")) {
        isIPv6 = true;
        break;
      }
    }
    return isIPv6;
  }
}
