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
import org.commscope.tr069adapter.acs.common.inform.ConnectionRequestInform;
import org.commscope.tr069adapter.acs.common.inform.PeriodicInform;
import org.commscope.tr069adapter.acs.common.inform.TransferCompleteInform;
import org.commscope.tr069adapter.acs.common.inform.ValueChangeInform;
import org.commscope.tr069adapter.mapper.MOMetaData;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.acs.ACSNotificationHandler;
import org.commscope.tr069adapter.mapper.dao.DeviceOperationsDAO;
import org.commscope.tr069adapter.mapper.entity.DeviceOperationDetails;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;
import org.commscope.tr069adapter.mapper.model.NetconfServerManagementError;
import org.commscope.tr069adapter.mapper.netconf.NetConfNotificationSender;
import org.commscope.tr069adapter.mapper.netconf.NetConfServerManager;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.commscope.tr069adapter.mapper.util.FirwareUpgradeErrorCode;
import org.commscope.tr069adapter.mapper.util.FirwareUpgradeStatus;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.commscope.tr069adapter.mapper.ves.VESNotificationSender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ACSNotificationHandlerImpl implements ACSNotificationHandler {

  private static final Logger logger = LoggerFactory.getLogger(ACSNotificationHandlerImpl.class);
  private static final String SOFT_MGMT_NS_URI = "urn:o-ran:software-management:1.0";

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

  @Autowired
  DeviceOperationsDAO deviceOperDAO;

  @Override
  public void handleOperationResponse(DeviceRPCResponse opResult) {
    opResult.getOperationResponse().setParameterDTOs(
        filterUnsupportedParameters(opResult.getOperationResponse().getParameterDTOs(),
            opResult.getDeviceDetails().getSoftwareVersion(),
            opResult.getDeviceDetails().getHardwareVersion()));
    syncHandler.notifyResult(opResult);
  }

  @Override
  public void handleNotification(DeviceInform notification) {
    boolean isAlarmVC = isAlarmVC(notification);

    if (notification instanceof BootstrapInform) {
      logger.info("BootStrap notification received");
      BootstrapInform bootstrapNotification = (BootstrapInform) notification;

      DeviceOperationDetails deviceDetails =
          deviceOperDAO.findByDeviceId(notification.getDeviceDetails().getDeviceId());
      if (deviceDetails == null) {
        deviceDetails = new DeviceOperationDetails();
        deviceDetails.setDeviceId(notification.getDeviceDetails().getDeviceId());
        deviceDetails.setSwVersion(notification.getDeviceDetails().getSoftwareVersion());
        deviceOperDAO.save(deviceDetails);
      }

      checkForActivateNotification(notification);

      // send request to create the netconf server instance for the bootstrap device
      // id
      NetConfServerDetails serverInfo = createNtConfServer(bootstrapNotification);
      if (serverInfo == null)
        return;

      vesnotiSender.sendNotification(bootstrapNotification, serverInfo);
      BootstrapInform bsInform =
          getDeviceBootStrapNotification(bootstrapNotification, TR069InformType.BOOTSTRAP);
      ValueChangeInform vcInform = null;
      if (bootstrapNotification.getValueChangeNotification() != null) {
        logger.info("Bootstrap notification received along with VC");
        vcInform =
            getDeviceValueChangeNotification(bootstrapNotification, TR069InformType.VALUECHANGE);
        processVCNotification(vcInform, isAlarmVC);
      }
      notiSender.sendNotification(bsInform);
      if (vcInform != null)
        processVCNotification(vcInform, isAlarmVC);
    } else if (notification instanceof BootInform) {
      logger.info("Boot notification received");

      NetConfServerDetails serverInfo = createNtConfServer(notification);
      if (serverInfo == null)
        return;

      checkForActivateNotification(notification);
      BootInform bootNotification = (BootInform) notification;
      BootInform bInform = getDeviceBootNotification(bootNotification, TR069InformType.BOOT);
      ValueChangeInform vcInform = null;
      if (bootNotification.getValueChangeNotification() != null) {
        logger.info("Boot notification received along with VC");
        vcInform = getDeviceValueChangeNotification(bootNotification, TR069InformType.VALUECHANGE);
      }
      notiSender.sendNotification(bInform);
      processVCNotification(vcInform, isAlarmVC);
    } else if (notification instanceof PeriodicInform) {
      PeriodicInform pINotificaiton = (PeriodicInform) notification;
      vesnotiSender.sendNotification(pINotificaiton, null);
      notiSender.sendNotification(pINotificaiton);
      logger.info("PI notification received");
    } else if (notification instanceof ConnectionRequestInform) {
      ConnectionRequestInform crNotificaiton = (ConnectionRequestInform) notification;
      vesnotiSender.sendNotification(crNotificaiton, null);
      logger.info("ConnectionRequestInform notification received");
    } else if (notification instanceof ValueChangeInform) {
      ValueChangeInform valueChgNotificaiton = (ValueChangeInform) notification;
      processVCNotification(valueChgNotificaiton, isAlarmVC);
    } else if (notification instanceof TransferCompleteInform) {
      TransferCompleteInform tfNotificaiton = (TransferCompleteInform) notification;
      if (tfNotificaiton.getCommandKey() != null && tfNotificaiton.getCommandKey()
          .equalsIgnoreCase(tfNotificaiton.getDeviceDetails().getDeviceId())) {
        logger.debug("TransferCompleteInform is recevied at mapper");
        processTransferCompleteInform(tfNotificaiton);
        logger.debug("TransferCompleteInform processing completed at mapper");
      }
    }

    pnpPreProvisioningHandler.onDeviceNotification(notification);
  }

  private NetConfServerDetails createNtConfServer(DeviceInform inform) {
    String eNodeBName = pnpPreProvisioningHandler.getEnodeBName(
        inform.getDeviceDetails().getDeviceId(), inform.getDeviceDetails().getSoftwareVersion(),
        inform.getDeviceDetails().getHardwareVersion());
    if (eNodeBName == null)
      eNodeBName = inform.getDeviceDetails().getDeviceId();
    NetConfServerDetails serverInfo =
        netconfManager.createNetconfServer(inform.getDeviceDetails().getDeviceId(), eNodeBName,
            inform.getDeviceDetails().getSoftwareVersion(),
            inform.getDeviceDetails().getHardwareVersion());
    if (serverInfo != null && !NetconfServerManagementError.SUCCESS.equals(serverInfo.getError())) {
      logger.error("Failed to handle boot/bootstrap notification. Server INFO: {}", serverInfo);
      logger.error("Failed to create the netconf server for device ID: {}  Error: {}",
          inform.getDeviceDetails().getDeviceId(), serverInfo.getError());
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

  public List<ParameterDTO> filterUnsupportedParameters(List<ParameterDTO> parameters,
      String swVersion, String hwVersion) {
    List<ParameterDTO> result = new ArrayList<>();
    if (null != parameters) {
      for (ParameterDTO param : parameters) {
        MOMetaData metaData =
            metaDataUtil.getMetaDataByTR69Name(param.getParamName(), swVersion, hwVersion);
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

  private void processTransferCompleteInform(TransferCompleteInform notification) {

    try {
      ArrayList<ParameterDTO> paramList = new ArrayList<ParameterDTO>();
      DeviceOperationDetails fwDetails =
          deviceOperDAO.findByDeviceId(notification.getDeviceDetails().getDeviceId());
      if (fwDetails == null || fwDetails.getFileName() == null) {
        logger.debug(
            "TransferCompleteInform recevied for invaild device, there is no entry exist in the database");
        return;
      }
      if (fwDetails.getDownLoadStatus() == FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus()) {
        paramList.add(new ParameterDTO("download-event.file-name", fwDetails.getFileName()));

        String status = FirwareUpgradeErrorCode.getErrorCodeMapping(notification.getFaultCode());
        paramList.add(new ParameterDTO("download-event.status", status));
        if (notification.getFaultCode() != 0) {
          fwDetails.setDownLoadStatus(FirwareUpgradeStatus.DOWNLOAD_FAILED.getStatus());
          paramList
              .add(new ParameterDTO("download-event.error-message", notification.getFaultString()));
        } else {
          fwDetails.setDownLoadStatus(FirwareUpgradeStatus.DOWNLOAD_COMPLETED.getStatus());
          logger.debug("downloading file completed on the device successfully.");
        }
        deviceOperDAO.save(fwDetails);

        logger.debug("sending download-event notification to netconfserver");

        if (notiSender.sendCustomNotification(notification.getDeviceDetails().getDeviceId(),
            paramList, SOFT_MGMT_NS_URI).getStatusCode().is2xxSuccessful()) {
          logger.debug("sending download-event notification to netconfserver sucess");
        } else {
          logger.error("sending download-event notification to netconfserver failed");
        }
      } else {
        logger.debug(
            "TransferCompleteInform recevied after boot is received; already software is activated");
      }
    } catch (Exception e) {
      logger.debug("Exception occured while processing TransferCompleteInform " + e.toString());
    }
  }

  private void checkForActivateNotification(DeviceInform notification) {

    try {
      ArrayList<ParameterDTO> paramList = new ArrayList<ParameterDTO>();
      DeviceOperationDetails devDetails =
          deviceOperDAO.findByDeviceId(notification.getDeviceDetails().getDeviceId());

      if (devDetails == null
          || devDetails.getDownLoadStatus() == FirwareUpgradeStatus.NOT_STARTED.getStatus()) {
        logger.debug("firmware upgrade is not in progress");
        return;
      }

      if (!notification.getDeviceDetails().getSoftwareVersion()
          .equalsIgnoreCase(devDetails.getSwVersion())
          && devDetails.getDownLoadStatus() == FirwareUpgradeStatus.DOWNLOAD_INTIATED.getStatus()) {
        logger.debug("received the boot/bootstrap before the transfer complete recevied");
        TransferCompleteInform inform = new TransferCompleteInform();
        inform.setDeviceDetails(notification.getDeviceDetails());
        inform.setFaultCode(0);
        processTransferCompleteInform(inform);
      }

      devDetails = deviceOperDAO.findByDeviceId(notification.getDeviceDetails().getDeviceId());
      if (devDetails.getDownLoadStatus() == FirwareUpgradeStatus.DOWNLOAD_COMPLETED.getStatus()) {
        paramList.add(new ParameterDTO("activation-event.slot-name", "Active-Partion"));
        // check for software change
        if (notification.getDeviceDetails().getSoftwareVersion()
            .equalsIgnoreCase(devDetails.getSwVersion())) {
          paramList.add(new ParameterDTO("activation-event.status", "APPLICATION_ERROR"));
          paramList.add(new ParameterDTO("activation-event.error-message",
              "Same Software Version is reported after upgrade"));
          devDetails.setDownLoadStatus(FirwareUpgradeStatus.ACTIVATION_ERROR.getStatus());
        } else {
          devDetails.setSwVersion(notification.getDeviceDetails().getSoftwareVersion());
          devDetails.setDownLoadStatus(FirwareUpgradeStatus.ACTIVATION_COMPLETED.getStatus());
          paramList.add(new ParameterDTO("activation-event.status", "COMPLETED"));
        }
        deviceOperDAO.save(devDetails);

        logger.debug("sending activation-event notification to netconfserver");

        if (notiSender.sendCustomNotification(notification.getDeviceDetails().getDeviceId(),
            paramList, SOFT_MGMT_NS_URI).getStatusCode().is2xxSuccessful()) {
          logger.debug("sending activation-event notification to netconfserver sucess");
        } else {
          logger.error("sending activation-event notification to netconfserver failed");
        }
      }
    } catch (Exception e) {
      logger.debug(
          "Exception occured while processing ProcessFirmWareActivateNotification " + e.toString());
    }
  }
}
