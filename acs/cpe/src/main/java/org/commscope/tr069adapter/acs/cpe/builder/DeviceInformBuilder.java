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

package org.commscope.tr069adapter.acs.cpe.builder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.commscope.tr069adapter.acs.common.InformType;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069InformType;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.common.inform.AbstractDeviceInform;
import org.commscope.tr069adapter.acs.common.inform.BootInform;
import org.commscope.tr069adapter.acs.common.inform.BootstrapInform;
import org.commscope.tr069adapter.acs.common.inform.ConnectionRequestInform;
import org.commscope.tr069adapter.acs.common.inform.PeriodicInform;
import org.commscope.tr069adapter.acs.common.inform.TransferCompleteInform;
import org.commscope.tr069adapter.acs.common.inform.ValueChangeInform;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.cpe.rpc.Inform;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class DeviceInformBuilder {

  private static final Logger logger = LoggerFactory.getLogger(DeviceInformBuilder.class);

  /**
   * @param inform
   * @return
   * @throws TR069EventProcessingException
   */
  public AbstractDeviceInform constructDeviceInform(Inform inform)
      throws TR069EventProcessingException {

    AbstractDeviceInform deviceInform = null;

    try {
      TR069DeviceDetails deviceDetails = buildDeviceDetailsFromInform(inform);

      List<InformType> informTypeList = new ArrayList<>();
      for (Entry<String, String> obj : inform.getEvents()) {
        if (null != TR069InformType.getTR069NotificationType(obj.getKey())) {
          informTypeList.add(TR069InformType.getTR069NotificationType(obj.getKey()));
        } else {
          logger.error("Invalid Event code: {}", obj.getValue());
        }
      }

      logger.debug("Building Device Inform event based on the event codes sent by the device");

      if (informTypeList.contains(TR069InformType.BOOTSTRAP)) {
        logger.debug("Constructing BOOTSTRAP Inform");
        BootstrapInform bootstrapInform = new BootstrapInform();
        bootstrapInform.setDeviceDetails(deviceDetails);
        bootstrapInform.setInformTypeList(informTypeList);
        bootstrapInform.setParameters(getParameterList(inform.getParams()));
        if (informTypeList.contains(TR069InformType.VALUECHANGE)) {
          ValueChangeInform vcNotification = new ValueChangeInform();
          bootstrapInform.setValueChangeNotification(vcNotification);
        }
        deviceInform = bootstrapInform;
      } else if (informTypeList.contains(TR069InformType.BOOT)) {
        logger.debug("Constructing BOOT Inform");
        BootInform bootInform = new BootInform();
        bootInform.setDeviceDetails(deviceDetails);
        bootInform.setInformTypeList(informTypeList);
        bootInform.setParameters(getParameterList(inform.getParams()));
        if (informTypeList.contains(TR069InformType.VALUECHANGE)) {
          ValueChangeInform vcNotification = new ValueChangeInform();
          bootInform.setValueChangeNotification(vcNotification);
        }
        deviceInform = bootInform;
      } else if (informTypeList.contains(TR069InformType.PERIODIC)) {
        logger.debug("Constructing PERIODIC Inform");
        PeriodicInform periodicInform = new PeriodicInform();
        periodicInform.setDeviceDetails(deviceDetails);
        periodicInform.setInformTypeList(informTypeList);
        periodicInform.setParameters(getParameterList(inform.getParams()));
        deviceInform = periodicInform;
      } else if (informTypeList.contains(TR069InformType.VALUECHANGE)) {
        logger.debug("Constructing VALUECHANGE Inform");
        ValueChangeInform valueChangeInform = new ValueChangeInform();
        valueChangeInform.setDeviceDetails(deviceDetails);
        valueChangeInform.setInformTypeList(informTypeList);
        valueChangeInform.setParameters(getParameterList(inform.getParams()));
        deviceInform = valueChangeInform;
      } else if (informTypeList.contains(TR069InformType.TRANSFER_COMPLETE)) {
        logger.debug("Constructing Transfer Complete Inform");
        TransferCompleteInform transferCompleteInform = new TransferCompleteInform();
        transferCompleteInform.setDeviceDetails(deviceDetails);
        transferCompleteInform.setInformTypeList(informTypeList);
        transferCompleteInform.setParameters(getParameterList(inform.getParams()));
        deviceInform = transferCompleteInform;
      } else if (informTypeList.contains(TR069InformType.CONNECTIONREQUEST)) {
        logger.debug("Constructing Connection Request Inform");
        ConnectionRequestInform connectionRequestInform = new ConnectionRequestInform();
        connectionRequestInform.setDeviceDetails(deviceDetails);
        connectionRequestInform.setInformTypeList(informTypeList);
        connectionRequestInform.setParameters(getParameterList(inform.getParams()));
        deviceInform = connectionRequestInform;
      }
    } catch (Exception e) {
      throw new TR069EventProcessingException(ErrorCode.FAILED_PROCESSING_INFORM, e.getMessage());
    }
    return deviceInform;
  }

  /**
   * @param inform
   * @return
   */
  private TR069DeviceDetails buildDeviceDetailsFromInform(Inform inform) {
    TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();
    tr069DeviceDetails.setDeviceId(inform.getSn());
    tr069DeviceDetails.setSoftwareVersion(inform.getSoftwareVersion());
    tr069DeviceDetails.setHardwareVersion(inform.getHardwareVersion());
    tr069DeviceDetails.setOui(inform.getOui());
    tr069DeviceDetails.setProductClass(inform.getProductClass());
    tr069DeviceDetails.setConnectionRequestURL(inform.getURL());
    tr069DeviceDetails.setUsername(inform.getConreqUser());
    tr069DeviceDetails.setPassword(inform.getConreqPass());

    return tr069DeviceDetails;
  }

  /**
   * @param parameterList
   * @return
   */
  private List<ParameterDTO> getParameterList(Map<String, String> parameterList) {
    List<ParameterDTO> parameterDTOList = new ArrayList<>();
    Set<String> keys = parameterList.keySet();
    for (String key : keys) {
      ParameterDTO param = new ParameterDTO();
      param.setParamName(key);
      param.setParamValue(parameterList.get(key));
      parameterDTOList.add(param);
    }

    return parameterDTOList;
  }
}
