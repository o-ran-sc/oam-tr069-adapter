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

package org.commscope.tr069adapter.acs.cpe.test;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceDetails;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.OperationOptions;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionState;
import org.commscope.tr069adapter.acs.requestprocessor.entity.SessionManagerEntity;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceEntity;

public class RequestProcessorTestsUtils {

  public static final String SERVER_URI = "http://localhost:9000/";
  public static final String macId = "0005B95196D0";
  public static final String OUI = "0005B9";
  public static final String PRODUCT_CLASS = "LTE_Enterprise_C-RANSC_Cntrl";
  public static final Long DEVICE_TYPE_ID = 10L;
  public static final String HAEDWARE_VERSION = "1.1.1.1";
  public static final String SW_VERSION = "4.5.00.001";

  public static final String user = OUI + "-" + PRODUCT_CLASS + "-" + macId;
  public static final String password = "airvana";
  public static final String connectionReqURL =
      "http://10.211.55.33:45076/ConnectionRequest?command=cr&sn=0005B95196D0";


  public static final String executionTimeout = "60000";



  public static SessionManagerEntity getSessionManagerEntity(SessionState sessionState) {
    SessionManagerEntity sessionManagerEntity = new SessionManagerEntity();

    sessionManagerEntity.setDeviceId(macId);
    sessionManagerEntity.setCurrentOperationId(10L);

    sessionManagerEntity.setState(sessionState.getValue());

    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.MINUTE, -2);

    sessionManagerEntity.setSessionStartTime(calendar.getTime());

    return sessionManagerEntity;
  }

  public static SessionManagerEntity getSessionManagerEntity() {
    SessionManagerEntity sessionManagerEntity = new SessionManagerEntity();

    sessionManagerEntity.setDeviceId(macId);
    sessionManagerEntity.setCurrentOperationId(10L);

    sessionManagerEntity.setState(SessionState.PROCESSING.getValue());

    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.MINUTE, -2);

    sessionManagerEntity.setSessionStartTime(calendar.getTime());

    return sessionManagerEntity;
  }


  public static TR069DeviceEntity getTR069DeviceEntity() {
    TR069DeviceEntity tr069DeviceEntity = new TR069DeviceEntity();

    tr069DeviceEntity.setDeviceId(macId);
    tr069DeviceEntity.setHwVersion(HAEDWARE_VERSION);
    tr069DeviceEntity.setSwVersion(SW_VERSION);
    tr069DeviceEntity.setPassword(password);
    tr069DeviceEntity.setConnectionReqURL(connectionReqURL);
    tr069DeviceEntity.setUserName(user);
    tr069DeviceEntity.setLastUpdatedTime(new Date());
    return tr069DeviceEntity;
  }

  public static OperationOptions getOperationOptions(Long executionTimeout) {
    OperationOptions operationOptions = new OperationOptions();
    operationOptions.setExecutionTimeout(executionTimeout);
    return operationOptions;
  }


  public static DeviceDetails getDeviceDetails() {
    DeviceDetails deviceDetails = new DeviceDetails();
    deviceDetails.setDeviceId(macId);
    deviceDetails.setDeviceTypeId(DEVICE_TYPE_ID);
    deviceDetails.setOui(OUI);
    deviceDetails.setProductClass(PRODUCT_CLASS);
    return deviceDetails;
  }


  public static OperationDetails getOperationDetails(OperationCode opCode) {
    OperationDetails operationDetails = new OperationDetails();
    operationDetails.setOpCode(opCode);
    operationDetails.setParmeters(getParameterDTO());
    return operationDetails;
  }

  public static List<ParameterDTO> getParameterDTO() {
    List<ParameterDTO> paramDTOList = new ArrayList<ParameterDTO>();

    ParameterDTO parameterDTO = new ParameterDTO();
    parameterDTO.setDataType("string");
    parameterDTO.setParamName("Device.Services.FAPService.1.CellConfig.LTE.RAN.RF.PhyCellID");
    parameterDTO.setParamValue("10");
    parameterDTO.setProcessed(false);

    ParameterDTO parameterDTO1 = new ParameterDTO();
    parameterDTO.setDataType("string");
    parameterDTO.setParamName("Device.Services.FAPService.1.CellConfig.LTE.RAN.RF.DLBandwidth");
    parameterDTO.setParamValue("15");
    parameterDTO.setProcessed(false);

    paramDTOList.add(parameterDTO);
    paramDTOList.add(parameterDTO1);

    return paramDTOList;
  }
}
