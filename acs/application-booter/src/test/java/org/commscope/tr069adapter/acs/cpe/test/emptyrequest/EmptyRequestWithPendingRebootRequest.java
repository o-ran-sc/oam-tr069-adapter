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

package org.commscope.tr069adapter.acs.cpe.test.emptyrequest;

import java.util.ArrayList;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.OperationOptions;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;

public class EmptyRequestWithPendingRebootRequest extends EmptyRequestWithPendingRequest {

  @Override
  public DeviceRPCRequest getDeviceRPCRequest(TR069DeviceDetails tr069DeviceDetails) {
    ParameterDTO parameterDTO = new ParameterDTO();
    parameterDTO.setParamName("Device.ManagementServer.PeriodicInformEnable");
    List<ParameterDTO> parameters = new ArrayList<>();
    parameters.add(parameterDTO);
    OperationDetails opDetails = new OperationDetails();
    opDetails.setOpCode(TR069OperationCode.REBOOT);
    opDetails.setParmeters(parameters);
    OperationOptions options = new OperationOptions();
    options.setExecutionTimeout(300000L);
    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    deviceRPCRequest.setDeviceDetails(tr069DeviceDetails);
    deviceRPCRequest.setOperationId(1000L);
    deviceRPCRequest.setOpDetails(opDetails);
    deviceRPCRequest.setOptions(options);

    return deviceRPCRequest;
  }
}
