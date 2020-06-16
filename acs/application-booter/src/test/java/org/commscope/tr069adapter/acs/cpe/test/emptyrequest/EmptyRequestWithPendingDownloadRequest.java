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

public class EmptyRequestWithPendingDownloadRequest extends EmptyRequestWithPendingRequest {

  @Override
  public DeviceRPCRequest getDeviceRPCRequest(TR069DeviceDetails tr069DeviceDetails) {
    List<ParameterDTO> parameters = new ArrayList<>();
    ParameterDTO parameterDTO = new ParameterDTO();
    parameterDTO.setParamName("FileType");
    parameterDTO.setParamValue("xyz");
    parameters.add(parameterDTO);

    ParameterDTO parameterDTO1 = new ParameterDTO();
    parameterDTO1.setParamName("URL");
    parameterDTO1.setParamValue("xyz");
    parameters.add(parameterDTO1);

    ParameterDTO parameterDTO2 = new ParameterDTO();
    parameterDTO2.setParamName("Username");
    parameterDTO2.setParamValue("xyz");
    parameters.add(parameterDTO2);

    ParameterDTO parameterDTO3 = new ParameterDTO();
    parameterDTO3.setParamName("Password");
    parameterDTO3.setParamValue("xyz");
    parameters.add(parameterDTO3);

    ParameterDTO parameterDTO4 = new ParameterDTO();
    parameterDTO4.setParamName("FileSize");
    parameterDTO4.setParamValue("872362893480");
    parameters.add(parameterDTO4);


    ParameterDTO parameterDTO5 = new ParameterDTO();
    parameterDTO5.setParamName("TargetFileName");
    parameterDTO5.setParamValue("xyz");
    parameters.add(parameterDTO5);

    ParameterDTO parameterDTO6 = new ParameterDTO();
    parameterDTO6.setParamName("DelaySeconds");
    parameterDTO6.setParamValue("12");
    parameters.add(parameterDTO6);

    ParameterDTO parameterDTO7 = new ParameterDTO();
    parameterDTO7.setParamName("SuccessURL");
    parameterDTO7.setParamValue("xyz");
    parameters.add(parameterDTO7);

    ParameterDTO parameterDTO8 = new ParameterDTO();
    parameterDTO8.setParamName("FailureURL");
    parameterDTO8.setParamValue("xyz");
    parameters.add(parameterDTO8);


    OperationDetails opDetails = new OperationDetails();
    opDetails.setOpCode(TR069OperationCode.DOWNLOAD);
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
