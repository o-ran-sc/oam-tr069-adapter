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

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.ParameterAttributeDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.cpe.TR069RPC;
import org.commscope.tr069adapter.acs.cpe.rpc.AddObject;
import org.commscope.tr069adapter.acs.cpe.rpc.DeleteObject;
import org.commscope.tr069adapter.acs.cpe.rpc.Download;
import org.commscope.tr069adapter.acs.cpe.rpc.FactoryReset;
import org.commscope.tr069adapter.acs.cpe.rpc.GetParameterAttributes;
import org.commscope.tr069adapter.acs.cpe.rpc.GetParameterValues;
import org.commscope.tr069adapter.acs.cpe.rpc.Reboot;
import org.commscope.tr069adapter.acs.cpe.rpc.SetParameterAttributes;
import org.commscope.tr069adapter.acs.cpe.rpc.SetParameterValues;
import org.springframework.stereotype.Component;

@Component
public class DeviceRPCBuilder {

  /**
   * @param deviceRPCRequest
   * @return
   */


  public TR069RPC constructDeviceRPC(DeviceRPCRequest deviceRPCRequest) {
    TR069RPC message = null;

    OperationDetails opDetails = deviceRPCRequest.getOpDetails();
    if (null == opDetails || null == opDetails.getOpCode()) {
      return null;
    }

    OperationCode opCode = opDetails.getOpCode();
    if (opCode.equals(TR069OperationCode.SET_PARAMETER_VALUES)) {
      message = buildSetParameterValues(opDetails);
    } else if (opCode.equals(TR069OperationCode.GET_PARAMETER_VALUES)) {
      message = buildGetParameterValues(opDetails);
    } else if (opCode.equals(TR069OperationCode.SET_PARAMETER_ATTRIBUTES)) {
      message = buildSetParameterAttributes(opDetails);
    } else if (opCode.equals(TR069OperationCode.GET_PARAMETER_ATTRIBUTES)) {
      message = buildGetParameterAttributes(opDetails);
    } else if (opCode.equals(TR069OperationCode.ADD_OBJECT)) {
      List<ParameterDTO> poarameterDTOs = opDetails.getParmeters();
      for (ParameterDTO param : poarameterDTOs) {
        AddObject addObject = new AddObject(param.getParamName(), String.valueOf(hashCode()));
        message = addObject;
      }
    } else if (opCode.equals(TR069OperationCode.DELETE_OBJECT)) {
      List<ParameterDTO> poarameterDTOs = opDetails.getParmeters();
      for (ParameterDTO param : poarameterDTOs) {
        DeleteObject deleteObject =
            new DeleteObject(param.getParamName(), String.valueOf(hashCode()));
        message = deleteObject;
      }
    } else if (opCode.equals(TR069OperationCode.DOWNLOAD)) {
      String deviceId = deviceRPCRequest.getDeviceDetails().getDeviceId();
      List<ParameterDTO> poarameterDTOs = opDetails.getParmeters();
      message = populateDownloadParams(deviceId, poarameterDTOs);
    } else if (opCode.equals(TR069OperationCode.FACTORY_RESET)) {
      message = new FactoryReset();
    } else if (opCode.equals(TR069OperationCode.REBOOT)) {
      message = new Reboot();
    }

    return message;
  }

  /**
   * @param poarameterDTOs
   * @return
   */
  private Download populateDownloadParams(String deviceId, List<ParameterDTO> poarameterDTOs) {
    Download download = new Download();
    for (ParameterDTO param : poarameterDTOs) {
      if (param.getParamName().equals("FileType")) {
        download.setFileType(param.getParamValue());
      } else if (param.getParamName().equals("URL")) {
        download.setUrl(param.getParamValue());
      } else if (param.getParamName().equals("Username")) {
        download.setUserName(param.getParamValue());
      } else if (param.getParamName().equals("Password")) {
        download.setPassword(param.getParamValue());
      } else if (param.getParamName().equals("FileSize")) {
        download.setFileSize(Long.parseLong(param.getParamValue()));
      } else if (param.getParamName().equals("TargetFileName")) {
        download.setTargetFileName(param.getParamValue());
      } else if (param.getParamName().equals("DelaySeconds")) {
        download.setDelaySeconds(Integer.parseInt(param.getParamValue()));
      } else if (param.getParamName().equals("SuccessURL")) {
        download.setSuccessUrl(param.getParamValue());
      } else if (param.getParamName().equals("FailureURL")) {
        download.setFailureUrl(param.getParamValue());
      }
    }
    download.setCommandKey(String.valueOf(deviceId));

    return download;
  }

  private SetParameterValues buildSetParameterValues(OperationDetails opDetails) {
    SetParameterValues spv = new SetParameterValues();
    List<ParameterDTO> parmeters = opDetails.getParmeters();
    for (ParameterDTO param : parmeters) {
      spv.addValue(param.getParamName(), param.getParamValue(), param.getDataType());
    }

    return spv;
  }

  private GetParameterValues buildGetParameterValues(OperationDetails opDetails) {
    GetParameterValues gpv = new GetParameterValues();
    List<ParameterDTO> parmeters = opDetails.getParmeters();
    List<String> paramNamelist = new ArrayList<>();
    for (ParameterDTO param : parmeters) {
      paramNamelist.add(param.getParamName());
    }
    String[] parameterNames = new String[paramNamelist.size()];
    for (int i = 0; i < paramNamelist.size(); i++) {
      parameterNames[i] = paramNamelist.get(i);
    }
    gpv.setParameterNames(parameterNames);
    return gpv;
  }

  private SetParameterAttributes buildSetParameterAttributes(OperationDetails opDetails) {
    SetParameterAttributes spa = new SetParameterAttributes();
    List<ParameterDTO> parameterAttributes = opDetails.getParmeters();
    for (ParameterDTO parameterDTO : parameterAttributes) {
      ParameterAttributeDTO parameterAttributeDTO = (ParameterAttributeDTO) parameterDTO;
      spa.addAttribute(parameterAttributeDTO.getParamName(),
          parameterAttributeDTO.getNotificationChange(), parameterAttributeDTO.getNotification(),
          parameterAttributeDTO.getAccesslistChange(), parameterAttributeDTO.getAccessList());
    }
    return spa;
  }

  private GetParameterAttributes buildGetParameterAttributes(OperationDetails opDetails) {
    GetParameterAttributes gpa = new GetParameterAttributes();
    List<ParameterDTO> poarameterDTOs = opDetails.getParmeters();
    List<String> paramNamelist = new ArrayList<>();
    for (ParameterDTO param : poarameterDTOs) {
      paramNamelist.add(param.getParamName());
    }
    String[] parameterNames = new String[paramNamelist.size()];
    for (int i = 0; i < paramNamelist.size(); i++) {
      parameterNames[i] = paramNamelist.get(i);
    }
    gpa.setParameterNames(parameterNames);

    return gpa;
  }
}
