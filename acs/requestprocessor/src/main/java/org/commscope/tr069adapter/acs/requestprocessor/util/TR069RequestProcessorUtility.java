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


package org.commscope.tr069adapter.acs.requestprocessor.util;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.DEVICE_RPC_EXECUTION_TIMEOUT_SECONDS;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.OperationCode;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.OperationOptions;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.CustomOperationCode;
import org.commscope.tr069adapter.acs.common.dto.ParameterAttributeDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceRPCRequestEntity;

public class TR069RequestProcessorUtility {

  protected static final int BLOCK_SIZE = 3999;
  protected static final int MAX_SIZE = 11998;

  private TR069RequestProcessorUtility() {
    super();
  }

  /**
   * Converts entity object to DTO
   * 
   * @param entityList
   * @return
   */
  public static DeviceRPCRequest convertToDTO(List<TR069DeviceRPCRequestEntity> entityList) {
    DeviceRPCRequest deviceRPCRequest = new DeviceRPCRequest();
    OperationDetails operationDetails = new TR069OperationDetails();

    boolean isCustomOperation = true;
    OperationCode opCode = null;

    TR069DeviceRPCRequestEntity entity = entityList.get(0);
    if (CustomOperationCode.getByOperationCode(entity.getOpCode()) != null) {
      opCode = CustomOperationCode.getByOperationCode(entity.getOpCode());
    } else {
      opCode = TR069OperationCode.getByOperationCode(entity.getOpCode());
      isCustomOperation = false;
    }

    operationDetails.setOpCode(opCode);
    dtoFromEntityJson(operationDetails, entityList, isCustomOperation);
    deviceRPCRequest.setOpDetails(operationDetails);

    OperationOptions opOptions = new OperationOptions();
    opOptions.setExecutionTimeout(entity.getRequestTimeOut());
    deviceRPCRequest.setOptions(opOptions);

    TR069DeviceDetails deviceDetails = new TR069DeviceDetails();
    deviceDetails.setDeviceId(entity.getDeviceId());

    deviceRPCRequest.setDeviceDetails(deviceDetails);
    deviceRPCRequest.setOperationId(entity.getOperationId());
    return deviceRPCRequest;
  }

  /**
   * Converts DTO to entity object
   * 
   * @param deviceRPCRequest
   * @return
   * @throws TR069EventProcessingException
   */
  public static List<TR069DeviceRPCRequestEntity> convertToEntity(DeviceRPCRequest deviceRPCRequest)
      throws TR069EventProcessingException {

    if (deviceRPCRequest.getOpDetails() == null
        || deviceRPCRequest.getOpDetails().getOpCode() == null) {
      throw new TR069EventProcessingException(ErrorCode.MISSING_OPERATION_DETAILS);
    }

    Integer opCode = null;
    boolean isCustomOperation = true;
    OperationCode operationCode = deviceRPCRequest.getOpDetails().getOpCode();
    if (CustomOperationCode.getByOperationCode(operationCode.getOperationCode()) != null) {
      CustomOperationCode customOperationCode =
          CustomOperationCode.getByOperationCode(operationCode.getOperationCode());
      opCode = customOperationCode.getOperationCode();
    } else {
      TR069OperationCode tr069OperationCode =
          (TR069OperationCode) deviceRPCRequest.getOpDetails().getOpCode();
      opCode = tr069OperationCode.getOperationCode();
      isCustomOperation = false;
    }

    String paramListJSON = paramListToJson(deviceRPCRequest, isCustomOperation);
    List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntities =
        getTR069DeviceRPCRequestEntities(paramListJSON);

    for (TR069DeviceRPCRequestEntity entity : tr069DeviceRPCRequestEntities) {
      entity.setOpCode(opCode);
      entity.setCreateTime(new Date());
      entity.setDeviceId(deviceRPCRequest.getDeviceDetails().getDeviceId());
      entity.setOperationId(deviceRPCRequest.getOperationId());
      entity.setIsProcessed(0);
      if (deviceRPCRequest.getOptions() != null
          && deviceRPCRequest.getOptions().getExecutionTimeout() != 0L) {
        entity.setRequestTimeOut(deviceRPCRequest.getOptions().getExecutionTimeout());
      } else {
        entity.setRequestTimeOut(DEVICE_RPC_EXECUTION_TIMEOUT_SECONDS);
        OperationOptions options = new OperationOptions();
        options.setExecutionTimeout(DEVICE_RPC_EXECUTION_TIMEOUT_SECONDS);
        deviceRPCRequest.setOptions(options);
      }
    }

    return tr069DeviceRPCRequestEntities;
  }

  /**
   * @param entity
   * @param dto
   * @param isCustomOperation
   */
  private static String paramListToJson(DeviceRPCRequest dto, boolean isCustomOperation) {
    String attrJsonString = null;
    if (isCustomOperation) {
      TR069OperationDetails operationDetails = (TR069OperationDetails) dto.getOpDetails();
      StringBuilder buffer = new StringBuilder();

      List<ParameterDTO> deletParamList = operationDetails.getDeleteParamList();
      List<ParameterDTO> modifyParamList = operationDetails.getModifyParamList();
      List<ParameterDTO> setParamList = operationDetails.getSetParamList();

      if (deletParamList != null && !deletParamList.isEmpty()) {
        convertBooleanValues(deletParamList);
        buffer.append(toJson(deletParamList));
      } else {
        buffer.append(toJson(new ArrayList<>()));
      }

      if (modifyParamList != null && !modifyParamList.isEmpty()) {
        convertBooleanValues(modifyParamList);
        buffer.append(toJson(modifyParamList));
      } else {
        buffer.append(toJson(new ArrayList<>()));
      }

      if (setParamList != null && !setParamList.isEmpty()) {
        convertBooleanValues(setParamList);
        buffer.append(toJson(setParamList));
      } else {
        buffer.append(toJson(new ArrayList<>()));
      }

      attrJsonString = buffer.toString();
    } else {
      List<ParameterDTO> parameterDTOs = dto.getOpDetails().getParmeters();
      attrJsonString = toJson(parameterDTOs);
    }

    return attrJsonString;
  }

  /**
   * @param paramListJSON
   * @return
   */
  private static List<TR069DeviceRPCRequestEntity> getTR069DeviceRPCRequestEntities(
      String paramListJSON) {
    List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntities = new ArrayList<>();

    if (paramListJSON.length() < MAX_SIZE) {
      TR069DeviceRPCRequestEntity entity = new TR069DeviceRPCRequestEntity();
      entity.setAttributeJson1(getAttriuteJsonData(paramListJSON, 0));
      entity.setAttributeJson2(getAttriuteJsonData(paramListJSON, 1));
      entity.setAttributeJson3(getAttriuteJsonData(paramListJSON, 2));

      tr069DeviceRPCRequestEntities.add(entity);
    } else {
      int noOfEntities = 0;
      while (true) {
        int i = 3 * noOfEntities;
        TR069DeviceRPCRequestEntity entity = new TR069DeviceRPCRequestEntity();
        entity.setAttributeJson1(getAttriuteJsonData(paramListJSON, i));
        entity.setAttributeJson2(getAttriuteJsonData(paramListJSON, ++i));
        entity.setAttributeJson3(getAttriuteJsonData(paramListJSON, ++i));

        if (entity.getAttributeJson1() != null) {
          tr069DeviceRPCRequestEntities.add(entity);
        }

        if (entity.getAttributeJson1() == null || entity.getAttributeJson2() == null
            || entity.getAttributeJson3() == null) {
          break;
        }

        noOfEntities++;
      }

    }
    return tr069DeviceRPCRequestEntities;
  }

  /**
   * @param eventData
   * @param blockNum
   * @return
   */
  private static String getAttriuteJsonData(String eventData, int blockNum) {
    int eventDataSize = eventData.length();
    int startIndex = blockNum * BLOCK_SIZE;
    if (startIndex > eventDataSize) {
      return null;
    }
    int endIndex = startIndex + BLOCK_SIZE;
    endIndex = endIndex < eventDataSize ? endIndex : eventDataSize;
    return eventData.substring(startIndex, endIndex);
  }

  /**
   * @param opDetails
   * @param entity
   * @param isCustomOperation
   */
  private static void dtoFromEntityJson(OperationDetails opDetails,
      List<TR069DeviceRPCRequestEntity> entityList, boolean isCustomOperation) {
    StringBuilder sb = new StringBuilder();
    for (TR069DeviceRPCRequestEntity entity : entityList) {
      append(sb, entity.getAttributeJson1());
      append(sb, entity.getAttributeJson2());
      append(sb, entity.getAttributeJson3());
    }

    if (isCustomOperation) {
      String[] splitStringArray = sb.toString().split("]\\[");
      List<ParameterDTO> deleteParamList = new ArrayList<>();
      List<ParameterDTO> modifyParamList = new ArrayList<>();
      List<ParameterDTO> setParamList = new ArrayList<>();
      for (int i = 0; i < splitStringArray.length; i++) {
        String data = splitStringArray[i];
        if (i == 0) {
          data = data + "]";
          deleteParamList.addAll(fromJson(data));
        } else if ((i + 1) == splitStringArray.length) {
          data = "[" + data;
          setParamList.addAll(fromJson(data));
        } else {
          data = "[" + data + "]";
          modifyParamList.addAll(fromJson(data));
        }
      }
      TR069OperationDetails tr069OperationDetails = (TR069OperationDetails) opDetails;
      tr069OperationDetails.setDeleteParamList(deleteParamList);
      tr069OperationDetails.setModifyParamList(modifyParamList);
      tr069OperationDetails.setSetParamList(setParamList);
    } else {
      if (TR069OperationCode.SET_PARAMETER_ATTRIBUTES.equals(opDetails.getOpCode())) {
        List<ParameterDTO> list = fromJsonToParameterAttribute(sb.toString());
        opDetails.setParmeters(list);
      } else {
        List<ParameterDTO> list = fromJson(sb.toString());
        opDetails.setParmeters(list);
      }
    }
  }

  /**
   * @param sb
   * @param temp
   */
  private static void append(StringBuilder sb, String temp) {
    if (temp != null && !temp.isEmpty()) {
      sb.append(temp);
    }
  }

  /**
   * @param jsonString
   * @return
   */
  private static List<ParameterDTO> fromJson(String jsonString) {
    Gson gson = new Gson();
    Type collectionType = new TypeToken<List<ParameterDTO>>() {}.getType();
    return gson.fromJson(jsonString, collectionType);
  }

  /**
   * @param jsonString
   * @return
   */
  private static List<ParameterDTO> fromJsonToParameterAttribute(String jsonString) {
    Gson gson = new Gson();
    Type collectionType = new TypeToken<List<ParameterAttributeDTO>>() {}.getType();
    return gson.fromJson(jsonString, collectionType);
  }

  /**
   * @param entity
   * @return
   */
  private static <T> String toJson(T entity) {
    Gson gson = new Gson();
    return gson.toJson(entity);
  }

  /**
   * @param parameterDTOs
   */
  private static void convertBooleanValues(List<ParameterDTO> parameterDTOs) {
    for (ParameterDTO param : parameterDTOs) {
      if (param.getDataType() != null && param.getDataType().equalsIgnoreCase("boolean")) {
        if (param.getParamValue() != null && (param.getParamValue().equalsIgnoreCase("true")
            || param.getParamValue().equalsIgnoreCase("1"))) {
          param.setParamValue("1");
        } else {
          param.setParamValue("0");
        }
      }
    }
  }

}
