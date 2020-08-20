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

package org.commscope.tr069adapter.acs.requestprocessor.custom;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.NUMBER_REGEX;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.SUCCESS;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationDetails;
import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.common.response.AddObjectResponse;
import org.commscope.tr069adapter.acs.common.response.DeleteObjectResponse;
import org.commscope.tr069adapter.acs.common.response.GetParameterValueResponse;
import org.commscope.tr069adapter.acs.common.response.SetParameterValueResponse;
import org.commscope.tr069adapter.acs.requestprocessor.dao.DeviceRPCRequestRepositoryHelper;
import org.commscope.tr069adapter.acs.requestprocessor.dto.CustomOperationData;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceRPCRequestEntity;
import org.commscope.tr069adapter.acs.requestprocessor.impl.TR069RequestProcessEngine;
import org.commscope.tr069adapter.acs.requestprocessor.util.TR069RequestProcessorUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("ConfigureMultipleObject")
public class ConfigureMultipleObject implements CustomOperation {

  private static final Logger logger = LoggerFactory.getLogger(ConfigureMultipleObject.class);

  @Autowired
  TR069RequestProcessEngine tr069ProcessEngine;

  @Autowired
  protected DeviceRPCRequestRepositoryHelper deviceRPCRequestRepositoryHelper;

  public CustomOperationData executeCustomLogic(CustomOperationData customOperationData)
      throws TR069EventProcessingException {

    TR069DeviceDetails deviceDetails = customOperationData.getDeviceDetails();
    DeviceRPCResponse deviceRPCResponse = customOperationData.getDeviceRPCResponse();
    DeviceRPCRequest nbiDeviceOperationRequest = customOperationData.getDeviceRPCRequest();

    logger.debug("Started processing Configure multiple object");
    DeviceRPCRequest operRequest = null;
    Long responseOperationId = null;
    if (deviceRPCResponse != null && deviceRPCResponse.getOperationId() != null) {
      responseOperationId = deviceRPCResponse.getOperationId();
      if (deviceRPCResponse.getFaultKey() != null && responseOperationId != null
          && responseOperationId.equals(nbiDeviceOperationRequest.getOperationId())) {
        logger.error("The Configure Multiple Object operation has failed, Reason: {}",
            deviceRPCResponse.getFaultString());

        logger.debug(
            "Deleting the NBI operation request for custom operation configureMultipleObjects with operation ID: {}",
            responseOperationId);
        List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntityList =
            deviceRPCRequestRepositoryHelper
                .findByDeviceIdAndOperationId(deviceDetails.getDeviceId(), responseOperationId);
        for (TR069DeviceRPCRequestEntity tr069DeviceRPCRequestEntity : tr069DeviceRPCRequestEntityList) {
          tr069DeviceRPCRequestEntity.setIsProcessed(Integer.valueOf(1));
        }
        deviceRPCRequestRepositoryHelper.saveAll(tr069DeviceRPCRequestEntityList);

        OperationResponse operationResponse = new GetParameterValueResponse();
        operationResponse.setParameterDTOs(new ArrayList<>());
        operationResponse.setStatus(1);
        deviceRPCResponse.setOperationResponse(operationResponse);

        customOperationData.setDeviceRPCResponse(deviceRPCResponse);
        customOperationData.setDeviceRPCRequest(null);
        logger.debug("Finished processing Configure multiple object");
        return customOperationData;
      }
    }

    TR069OperationDetails tr069OperationDetails =
        (TR069OperationDetails) nbiDeviceOperationRequest.getOpDetails();
    List<ParameterDTO> tr069deleteParamList = tr069OperationDetails.getDeleteParamList();
    List<ParameterDTO> tr069modifyParamList = tr069OperationDetails.getModifyParamList();
    List<ParameterDTO> tr069setParamList = tr069OperationDetails.getSetParamList();

    OperationOrder nextOperation = null;
    boolean isPendingOperationExists = true;
    final String NXT_OPERATION = "Next operation to be executed is : ";

    if ((responseOperationId == null) || (responseOperationId != null
        && !responseOperationId.equals(nbiDeviceOperationRequest.getOperationId()))) {
      // Must be called from Empty HTTP request, First operation to be called

      // Must be called from a different user operation, First operation to be called
      // if response operation id is different from current
      // nbiDevOperRequest operation id then first step is deleteObject

      nextOperation = OperationOrder.DELETE_OBJECT;
      logger.debug(NXT_OPERATION, nextOperation);
    } else {
      // Since the responseOperation is not null and equivalent, find the
      // response type and the take next action
      OperationResponse opResponse = deviceRPCResponse.getOperationResponse();
      if (opResponse instanceof DeleteObjectResponse) {
        logger.debug("Received delete object response");
        if (null != tr069deleteParamList && !tr069deleteParamList.isEmpty()) {
          int i = 0;
          for (ParameterDTO deleteParam : tr069deleteParamList) {
            i++;
            if (!deleteParam.isProcessed()) {
              deleteParam.setProcessed(true);
            } else {
              continue;
            }
            logger.debug("Persisting the NBI request for deleteObject");
            // Update the existing NBI request
            List<TR069DeviceRPCRequestEntity> entityList =
                deviceRPCRequestRepositoryHelper.findByDeviceIdAndOperationId(
                    deviceDetails.getDeviceId(), nbiDeviceOperationRequest.getOperationId());
            List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntityList =
                TR069RequestProcessorUtility.convertToEntity(nbiDeviceOperationRequest);
            for (int j = 0; j < entityList.size(); j++) {
              tr069DeviceRPCRequestEntityList.get(j).setId(entityList.get(j).getId());
            }
            deviceRPCRequestRepositoryHelper.saveAll(tr069DeviceRPCRequestEntityList);

            if (tr069deleteParamList.size() > i) {
              nextOperation = OperationOrder.DELETE_OBJECT;
              logger.debug(NXT_OPERATION, nextOperation);
              break;
            } else {
              nextOperation = OperationOrder.ADD_OBJECT;
              logger.debug(NXT_OPERATION, nextOperation);
            }
          }
        } else {
          nextOperation = OperationOrder.ADD_OBJECT;
          logger.debug(NXT_OPERATION, nextOperation);
        }
      } else if (opResponse instanceof AddObjectResponse) {
        logger.debug("Received Add object response");
        if (null != tr069setParamList && !tr069setParamList.isEmpty()) {
          long instanceNumber;
          boolean addParamExist = false;
          AddObjectResponse addObjResponse =
              (AddObjectResponse) deviceRPCResponse.getOperationResponse();
          List<ParameterDTO> modifyParamList = new ArrayList<>();
          List<ParameterDTO> removeParamList = new ArrayList<>();
          ParameterDTO addParam = null;

          if (null != addObjResponse) {
            instanceNumber = addObjResponse.getInstanceNumber();
            String replaceIndex = null;
            String replaceParam = null;

            for (ParameterDTO setParam : tr069setParamList) {
              if (!setParam.isProcessed()) {
                String paramName = setParam.getParamName();
                final Matcher matcher = Pattern.compile(NUMBER_REGEX).matcher(paramName);
                String index = null;
                String modifyParamName = null;
                String subString = null;
                while (matcher.find()) {
                  index = matcher.group().substring(1, matcher.group().length() - 1);
                  StringBuilder sb = new StringBuilder(paramName);
                  int lastIndex = paramName.lastIndexOf(matcher.group());
                  modifyParamName = (sb.replace(lastIndex, lastIndex + matcher.group().length(),
                      "." + instanceNumber + ".")).toString();
                  subString = paramName.substring(0, matcher.start()) + ".";
                }
                if (null == replaceIndex)
                  replaceIndex = index;
                if (null == replaceParam)
                  replaceParam = subString;
                if (null != replaceIndex && null != index && replaceIndex.equals(index)
                    && replaceParam.equalsIgnoreCase(subString)) {
                  setParam.setProcessed(true);
                  modifyParamList.add(prepareParamDTO(modifyParamName, null, setParam));
                  removeParamList.add(prepareParamDTO(null, null, setParam));
                } else {
                  addParamExist = true;
                }
                if (null == addParam) {
                  logger.debug(
                      "The device index chosen is {} for adding the NBI tab parameter with index {}",
                      instanceNumber, replaceIndex);
                  addParam = prepareParamDTO(subString + replaceIndex,
                      String.valueOf(instanceNumber), setParam);
                }
              }
            }
          }
          // Replace index with instance number and add in modify param list
          if (!modifyParamList.isEmpty()) {
            tr069modifyParamList.addAll(modifyParamList);
          }
          // Prepare add object param and add in set param list
          if (null != addParam) {
            addParam.setDataType("1");
            tr069setParamList.add(addParam);
          }
          // Remove all processed set params from set param list
          if (!removeParamList.isEmpty()) {
            tr069setParamList.removeAll(removeParamList);
          }

          logger.debug("Persisting the NBI request for addObject");
          List<TR069DeviceRPCRequestEntity> entityList =
              deviceRPCRequestRepositoryHelper.findByDeviceIdAndOperationId(
                  deviceDetails.getDeviceId(), nbiDeviceOperationRequest.getOperationId());
          List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntityList =
              TR069RequestProcessorUtility.convertToEntity(nbiDeviceOperationRequest);
          for (int i = 0; i < entityList.size(); i++) {
            tr069DeviceRPCRequestEntityList.get(i).setId(entityList.get(i).getId());
          }
          deviceRPCRequestRepositoryHelper.saveAll(tr069DeviceRPCRequestEntityList);

          if (addParamExist) {
            nextOperation = OperationOrder.ADD_OBJECT;
            logger.debug(NXT_OPERATION, nextOperation);
          } else {
            nextOperation = OperationOrder.SET_PARAMETER_VALUE;
            logger.debug(NXT_OPERATION, nextOperation);
          }
        } else {
          nextOperation = OperationOrder.SET_PARAMETER_VALUE;
          logger.debug(NXT_OPERATION, nextOperation);
        }
      } else if (opResponse instanceof SetParameterValueResponse) {
        logger.debug("Received Set parameter value response");
        isPendingOperationExists = false;
        for (ParameterDTO setParam : tr069modifyParamList) {
          if (Boolean.TRUE.equals(setParam.isInitiated())
              && Boolean.FALSE.equals(setParam.isProcessed())) {
            setParam.setInitiated(Boolean.FALSE);
            setParam.setProcessed(Boolean.TRUE);
          } else if (Boolean.FALSE.equals(setParam.isInitiated())
              && Boolean.FALSE.equals(setParam.isProcessed())) {
            isPendingOperationExists = true;
            nextOperation = OperationOrder.SET_PARAMETER_VALUE;
          }
        }

        updateParamChangedFlagInDb(deviceDetails.getDeviceId(), nbiDeviceOperationRequest);
        logger.debug("Next operation to be executed is : {}", nextOperation);
      }
    }

    if (isPendingOperationExists) {
      boolean checkForNextoperation = true;
      while (checkForNextoperation) {
        switch (nextOperation) {
          case DELETE_OBJECT:
            if (null != tr069deleteParamList && !tr069deleteParamList.isEmpty()) {
              logger.debug("Started executing delete object request");
              checkForNextoperation = false;
              DeviceRPCRequest clonedOpRequest = cloneNBIRequest(nbiDeviceOperationRequest);
              List<ParameterDTO> deleteParamList = new ArrayList<>();
              // Just take the first not processed delete object from the list
              for (ParameterDTO deleteParam : tr069deleteParamList) {
                if (!deleteParam.isProcessed()) {
                  deleteParamList.add(deleteParam);
                  break;
                }
              }
              clonedOpRequest.setOpDetails(null);
              OperationDetails opDetails = new OperationDetails();
              opDetails.setOpCode(TR069OperationCode.DELETE_OBJECT);
              opDetails.setParmeters(deleteParamList);
              clonedOpRequest.setOpDetails(opDetails);
              operRequest = clonedOpRequest;
            } else {
              nextOperation = OperationOrder.ADD_OBJECT;
              logger.debug(NXT_OPERATION, nextOperation);
            }
            break;
          case ADD_OBJECT:
            if (null != tr069setParamList && !tr069setParamList.isEmpty()) {
              logger.debug("Started executing Add object request");
              checkForNextoperation = false;
              DeviceRPCRequest clonedOpRequest = cloneNBIRequest(nbiDeviceOperationRequest);
              List<ParameterDTO> addParamList = new ArrayList<>();
              // Just take the first not processed delete object from the list
              for (ParameterDTO addParam : tr069setParamList) {
                if (!addParam.isProcessed()) {
                  String addParamMO = null;
                  final Matcher matcher =
                      Pattern.compile(NUMBER_REGEX).matcher(addParam.getParamName());
                  while (matcher.find()) {
                    addParamMO = addParam.getParamName().substring(0, matcher.start()) + ".";
                  }
                  if (null != addParamMO) {
                    addParamList.add(prepareParamDTO(addParamMO, null, addParam));
                  }
                  break;
                }
              }
              clonedOpRequest.setOpDetails(null);
              OperationDetails opDetails = new OperationDetails();
              opDetails.setOpCode(TR069OperationCode.ADD_OBJECT);
              opDetails.setParmeters(addParamList);
              clonedOpRequest.setOpDetails(opDetails);
              operRequest = clonedOpRequest;
            } else {
              nextOperation = OperationOrder.SET_PARAMETER_VALUE;
              logger.debug(NXT_OPERATION, nextOperation);
            }
            break;
          case SET_PARAMETER_VALUE:
            checkForNextoperation = false;
            if (null != tr069modifyParamList && !tr069modifyParamList.isEmpty()) {
              logger.debug("Started executing SPV request");
              DeviceRPCRequest clonedOpRequest = cloneNBIRequest(nbiDeviceOperationRequest);
              clonedOpRequest.setOpDetails(null);
              OperationDetails opDetails = new OperationDetails();
              opDetails.setOpCode(TR069OperationCode.SET_PARAMETER_VALUES);
              List<ParameterDTO> unprocessedParamList = new ArrayList<>();
              ParameterDTO adminStateParam = null;
              for (ParameterDTO paramDTO : tr069modifyParamList) {
                if (!paramDTO.isProcessed()) {
                  if (isAdminStateExists(paramDTO)) {
                    adminStateParam = paramDTO;
                  } else {
                    unprocessedParamList.add(paramDTO);
                  }
                }
              }

              if (null != adminStateParam && isAdminStateFalse(adminStateParam.getParamValue())) {
                List<ParameterDTO> adminStateParamList = new ArrayList<>();
                adminStateParam.setInitiated(Boolean.TRUE);
                adminStateParamList.add(adminStateParam);
                opDetails.setParmeters(adminStateParamList);
                updateParamChangedFlagInDb(deviceDetails.getDeviceId(), nbiDeviceOperationRequest);
              } else if (!unprocessedParamList.isEmpty()) {
                setInititedFlagTrue(unprocessedParamList);
                opDetails.setParmeters(unprocessedParamList);
                updateParamChangedFlagInDb(deviceDetails.getDeviceId(), nbiDeviceOperationRequest);
              } else if (null != adminStateParam
                  && isAdminStateTrue(adminStateParam.getParamValue())) {
                List<ParameterDTO> paramList = new ArrayList<>();
                adminStateParam.setInitiated(Boolean.TRUE);
                paramList.add(adminStateParam);
                opDetails.setParmeters(paramList);
                updateParamChangedFlagInDb(deviceDetails.getDeviceId(), nbiDeviceOperationRequest);
              } else {
                isPendingOperationExists = false;
              }

              clonedOpRequest.setOpDetails(opDetails);
              operRequest = clonedOpRequest;
            } else {
              isPendingOperationExists = false;
              operRequest = null;
            }
            break;
          default:
            isPendingOperationExists = false;
            operRequest = null;
        }
      }
    }

    if (!isPendingOperationExists) {
      logger.debug(
          "No pending operation exists, hence marking the operation as processed with id {} "
              + "and sending GetParameterValueResponse for ConfigureMultipleObjects",
          responseOperationId);
      List<ParameterDTO> responseParamList = new ArrayList<>();

      if (tr069deleteParamList != null) {
        for (ParameterDTO delete : tr069deleteParamList) {
          delete.setParamValue(SUCCESS);
          delete.setDataType("2");
          responseParamList.add(delete);
        }
      }

      if (tr069modifyParamList != null) {
        for (ParameterDTO modify : tr069modifyParamList) {
          modify.setParamValue(SUCCESS);
          modify.setDataType("4");
          responseParamList.add(modify);
        }
      }

      responseParamList.addAll(tr069setParamList);

      OperationResponse operationResponse = new GetParameterValueResponse();
      operationResponse.setParameterDTOs(responseParamList);
      if (deviceRPCResponse != null) {
        deviceRPCResponse.setDeviceDetails(deviceDetails);
        deviceRPCResponse.setOperationResponse(operationResponse);
        deviceRPCResponse.setOperationId(responseOperationId);
      }

      logger.debug(
          "Prepared operation result for custom operation Configure Multiple Objects, hence marking as processed the corresponding NBI Operation request record");
      deviceRPCRequestRepositoryHelper.markDeviceRPCRequestAsProcessed(deviceDetails.getDeviceId(),
          responseOperationId);
    }

    customOperationData.setDeviceRPCResponse(deviceRPCResponse);
    customOperationData.setDeviceRPCRequest(operRequest);
    logger.debug("Finished processing Configure multiple object");
    return customOperationData;
  }


  enum OperationOrder {

    SET_PARAMETER_VALUE(null), ADD_OBJECT(SET_PARAMETER_VALUE), DELETE_OBJECT(ADD_OBJECT);

    OperationOrder nextOperation;

    OperationOrder(OperationOrder nextOperation) {
      this.nextOperation = nextOperation;
    }

    public OperationOrder getNextOperation() {
      return nextOperation;
    }

  }

  private DeviceRPCRequest cloneNBIRequest(DeviceRPCRequest nbiDeviceOperationRequest) {
    return new DeviceRPCRequest(nbiDeviceOperationRequest);
  }

  private ParameterDTO prepareParamDTO(String name, String value, ParameterDTO paramDTO) {
    ParameterDTO parameterDTO = new ParameterDTO();
    if (null != name) {
      parameterDTO.setParamName(name);
    } else {
      parameterDTO.setParamName(paramDTO.getParamName());
    }
    if (null != value) {
      parameterDTO.setParamValue(value);
    } else {
      parameterDTO.setParamValue(paramDTO.getParamValue());
    }
    parameterDTO.setDataType(paramDTO.getDataType());
    parameterDTO.setProcessed(paramDTO.isProcessed());

    return parameterDTO;
  }

  private boolean isAdminStateExists(ParameterDTO paramDTO) {
    return (paramDTO.getParamName().contains(TR069RequestProcessorUtility.ADMIN_STATE)
        || paramDTO.getParamName().contains(TR069RequestProcessorUtility.ADMIN_STATUS));
  }

  private void updateParamChangedFlagInDb(String deviceId,
      DeviceRPCRequest nbiDeviceOperationRequest) throws TR069EventProcessingException {
    List<TR069DeviceRPCRequestEntity> entityList = deviceRPCRequestRepositoryHelper
        .findByDeviceIdAndOperationId(deviceId, nbiDeviceOperationRequest.getOperationId());
    List<TR069DeviceRPCRequestEntity> tr069DeviceRPCRequestEntityList =
        TR069RequestProcessorUtility.convertToEntity(nbiDeviceOperationRequest);
    for (int i = 0; i < entityList.size(); i++) {
      tr069DeviceRPCRequestEntityList.get(i).setId(entityList.get(i).getId());
    }
    deviceRPCRequestRepositoryHelper.saveAll(tr069DeviceRPCRequestEntityList);
  }

  private void setInititedFlagTrue(List<ParameterDTO> unprocessedParamList) {
    for (ParameterDTO paramDTO : unprocessedParamList) {
      paramDTO.setInitiated(Boolean.TRUE);
    }
  }

  private boolean isAdminStateFalse(String adminState) {
    return (null != adminState && (adminState.equalsIgnoreCase(Boolean.FALSE.toString())
        || adminState.equalsIgnoreCase("0")));
  }

  private boolean isAdminStateTrue(String adminState) {
    return (null != adminState && (adminState.equalsIgnoreCase(Boolean.TRUE.toString())
        || adminState.equalsIgnoreCase("1")));
  }

}
