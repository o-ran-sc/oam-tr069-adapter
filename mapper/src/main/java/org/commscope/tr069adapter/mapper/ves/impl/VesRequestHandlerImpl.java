package org.commscope.tr069adapter.mapper.ves.impl;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;
import org.commscope.tr069adapter.acs.common.OperationResponse;
import org.commscope.tr069adapter.mapper.MapperConfigProperties;
import org.commscope.tr069adapter.mapper.model.ErrorCodeDetails;
import org.commscope.tr069adapter.mapper.sync.SynchronizedRequestHandler;
import org.commscope.tr069adapter.mapper.util.ErrorCodeUtil;
import org.commscope.tr069adapter.mapper.util.MapperConstants;
import org.commscope.tr069adapter.mapper.ves.VesRequestHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class VesRequestHandlerImpl implements VesRequestHandler {

  private static final Logger LOG = LoggerFactory.getLogger(VesRequestHandlerImpl.class);

  @Autowired
  SynchronizedRequestHandler syncHandler;

  @Autowired
  MapperConfigProperties config;

  @Autowired
  private ErrorCodeUtil errorCodeUtil;


  @Override
  public DeviceRPCResponse handleDeviceConnectivityRequest(DeviceRPCRequest deviceRPCRequest) {
    DeviceRPCResponse deviceRPCResponse = syncHandler.performDeviceOperation(deviceRPCRequest);
    if (null == deviceRPCResponse) {
      return getTimeOutResponse(deviceRPCRequest);
    }

    return deviceRPCResponse;
  }

  private DeviceRPCResponse getTimeOutResponse(DeviceRPCRequest deviceRPCRequest) {
    DeviceRPCResponse timeOutErrorResponse = new DeviceRPCResponse();

    timeOutErrorResponse.setDeviceDetails(deviceRPCRequest.getDeviceDetails());

    OperationResponse operationResponse = new OperationResponse();
    operationResponse.setStatus(MapperConstants.DEVICE_REACHABILITY_OP_FAILURE_CODE);// device
                                                                                     // reachable...change
                                                                                     // value 1 to
                                                                                     // some
                                                                                     // constant or
                                                                                     // enum
    operationResponse.setOperationCode(deviceRPCRequest.getOpDetails().getOpCode());

    timeOutErrorResponse.setOperationResponse(operationResponse);
    timeOutErrorResponse.setFaultKey(MapperConstants.DEVICE_TIMEOUT_STATUS_CODE);
    ErrorCodeDetails errorCodeDetails =
        errorCodeUtil.getErrorCodeMetaData(MapperConstants.DEVICE_TIMEOUT_STATUS_CODE);
    if (null != errorCodeDetails) {
      timeOutErrorResponse.setFaultString(errorCodeDetails.getErrorMessage());
    } else {
      timeOutErrorResponse.setFaultString(MapperConstants.DEFAULT_OP_TIMEOUT_MSG);
    }
    return timeOutErrorResponse;
  }
}
