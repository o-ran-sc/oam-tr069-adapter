package org.commscope.tr069adapter.mapper.ves;

import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.DeviceRPCResponse;

public interface VesRequestHandler {

  public DeviceRPCResponse handleDeviceConnectivityRequest(DeviceRPCRequest deviceRPCRequest);
}
