package org.commscope.tr069adapter.mapper.util;

import java.util.HashMap;
import java.util.Map;

public class FirwareUpgradeErrorCode {
  private static Map<Integer, String> ErrorCodes = new HashMap<Integer, String>();;

  static {
    ErrorCodes.put(9019, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9001, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9016, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9012, "AUTHENTICATION_ERROR");
    ErrorCodes.put(9015, "PROTOCOL_ERROR");
    ErrorCodes.put(9002, "APPLICATION_ERROR");
    ErrorCodes.put(9003, "APPLICATION_ERROR");
    ErrorCodes.put(9020, "TIMEOUT");
    ErrorCodes.put(0, "COMPLETED");
  }

  public static String getErrorCodeMapping(int errorCode) {
    return ErrorCodes.get(errorCode) != null ? ErrorCodes.get(errorCode) : "APPLICATION_ERROR";
  }

}
