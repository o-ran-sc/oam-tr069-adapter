package org.commscope.tr069adapter.mapper.util;

public class MapperConstants {
  public static final String HEART_BEAT_PERIOD = "heartbeatPeriod";
  public static final String HEART_BEAT = "heartbeat";
  public static final String COUNT_DOWN_TIMER = "countDownTimer";
  public static final String COUNT_DOWN_TIMER_SET_VAL = "0";
  public static final String INVALID_COUNT_DOWN_TIMER_MSG =
      "Only 0 is allowed for countDownTimer value during set operation";
  public static final String INVALID_PARAM_VAL_ERROR_CODE = "9007";

  public static final int DEVICE_REACHABLE_STATUS_CODE = 100;
  public static final int DEVICE_REACHABILITY_OP_FAILURE_CODE = 101;
  public static final String DEVICE_TIMEOUT_STATUS_CODE = "8006";
  public static final String DEVICE_REACHABLE_MSG = "Device is reachable";
  public static final String DEFAULT_OP_TIMEOUT_MSG = "Operation Timed out";
  public static final String RPC_SUCCESS = "0";
}
