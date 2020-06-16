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


package org.commscope.tr069adapter.acs.common.utils;

/**
 * Class Description: All the error definitions
 * 
 */

public enum ErrorCode {

  UNKNOWN_ERROR("UNKNOWN_ERROR"),

  UNSUPPORTED_CHARACTER_ENCODING("UNSUPPORTED_CHARACTER_ENCODING"), UNAUTHORIZED_EVENT(
      "UNAUTHORIZED_EVENT"), OUI_OR_PC_MISMATCH("OUI_OR_PC_MISMATCH"), INVALID_PARAMS_IN_INFORM(
          "INVALID_PARAMS_IN_INFORM"), FAILED_PROCESSING_INFORM(
              "FAILED_PROCESSING_INFORM"), FAILED_PROCESSING_RPC_RESPONSE(
                  "FAILED_PROCESSING_RPC_RESPONSE"), EMPTY_REQUEST_PROCESSING_ERROR(
                      "EMPTY_REQUEST_PROCESSING_ERROR"),

  // TR069 NBI Service Module errors
  DEVICE_NOT_ACTIVATED("DEVICE_NOT_ACTIVATED"), DEVICE_NOT_EXISTS("DEVICE_NOT_EXISTS"),

  // TR069 NBI Request Processor Module errors
  INVALID_NBI_REQUEST("INVALID_NBI_REQUEST"), MISSING_OPERATION_DETAILS(
      "MISSING_OPERATION_DETAILS"),

  // Session Manager Module errors
  SESSION_CREATION_ERROR("SESSION_CREATION_ERROR"), SESSION_ALREADY_LOCKED(
      "SESSION_ALREADY_LOCKED"), SESSION_EXPIRED(
          "SESSION_EXPIRED"), SESSION_INITIATION_FAILED("SESSION_INITIATION_FAILED");

  private String errorCodeKey;

  private ErrorCode(String errorCodeKey) {
    this.errorCodeKey = errorCodeKey;
  }

  public String getErrorCodeKey() {
    return errorCodeKey;
  }
}
