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

public class AcsConstants {

  private AcsConstants() {}

  // CPE Management constants
  public static final String INFORM = "Inform";
  public static final String TRANSFER_COMPLETE = "TransferComplete";
  public static final String ACS_SESSIONID = "JSESSIONID";
  public static final String CWMP_VERSION = "CWMP_VERSION";

  // JMS Queues and connection factory names
  public static final String TR069_NBI_REQUEST_Q = "TR069NBIRequestQueue";
  public static final String TR069_NBI_REQUEST_CF = "tr069NBIRequestCF";

  public static final String CR_REQ_Q = "TR069DeviceResponseQueue";
  public static final String CR_REQ_CF = "tr069DeviceResponseCF";

  public static final String NBI_NOTIFICATION_Q = "NBINotificationQueue";
  public static final String NBI_NOTIFICATION_CF = "nbiNotificationCF";

  public static final String NBI_OP_RESULT_Q = "NBIOpResultQueue";
  public static final String NBI_OP_RESULT_CF = "nbiOpResultCF";

  // Mapper Service Constants
  public static final String MAPPER_SERVICE_QUALILFIER = "mapper-service";

  // TIMER ID Constants
  public static final String CONNECTION_REQUEST = "CONNECTION_REQUEST"; // Constant used in
                                                                        // building the TIMER ID
  public static final String SEPERATOR = "_"; // Constant used in building the TIMER ID

  // Connection Request (HTTP Get) constants
  public static final long CONNECTION_RETRY_SLEEP_TIME = 10 * 1000L; // On Connection Request (HTTP
                                                                     // GET) failure, thread will
                                                                     // sleep for x seconds before
                                                                     // retrying.
  public static final Long CR_TIMEOUT = 10 * 1000L; // On Connection Request (HTTP GET) is
                                                    // accepted by device, time to wait for '6
                                                    // Connection Request' Inform to be sent by
                                                    // device. If device does not send any inform
                                                    // within the configured time, connection
                                                    // request shall be retried
  public static final String CR_TIMEOUT_CALLBACK = "ConnectionRequestTimeoutHandler"; // On
                                                                                      // CR_TIMEOUT
                                                                                      // case, the
                                                                                      // callback
                                                                                      // class to
                                                                                      // be called
  public static final int HTTP_OP_FAILED = 1;
  public static final int MAX_CONNECT_RETRY_COUNT = 3; // Max retry count in case device is not
                                                       // accpeting the Connection Request (HTTP
                                                       // GET)
  public static final int DEFAULT_CONNECTION_TIMEOUT = 1 * 10 * 1000; // HTTP GET TIMEOUT, the
                                                                      // thread will be held for x
                                                                      // secs while connecting to
                                                                      // device
  public static final int HTTP_STATUS_OK = 200;
  public static final int HTTP_STATUS_OK_WITH_NO_CONTENT = 204;
  public static final int HTTP_OP_SUCCESS = 0;
  public static final Long HTTP_CONNECTION_IDLE_TIMEOUT = 5 * 1000L;

  // Request Processor Retry and Delay constants
  public static final Integer MAX_RETRY_LIMIT = 5; // Max retry count by request processor while
                                                   // processing Informs and Device RPC Responses
  public static final Long DELAY = 10000L; // A delay befoee retrying

  // RPC Timer Execution Constants
  public static final Long DEVICE_RPC_EXECUTION_TIMEOUT_SECONDS = 30L; // Timeout for the RPC,
                                                                       // Timer starts after
                                                                       // persisting the RPC. On
                                                                       // timeout, timedout RPC
                                                                       // response shall be sent
                                                                       // to device
  public static final Long DEVICE_RESPONSE_TIMEOUT = 30 * 1000L; // Session timeout, each session
                                                                 // on moving to processing state
                                                                 // will timeout after the
                                                                 // configured value, will restart
                                                                 // the timer if RPOC isn't
                                                                 // timedout yet
  public static final String SESSION_TIMEOUT_CALLBACK_JNDI = "SessionTimeoutHandler";
  public static final String WAITING_FOR_DEVICE_RESPONSE = "WAITING_FOR_DEVICE_RESPONSE";
  public static final String WAITING_FOR_NBI_RESPONSE = "WAITING_FOR_NBI_RESPONSE";

  // Timeout operation status codes
  public static final int OPERATION_EXPIRATION_TIMEOUT = 3;

  // Custom function constants
  public static final String SESSION_ID = "SESSION_ID";
  public static final String NUMBER_REGEX = "[.][0-9]+[.]";
  public static final String SUCCESS = "SUCCESS";
}
