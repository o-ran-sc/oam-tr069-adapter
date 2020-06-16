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

package org.commscope.tr069adapter.vesagent.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import java.io.Serializable;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({"commonEventHeader", "pnfRegistrationFields", "faultFields"})
public class Event implements Serializable {

  /**
   * 
   */
  private static final long serialVersionUID = 1L;

  @JsonProperty("commonEventHeader")
  CommonEventHeader commonEventHeader;
  @JsonProperty("pnfRegistrationFields")
  PnfRegEventFields pnfRegistrationFields;
  @JsonProperty("faultFields")
  FaultFields faultFields;
  @JsonProperty("heartbeatFields")
  HeartbeatFields heartbeatFields;

  public CommonEventHeader getCommonEventHeader() {
    return commonEventHeader;
  }

  public void setCommonEventHeader(CommonEventHeader commonEventHeader) {
    this.commonEventHeader = commonEventHeader;
  }

  public PnfRegEventFields getPnfRegistrationFields() {
    return pnfRegistrationFields;
  }

  public void setPnfRegistrationFields(PnfRegEventFields pnfRegistrationFields) {
    this.pnfRegistrationFields = pnfRegistrationFields;
  }

  public FaultFields getFaultFields() {
    return faultFields;
  }

  public void setFaultFields(FaultFields faultFields) {
    this.faultFields = faultFields;
  }

  public HeartbeatFields getHeartbeatFields() {
    return heartbeatFields;
  }

  public void setHeartbeatFields(HeartbeatFields heartbeatFields) {
    this.heartbeatFields = heartbeatFields;
  }

}
