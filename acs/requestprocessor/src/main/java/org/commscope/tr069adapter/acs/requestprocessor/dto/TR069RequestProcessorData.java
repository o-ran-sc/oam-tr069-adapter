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


package org.commscope.tr069adapter.acs.requestprocessor.dto;

import java.io.Serializable;

import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;

public class TR069RequestProcessorData implements Serializable {

  private static final long serialVersionUID = -1935518456423720284L;

  private SessionDTO sessionDTO;

  private TR069DeviceDetails tr069DeviceDetails;

  public SessionDTO getSessionDTO() {
    return sessionDTO;
  }

  public void setSessionDTO(SessionDTO sessionDTO) {
    this.sessionDTO = sessionDTO;
  }

  public TR069DeviceDetails getTr069DeviceDetails() {
    return tr069DeviceDetails;
  }

  public void setTr069DeviceDetails(TR069DeviceDetails tr069DeviceDetails) {
    this.tr069DeviceDetails = tr069DeviceDetails;
  }
}
