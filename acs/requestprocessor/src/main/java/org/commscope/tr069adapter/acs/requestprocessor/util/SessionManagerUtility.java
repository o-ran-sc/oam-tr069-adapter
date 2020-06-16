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

import java.util.Date;

import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionState;
import org.commscope.tr069adapter.acs.requestprocessor.entity.SessionManagerEntity;

public class SessionManagerUtility {

  private SessionManagerUtility() {
    super();
  }

  public static SessionDTO convertToDTO(SessionManagerEntity entity) {
    if (null == entity) {
      return null;
    }

    SessionDTO dto = new SessionDTO();
    dto.setSessionState(SessionState.getByValue(entity.getState()));
    dto.setCurrentOperationId(entity.getCurrentOperationId());
    dto.setDeviceId(entity.getDeviceId());
    dto.setSessionId(entity.getSessionId());
    dto.setSessionStartTime(entity.getSessionStartTime());

    return dto;
  }

  /**
   * @param dto
   * @return
   */
  public static SessionManagerEntity convertToEntity(SessionDTO dto) {
    SessionManagerEntity entity = new SessionManagerEntity();
    if (dto.getSessionState() != null) {
      entity.setState(dto.getSessionState().getValue());
    } else {
      entity.setState(SessionState.READY.getValue());
    }
    entity.setCurrentOperationId(dto.getCurrentOperationId());
    entity.setDeviceId(dto.getDeviceId());
    entity.setSessionId(dto.getSessionId());

    if (dto.getSessionStartTime() != null) {
      entity.setSessionStartTime(dto.getSessionStartTime());
    } else {
      entity.setSessionStartTime(new Date());
    }
    return entity;
  }
}
