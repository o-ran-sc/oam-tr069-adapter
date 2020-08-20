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

package org.commscope.tr069adapter.acs.requestprocessor.impl;

import com.fasterxml.uuid.EthernetAddress;
import com.fasterxml.uuid.Generators;
import com.fasterxml.uuid.impl.TimeBasedGenerator;

import java.util.Optional;
import java.util.UUID;

import org.commscope.tr069adapter.acs.common.exception.SessionConcurrentAccessException;
import org.commscope.tr069adapter.acs.common.exception.SessionManagerException;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.requestprocessor.dao.SessionRepository;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionDTO;
import org.commscope.tr069adapter.acs.requestprocessor.dto.SessionState;
import org.commscope.tr069adapter.acs.requestprocessor.entity.SessionManagerEntity;
import org.commscope.tr069adapter.acs.requestprocessor.util.SessionManagerUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SessionManager {

  private static final Logger logger = LoggerFactory.getLogger(SessionManager.class);

  @Autowired
  SessionRepository sessionRepository;

  /**
   * @param deviceId
   * @return
   * @throws SessionManagerException
   */
  public SessionDTO getSession(String deviceId) {
    SessionManagerEntity session = null;
    Optional<SessionManagerEntity> optionalSessionData = sessionRepository.findById(deviceId);
    if (optionalSessionData.isPresent()) {
      session = optionalSessionData.get();
    }
    return SessionManagerUtility.convertToDTO(session);
  }

  /**
   * @param sessionId
   * @return
   * @throws SessionManagerException
   */
  public SessionDTO getSessionBySessionId(String sessionId) throws SessionManagerException {

    SessionManagerEntity sessionManagerEntity = sessionRepository.findBySessionId(sessionId);
    if (sessionManagerEntity == null || SessionState.TERMINATED
        .equals(SessionState.getByValue(sessionManagerEntity.getState()))) {
      throw new SessionManagerException(ErrorCode.SESSION_EXPIRED, sessionId);
    }
    return SessionManagerUtility.convertToDTO(sessionManagerEntity);
  }

  /**
   * @param session
   * @return
   * @throws SessionManagerException
   */
  public SessionDTO createSession(SessionDTO session) throws SessionManagerException {
    if (session == null) {
      SessionManagerException sme = new SessionManagerException(ErrorCode.SESSION_CREATION_ERROR,
          "Session object cannot be null");
      String smeMessage = sme.getMessage().replaceAll("[\n|\r|\t]", "_");
      logger.error(smeMessage);
      throw sme;
    }
    logger.debug("Creating a new session for the device");
    return SessionManagerUtility
        .convertToDTO(sessionRepository.save(SessionManagerUtility.convertToEntity(session)));
  }

  /**
   * @param dto
   * @return
   * @throws SessionManagerException
   */
  public SessionDTO updateSession(SessionDTO dto) {
    return SessionManagerUtility
        .convertToDTO(sessionRepository.save(SessionManagerUtility.convertToEntity(dto)));
  }

  /**
   * @param deviceId
   * @throws SessionConcurrentAccessException
   */
  public void deleteSession(String deviceId) {
    sessionRepository.deleteById(deviceId);
  }

  /**
   * @param deviceId
   * @return
   * @throws SessionManagerException
   */
  public SessionDTO getLockedSession(String deviceId) {
    logger.debug("Acquiring the session lock for the device");
    return SessionManagerUtility.convertToDTO(sessionRepository.findByDeviceId(deviceId));
  }

  /**
   * @return
   */
  public String generateUniqueSessionID() {
    EthernetAddress addr = EthernetAddress.fromInterface();
    TimeBasedGenerator uuidGenerator = Generators.timeBasedGenerator(addr);
    UUID uuid = uuidGenerator.generate();
    return uuid.toString();
  }

}
