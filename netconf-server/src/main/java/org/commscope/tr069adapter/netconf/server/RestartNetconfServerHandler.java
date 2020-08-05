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

package org.commscope.tr069adapter.netconf.server;

import org.commscope.tr069adapter.netconf.entity.NetConfServerDetailsEntity;
import org.commscope.tr069adapter.netconf.error.RetryFailedException;
import org.commscope.tr069adapter.netconf.server.helper.ServerPortAllocationHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;

@Service
public class RestartNetconfServerHandler {

  private static final Logger logger = LoggerFactory.getLogger(RestartNetconfServerHandler.class);

  @Autowired
  NetConfServerManagerImpl manager;

  @Autowired
  ServerPortAllocationHelper serverPortAllocator;

  @Retryable(value = {RetryFailedException.class}, maxAttempts = 10,
      backoff = @Backoff(delay = 15000))
  public void restart(NetConfServerDetailsEntity entity) throws RetryFailedException {
    boolean isSucess = false;
    try {
      // restart netconf servers
      serverPortAllocator.checkAndReserveServerPort(entity.getListenPort());
      isSucess = manager.startNetConfServerInstance(entity);
    } catch (Exception e) {
      logger.error("Retry to netconf servers has  is failed. {}", e.toString());
      throw new RetryFailedException(e);
    }
    if (!isSucess) {
      throw new RetryFailedException(
          "Failed to start some of netconf servers. server list : " + entity);
    }
    logger.debug("Successfully started all failed netconf servers.");
  }

  @Recover
  public void recover(NetConfServerDetailsEntity entity) {
    logger.debug("Retrying starting failed netconf servers.");
    try {
      restart(entity);
    } catch (RetryFailedException e) {
      logger.error("Failed to start failed netconf servers. {}", e.toString());
    }
  }
}
