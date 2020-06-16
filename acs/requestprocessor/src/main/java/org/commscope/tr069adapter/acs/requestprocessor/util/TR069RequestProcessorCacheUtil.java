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

import org.commscope.tr069adapter.acs.requestprocessor.dto.TR069RequestProcessorData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;

@Component
public class TR069RequestProcessorCacheUtil implements TransactionSynchronization {

  private static final Logger logger =
      LoggerFactory.getLogger(TR069RequestProcessorCacheUtil.class);

  private static ThreadLocal<TR069RequestProcessorData> threadLocalCache = new ThreadLocal<>();

  public void put(TR069RequestProcessorData tR069RequestProcessorData) {

    try {
      logger.debug("Entering put");
      threadLocalCache.set(tR069RequestProcessorData);
    } catch (Exception e) {
      throw new IllegalStateException(e.getMessage());
    }
  }

  public static TR069RequestProcessorData get() {
    return threadLocalCache.get();
  }

  @Override
  public void beforeCompletion() {
    logger.debug("Entering beforeCompletion");
    threadLocalCache.remove();
  }

  @Override
  public void afterCompletion(int status) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void afterCommit() {
    throw new UnsupportedOperationException();
  }

  @Override
  public void beforeCommit(boolean arg0) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void flush() {
    throw new UnsupportedOperationException();
  }

  @Override
  public void resume() {
    throw new UnsupportedOperationException();
  }

  @Override
  public void suspend() {
    throw new UnsupportedOperationException();
  }
}
