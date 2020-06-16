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

package org.commscope.tr069adapter.common.scheduler.impl;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;
import org.springframework.stereotype.Component;

@Component
public class QuartzSchedulerProducer {

  private static final Log logger = LogFactory.getLog(QuartzSchedulerProducer.class);

  public Scheduler getStdScheduler() {
    StdSchedulerFactory schedulerFactory = new StdSchedulerFactory();
    try {
      schedulerFactory.initialize("config/quartz.properties");
      Properties props = new Properties();
      props.setProperty("org.quartz.scheduler.instanceName",
          props.getProperty("org.quartz.scheduler.instanceName") + "_standby");
      return schedulerFactory.getScheduler();
    } catch (SchedulerException e) {
      logger.error("SchedulerException : {}" + e.getMessage());
    }

    return null;
  }

  public Properties initialize(String filename) throws SchedulerException {

    InputStream is = null;
    Properties props = new Properties();

    is = Thread.currentThread().getContextClassLoader().getResourceAsStream(filename);

    try {
      if (is != null) {
        is = new BufferedInputStream(is);
      } else {
        is = new BufferedInputStream(new FileInputStream(filename));
      }
      props.load(is);
    } catch (IOException ioe) {
      throw new SchedulerException("Properties file: '" + filename + "' could not be read.", ioe);
    } finally {
      if (is != null)
        try {
          is.close();
        } catch (IOException ignore) {
          logger.error("while initialize : {}" + ignore.getMessage());
        }
    }

    return props;
  }
}
