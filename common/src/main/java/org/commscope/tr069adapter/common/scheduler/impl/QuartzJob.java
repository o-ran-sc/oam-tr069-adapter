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

import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.JOB_NAME;
import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.SUB_SYSTEM;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.commscope.tr069adapter.common.scheduler.ExecutionContext;
import org.commscope.tr069adapter.common.scheduler.Job;
import org.quartz.DisallowConcurrentExecution;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.PersistJobDataAfterExecution;
import org.springframework.context.ApplicationContext;

@PersistJobDataAfterExecution
@DisallowConcurrentExecution
public class QuartzJob implements org.quartz.Job {

  private static ApplicationContext appctx;

  private static final Log logger = LogFactory.getLog(QuartzJob.class);

  protected void executeInternal(JobExecutionContext context) throws JobExecutionException {
    JobDataMap dataMap = null;
    try {
      dataMap = context.getJobDetail().getJobDataMap();
      ExecutionContext jobContext = new ExecutionContext(context.getJobDetail().getKey().getName(),
          dataMap.getString(SUB_SYSTEM), dataMap.getString(JOB_NAME));
      for (String key : dataMap.getKeys()) {
        jobContext.addJobData(key, dataMap.get(key));
      }
      // add scheduling information
      jobContext.addJobData("SCHEDULED_TIME", context.getScheduledFireTime());
      String beanName = jobContext.getJobName();
      Job job = (Job) appctx.getBean(beanName);
      job.execute(jobContext);

    } catch (Exception exception) {
      if (dataMap != null) {
        logger.error("executeInternal : Executing job " + dataMap.getString(JOB_NAME)
            + ", SubSystem " + dataMap.getString(SUB_SYSTEM));
      }
      logger.error("executeInternal : Could not execute job. Exception while executing the job",
          exception);
      throw new JobExecutionException(exception);
    }
  }

  public static String getJndiName(String subSystem, String jobName) {
    String jndiName = "csadapter/";
    jndiName += subSystem + "/" + jobName + "#" + Job.class.getName();
    return jndiName;
  }

  public void execute(JobExecutionContext context) {
    try {
      executeInternal(context);
    } catch (JobExecutionException e) {
      logger.error("Exception : " + e.getMessage());
    }
  }

  public static void setApplicationContext(ApplicationContext ctx) {
    appctx = ctx;
  }

  public static ApplicationContext getApplicationContext() {
    return appctx;
  }
}
