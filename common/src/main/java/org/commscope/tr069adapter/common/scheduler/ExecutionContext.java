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


package org.commscope.tr069adapter.common.scheduler;

import static org.commscope.tr069adapter.common.scheduler.impl.QuartzSchedulerConstants.JOB_APPENDER;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class ExecutionContext implements Serializable {

  /**
   * 
   */
  private static final long serialVersionUID = 8182423848241219082L;

  /**
   * String representing the bean id which by which is job is configured in the spring container.
   */
  private String subSystemName;

  private String jobName;

  private String jobId;

  private String jobGroup;

  /**
   * Constructor for getting the execution context object. Need to pass a string which represents
   * the bean id that is configured in spring container.
   * 
   * @param subSystemName - String object which is bean id of the job that is to be executed.
   */
  public ExecutionContext(String subSystemName, String jobName) {
    if (null == subSystemName || "".equals(subSystemName))
      throw new InstantiationError("Subsystem name Can not be null or empty");
    this.subSystemName = subSystemName;
    if (null == jobName || "".equals(jobName))
      throw new InstantiationError("Job name Can not be null or empty");
    this.jobName = jobName;
  }

  /**
   * Constructor for getting the execution context object. Need to pass a string which represents
   * the bean id that is configured in spring container.
   * 
   * @param subSystemName - String object which is bean id of the job that is to be executed.
   */
  public ExecutionContext(String jobId, String subSystemName, String jobName) {
    if (null == subSystemName || "".equals(subSystemName))
      throw new InstantiationError("Subsystem name Can not be null or empty");
    this.subSystemName = subSystemName;
    if (null == jobName || "".equals(jobName))
      throw new InstantiationError("Job name Can not be null or empty");
    this.jobName = jobName;
    this.jobId = jobId;
  }

  public String getJobName() {
    return jobName;
  }

  public String getSubSystemName() {
    return subSystemName;
  }

  private Map<String, Object> context = new HashMap<>();

  /**
   * Can be used to get some job related data, at the time of job execution.
   * 
   * @param key
   * @return
   */
  public Object getJobData(String key) {
    return context.get(key);
  }

  /**
   * Can be used add some job related data at the time of scheduling the job. Note: This data can be
   * retrieved later at the time of execution by the Job implementation.
   * 
   * @param key
   * @param value
   */
  public void addJobData(String key, Object value) {
    context.put(key, value);
  }

  /**
   * This method can be used to get the all the job data parameter names which can be used to
   * iterate through the all job data parameters.
   * 
   * @return - String set.
   */
  public Set<String> getJobDatakeySet() {
    return context.keySet();
  }

  public String getJobId() {
    int index = jobId.lastIndexOf(JOB_APPENDER);
    if (index > -1) {
      jobId = jobId.substring(0, index);
    }
    return jobId; // remove appender
  }

  public String getJobGroup() {
    return jobGroup;
  }

  public void setJobGroup(String jobGroup) {
    this.jobGroup = jobGroup;
  }

}
