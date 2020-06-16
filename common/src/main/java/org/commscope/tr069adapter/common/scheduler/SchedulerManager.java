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

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface SchedulerManager {

  /**
   * Add the given job to the scheduler along with trigger details provided. This method is used to
   * run the job repeatedly at a given time interval. It throws an scheduler exception in case any
   * problem with the given data or some unexpected exception occurred while adding the job to the
   * scheduler.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param triggerInfo - object contains the information about the trigger of this particular job.
   * @param jobContext - jobContext information which can be passed at the time of scheduling the
   *        job. Note that it is strictly recommended to put only primitive types in this map.
   *        Putting objects are not recommended.
   * 
   * @throws SchedulerException - An object representing the error details which caused the problem
   *         while scheduling the job.
   */
  public void scheduleJob(String jobName, TriggerInfo triggerInfo, ExecutionContext jobContext)
      throws SchedulerException;

  /**
   * Add the given job to the scheduler along with trigger details provided. This method is used to
   * run the job repeatedly at a given time interval. It throws an scheduler exception in case any
   * problem with the given data or some unexpected exception occurred while adding the job to the
   * scheduler.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param jobGroup - String object representing the job group. which can be used later to query
   *        the job *
   * @param triggerInfo - object contains the information about the trigger of this particular job.
   * @param jobContext - jobContext information which can be passed at the time of scheduling the
   *        job. Note that it is strictly recommended to put only primitive types in this map.
   *        Putting objects are not recommended.
   * 
   * @throws SchedulerException - An object representing the error details which caused the problem
   *         while scheduling the job.
   */
  public void scheduleJob(String jobName, String jobGroup, TriggerInfo triggerInfo,
      ExecutionContext jobContext) throws SchedulerException;

  /**
   * Add the given job to the scheduler along with trigger details provided. This method is used to
   * run the job repeatedly at a given time interval. It throws an scheduler exception in case any
   * problem with the given data or some unexpected exception occurred while adding the job to the
   * scheduler.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param triggerInfo - object contains the information about the trigger of this particular job.
   * @param expiredTriggerInfo - object contains the information about the trigger of this
   *        particular job which will mark it to expire after end time.
   * @param jobContext - jobContext information which can be passed at the time of scheduling the
   *        job. Note that it is strictly recommended to put only primitive types in this map.
   *        Putting objects are not recommended.
   * @param expiredExecutionContext - expiredExecutionContext information which can be passed at the
   *        time of scheduling the job. Note that it is strictly recommended to put only primitive
   *        types in this map. Putting objects are not recommended.
   * 
   * @throws SchedulerException - An object representing the error details which caused the problem
   *         while scheduling the job.
   */
  public void scheduleJob(String jobName, String jobGroup, TriggerInfo triggerInfo,
      ExecutionContext jobContext, TriggerInfo expiredTriggerInfo,
      ExecutionContext expiredExecutionContext) throws SchedulerException;

  /**
   * Modifies the existing scheduled job which causes the changes in the job execution times as per
   * the given data.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param triggerInfo - object contains the information about the trigger of this particular job.
   * @throws SchedulerException - An object representing the error details which caused the problem
   *         while scheduling the job.
   */
  public void modifySchedule(String jobName, TriggerInfo triggerInfo) throws SchedulerException;

  /**
   * Modifies the existing scheduled job which causes the changes in the job execution times as per
   * the given data.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param jobGroup - String object representing the job group. which can be used later to query
   *        the job
   * @param triggerInfo - object contains the information about the trigger of this particular job.
   * @throws SchedulerException - An object representing the error details which caused the problem
   *         while scheduling the job.
   */
  public void modifySchedule(String jobName, String jobGroup, TriggerInfo triggerInfo)
      throws SchedulerException;

  public void modifySchedule(String jobName, String jobGroup, TriggerInfo triggerInfo,
      TriggerInfo expiredTriggerInfo, ExecutionContext expiredExecutionContext)
      throws SchedulerException;

  /**
   * Returns the list of scheduled jobs along with the job details such as the job status
   * (enabled/disabled) etc..
   * 
   * @return
   */
  public List<JobInfo> getJobList();

  /**
   * Returns the job details for a given job name such as the job status (enabled/disabled) etc..
   * 
   * @return
   */
  public JobInfo getJobInfo(String jobId);

  /**
   * Removes the given job from the schedule. Throws an exceptions in case the job does not exist.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * 
   * @throws SchedulerException - An object representing the error details which caused the problem
   *         while scheduling the job.
   */
  public void deleteSchedule(String jobName) throws SchedulerException;

  /**
   * Removes the given job from the schedule. Throws an exceptions in case the job does not exist.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param jobGroup - String object representing the job group. which can be used later to query
   *        the job
   * @throws SchedulerException - An object representing the error details which caused the problem
   *         while scheduling the job.
   */
  public void deleteSchedule(String jobName, String jobGroup) throws SchedulerException;

  /**
   * Checks if the given job is enabled or not.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @return - returns true if job is enabled. False otherwise.
   */
  public boolean isJobEnabled(String jobName);

  /**
   * Checks if the given job is enabled or not.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param jobGroup - String object representing the job group. which can be used later to query
   *        the job
   * @return - returns true if job is enabled. False otherwise.
   */
  public boolean isJobEnabled(String jobName, String jobGroup);

  /**
   * Enables the given job. Throws an exception in case job does not exist.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * 
   * @throws SchedulerException
   */
  public void enableJob(String jobName) throws SchedulerException;

  /**
   * Enables the given job. Throws an exception in case job does not exist.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param jobGroup - String object representing the job group. which can be used later to query
   *        the job
   * 
   * @throws SchedulerException
   */
  public void enableJob(String jobName, String jobGroup) throws SchedulerException;

  /**
   * Disables the given job. throws an exception in case the job does not exist.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * 
   * @throws SchedulerException
   */
  public void disableJob(String jobName) throws SchedulerException;

  /**
   * Disables the given job. throws an exception in case the job does not exist.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param jobGroup - String object representing the job group. which can be used later to query
   *        the job
   * @throws SchedulerException
   */
  public void disableJob(String jobName, String jobGroup) throws SchedulerException;

  /**
   * Checks if the job is present already in the system or not.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @return - returns false if job is not present in the system.
   * @throws SchedulerException
   */
  public boolean isJobExist(String jobName) throws SchedulerException;

  /**
   * Checks if the job is present already in the system or not.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @param jobGroup - String object representing the job group. which can be used later to query
   *        the job
   * @return - returns false if job is not present in the system.
   * @throws SchedulerException
   */
  public boolean isJobExist(String jobName, String jobGroup) throws SchedulerException;

  public JobInfo getJobInfo(String jobId, String string);

  /**
   * Checks if the job schedule exists in the system or not.
   * 
   * @param jobName - String object representing the job name. which can be used later to query the
   *        job
   * @return - returns false if job is not present in the system.
   * @throws SchedulerException
   */
  public boolean isJobScheduleExist(String jobName, String jobGroup) throws SchedulerException;

  public void updateJobData(String jobName, String jobGroup, Map<String, Object> jobData)
      throws SchedulerException;

  public Map<String, Object> getJobData(String jobName, String jobGroup) throws SchedulerException;

  public void deleteJobData(String jobName, String jobGroup, Set<String> keys)
      throws SchedulerException;

  public boolean resumeQuartzSchedulars() throws SchedulerException;

  public boolean stopQuartzSchedulars() throws SchedulerException;

  public void switchToStandby() throws SchedulerException;

  public void switchToActive() throws SchedulerException;

  public boolean isQuartzRunninginActiveMode();

  void deleteSchedule(String startJobName, String endJobName, String monitorJobName,
      String jobGroup) throws SchedulerException;

}
