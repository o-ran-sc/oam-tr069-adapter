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

import java.io.Serializable;

public class JobInfo implements Serializable {

  /**
   * 
   */
  private static final long serialVersionUID = -238097326157092042L;
  private String jobName;
  private boolean isEnabled;
  private TriggerInfo triggerInfo;

  public JobInfo() {
    super();
  }

  public TriggerInfo getTriggerInfo() {
    return triggerInfo;
  }

  public void setTriggerInfo(TriggerInfo triggerInfo) {
    this.triggerInfo = triggerInfo;
  }

  public String getJobName() {
    return jobName;
  }

  public void setJobName(String jobName) {
    this.jobName = jobName;
  }

  public boolean isEnabled() {
    return isEnabled;
  }

  public void setEnabled(boolean isEnabled) {
    this.isEnabled = isEnabled;
  }
}
