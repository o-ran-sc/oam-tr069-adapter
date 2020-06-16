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

package org.commscope.tr069adapter.acs.common.dto;

import java.util.Arrays;

import org.commscope.tr069adapter.acs.common.ParameterDTO;

public class ParameterAttributeDTO extends ParameterDTO {

  private static final long serialVersionUID = 3036245080206860431L;

  private boolean notificationChange;

  private boolean accesslistChange;

  private int notification;

  private String[] accessList;

  public boolean getNotificationChange() {
    return notificationChange;
  }

  public void setNotificationChange(boolean notificationChange) {
    this.notificationChange = notificationChange;
  }

  public boolean getAccesslistChange() {
    return accesslistChange;
  }

  public void setAccesslistChange(boolean accesslistChange) {
    this.accesslistChange = accesslistChange;
  }

  public int getNotification() {
    return notification;
  }

  public void setNotification(int notification) {
    this.notification = notification;
  }

  public String[] getAccessList() {
    return accessList;
  }

  public void setAccessList(String[] accessList) {
    this.accessList = accessList;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + Arrays.hashCode(accessList);
    result = prime * result + (accesslistChange ? 1231 : 1237);
    result = prime * result + notification;
    result = prime * result + (notificationChange ? 1231 : 1237);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    ParameterAttributeDTO other = (ParameterAttributeDTO) obj;
    if (!Arrays.equals(accessList, other.accessList))
      return false;
    if (accesslistChange != other.accesslistChange)
      return false;
    if (notification != other.notification)
      return false;
    return (notificationChange != other.notificationChange);
  }

}
