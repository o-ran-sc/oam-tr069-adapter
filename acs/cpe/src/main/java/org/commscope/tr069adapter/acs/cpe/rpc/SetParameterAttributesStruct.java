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

package org.commscope.tr069adapter.acs.cpe.rpc;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "SetParameterAttributesStruct",
    namespace = "urn:NbiService.notification.nbi.com")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SetParameterAttributesStruct",
    propOrder = {"Name", "NotificationChange", "Notification", "AccessListChange", "AccessList"})
public class SetParameterAttributesStruct implements Serializable {

  private static final long serialVersionUID = 3800958675601111274L;

  /**
   * @return the name
   */
  public String getName() {
    return name;
  }

  /**
   * @param name the name to set
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * @return the notificationChange
   */
  public boolean isNotificationChange() {
    return notificationChange;
  }

  /**
   * @param notificationChange the notificationChange to set
   */
  public void setNotificationChange(boolean notificationChange) {
    this.notificationChange = notificationChange;
  }

  /**
   * @return the notification
   */
  public int getNotification() {
    return notification;
  }

  /**
   * @param notification the notification to set
   */
  public void setNotification(int notification) {
    this.notification = notification;
  }

  /**
   * @return the accessListChange
   */
  public boolean isAccessListChange() {
    return accessListChange;
  }

  /**
   * @param accessListChange the accessListChange to set
   */
  public void setAccessListChange(boolean accessListChange) {
    this.accessListChange = accessListChange;
  }

  /**
   * @return the accessList
   */
  public String[] getAccessList() {
    return accessList;
  }

  /**
   * @param accessList the accessList to set
   */
  public void setAccessList(String[] accessList) {
    this.accessList = accessList;
  }

  private String name;
  private boolean notificationChange;
  private int notification;
  private boolean accessListChange;
  private String[] accessList;

  public SetParameterAttributesStruct() {

  }

  public SetParameterAttributesStruct(String name, boolean notificationChange, int notification,
      boolean accessListChange, String[] accessList) {
    this.name = name;
    this.notificationChange = notificationChange;
    this.notification = notification;
    this.accessList = accessList;
    this.accessListChange = accessListChange;
  }
}
