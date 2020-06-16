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

import java.util.Date;

import org.commscope.tr069adapter.acs.common.dto.TR069DeviceDetails;
import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceEntity;

public class TR069NBIUtility {

  private TR069NBIUtility() {
    super();
  }

  /**
   * A utility method to convert the DTO to Domain object
   * 
   * @param tr069DeviceDetails
   * @return
   */

  public static TR069DeviceEntity convertToEntity(TR069DeviceDetails tr069DeviceDetails) {
    TR069DeviceEntity tr069DeviceEntity = new TR069DeviceEntity();
    tr069DeviceEntity.setDeviceId(tr069DeviceDetails.getDeviceId());
    tr069DeviceEntity.setUserName(tr069DeviceDetails.getUsername());
    tr069DeviceEntity.setPassword(tr069DeviceDetails.getPassword());
    tr069DeviceEntity.setLastUpdatedTime(new Date());

    if (tr069DeviceDetails.getConnectionRequestURL() != null) {
      tr069DeviceEntity.setConnectionReqURL(tr069DeviceDetails.getConnectionRequestURL());
    }
    if (tr069DeviceDetails.getSoftwareVersion() != null) {
      tr069DeviceEntity.setSwVersion(tr069DeviceDetails.getSoftwareVersion());
    }
    if (tr069DeviceDetails.getHardwareVersion() != null) {
      tr069DeviceEntity.setHwVersion(tr069DeviceDetails.getHardwareVersion());
    }

    return tr069DeviceEntity;
  }

  /**
   * A utility method to convert the Domain object to DTO
   * 
   * @param entity
   * @return
   */
  public static TR069DeviceDetails convertToDTO(TR069DeviceEntity entity) {
    TR069DeviceDetails tr069DeviceDetails = new TR069DeviceDetails();

    tr069DeviceDetails.setDeviceId(entity.getDeviceId());
    tr069DeviceDetails.setUsername(entity.getUserName());
    tr069DeviceDetails.setPassword(entity.getPassword());
    tr069DeviceDetails.setSoftwareVersion(entity.getSwVersion());
    tr069DeviceDetails.setHardwareVersion(entity.getHwVersion());
    tr069DeviceDetails.setConnectionRequestURL(entity.getConnectionReqURL());

    return tr069DeviceDetails;
  }

}
