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

package org.commscope.tr069adapter.acs.requestprocessor.dao;

import java.util.List;

import org.commscope.tr069adapter.acs.requestprocessor.entity.TR069DeviceRPCRequestEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface DeviceRPCRequestRepository
    extends CrudRepository<TR069DeviceRPCRequestEntity, Long> {

  public List<TR069DeviceRPCRequestEntity> findByDeviceId(String deviceId);

  public List<TR069DeviceRPCRequestEntity> findByDeviceIdAndOperationId(String deviceId,
      Long operationId);

  public List<TR069DeviceRPCRequestEntity> findByDeviceIdAndIsProcessed(String deviceId,
      Integer isProcessed);

}
