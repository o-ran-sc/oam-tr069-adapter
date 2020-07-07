package org.commscope.tr069adapter.mapper.dao;

import org.commscope.tr069adapter.mapper.entity.DeviceOperationDetails;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface DeviceOperationsDAO extends CrudRepository<DeviceOperationDetails, Long> {

  public DeviceOperationDetails findByDeviceId(String deviceId);
}
