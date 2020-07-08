package org.commscope.tr069adapter.vesagent.repository;

import java.util.List;

import org.commscope.tr069adapter.vesagent.entity.DeviceDataEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

/**
 * 
 * @version 1.0
 * @since June 10, 2020
 * @author Prashant Kumar
 */

@Repository
public interface VesDataRepository extends CrudRepository<DeviceDataEntity, Long> {
  public List<DeviceDataEntity> findByDeviceId(String deviceId);

  public List<DeviceDataEntity> findByDeviceIdAndAttrGroup(String deviceId, String attrGroup);

  public List<DeviceDataEntity> findByAttrGroup(String attrGroup);

}
