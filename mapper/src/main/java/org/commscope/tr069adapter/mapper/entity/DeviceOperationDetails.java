package org.commscope.tr069adapter.mapper.entity;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

@Entity
@Table(name = "device_operation_details",
    uniqueConstraints = @UniqueConstraint(columnNames = {"DEVICE_ID"}))
public class DeviceOperationDetails implements Serializable {
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  @Column(name = "DEVICE_ID", length = 30)
  private String deviceId;

  @Column(name = "SW_VERSION", length = 30)
  private String swVersion;

  @Column(name = "DOWN_LOAD_STATUS")
  private int downLoadStatus;

  @Column(name = "FIRMWARE_FILE", length = 1024)
  private String fileName;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getDeviceId() {
    return deviceId;
  }

  public void setDeviceId(String deviceId) {
    this.deviceId = deviceId;
  }

  public String getSwVersion() {
    return swVersion;
  }

  public void setSwVersion(String swVersion) {
    this.swVersion = swVersion;
  }

  public int getDownLoadStatus() {
    return downLoadStatus;
  }

  public void setDownLoadStatus(int downLoadStatus) {
    this.downLoadStatus = downLoadStatus;
  }

  public String getFileName() {
    return fileName;
  }

  public void setFileName(String fileName) {
    this.fileName = fileName;
  }
}
