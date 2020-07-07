package org.commscope.tr069adapter.mapper.util;

public enum FirwareUpgradeStatus {
  NOT_STARTED(0), DOWNLOAD_INTIATED(1), DOWNLOAD_FAILED(2), DOWNLOAD_COMPLETED(3), ACTIVATION_ERROR(
      4), ACTIVATION_COMPLETED(5);

  private int status;

  private FirwareUpgradeStatus(int status) {
    this.status = status;
  }

  public int getStatus() {
    return status;
  }
}
