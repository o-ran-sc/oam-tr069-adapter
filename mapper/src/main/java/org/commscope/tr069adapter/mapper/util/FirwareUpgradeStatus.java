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
