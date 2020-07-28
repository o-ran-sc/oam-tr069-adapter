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

package org.commscope.tr069adapter.common.deviceversion;

import java.io.Serializable;
import java.util.Comparator;

public class DeviceVersion implements Serializable, Comparable<DeviceVersion> {

  /**
   * 
   */
  private static final long serialVersionUID = -7251276716604249440L;
  private int svMajorVersion = 0;
  private int svMinorVersion = 0;
  private int svPatchVersion = 0;
  private String swVersion;
  private boolean isGenericVersion = false;
  private boolean isHwRegex = false;
  private boolean isSwRegex = false;

  public DeviceVersion(String swVersion, String hwVersion) {
    super();
    setSwVersion(swVersion);
    this.hwVersion = hwVersion;
  }

  public DeviceVersion(String swVersion, String hwVersion, boolean isSwRegex,
      boolean isHwRegex) {
    super();
    this.hwVersion = hwVersion;
    this.swVersion = swVersion;
    this.isHwRegex = isHwRegex;
    this.isSwRegex = isSwRegex;
    if (!isSwRegex) {
      setSwVersion(swVersion);
    }
  }

  public String getSwVersion() {
    if (!isSwRegex)
      return svMajorVersion + "." + svMinorVersion + "." + svPatchVersion;
    else
      return this.swVersion;
  }

  public boolean isHwRegex() {
    return isHwRegex;
  }

  public boolean isSwRegex() {
    return isSwRegex;
  }

  private void setSwVersion(String swVersion) {
    if (swVersion.indexOf(".") > 0) {
      String[] verArray = swVersion.split("\\.");

      for (int i = 0; i < verArray.length; i++) {

        if (verArray[i].equals("*")) {
          verArray[i] = "0";
        }
      }
      try {
        svMajorVersion = Integer.parseInt(verArray[0]);
        svMinorVersion = Integer.parseInt(verArray[1]);
        svPatchVersion = Integer.parseInt(verArray[2]);
      } catch (Exception e) {
        // TODO: handle exception
      }

    } else if (swVersion.indexOf("x") > 0) {
      swVersion = "*";
    } else if (swVersion.equals("*")) {
      isGenericVersion = true;
    }

  }

  public String getHwVersion() {
    return hwVersion;
  }

  private String hwVersion;

  public int getSvMajorVersion() {
    return svMajorVersion;
  }

  public int getSvMinorVersion() {
    return svMinorVersion;
  }

  public int getSvPatchVersion() {
    return svPatchVersion;
  }

  private long deviceTypeId;

  public long getDeviceTypeId() {
    return deviceTypeId;
  }

  public boolean isGenericVersion() {
    return isGenericVersion;
  }

  public static Comparator<DeviceVersion> softwareComparator = new Comparator<DeviceVersion>() {
    @Override
    public int compare(DeviceVersion d1, DeviceVersion d2) {
      if (d1.getSvMajorVersion() != d2.getSvMajorVersion()) {
        return (d1.getSvMajorVersion() - d2.getSvMajorVersion());
      } else if (d1.getSvMinorVersion() != d2.getSvMinorVersion()) {
        return d1.getSvMinorVersion() - d2.getSvMinorVersion();
      } else
        return d1.getSvPatchVersion() - d2.getSvPatchVersion();
    }
  };

  @Override
  public int compareTo(DeviceVersion o) {
    if (deviceTypeId != o.deviceTypeId)
      return -1;

    if (isSwRegex) {
      return this.hashCode() - o.hashCode();
    } else {
      if (svMajorVersion != o.svMajorVersion) {
        return (svMajorVersion - o.svMajorVersion);
      } else if (svMinorVersion != o.svMinorVersion) {
        return svMinorVersion - o.svMinorVersion;
      } else if (svPatchVersion != o.svPatchVersion) {
        return svPatchVersion - o.svPatchVersion;
      } else {
        return hwVersion.compareToIgnoreCase(o.hwVersion);
      }
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + (int) (deviceTypeId ^ (deviceTypeId >>> 32));
    result = prime * result + ((hwVersion == null) ? 0 : hwVersion.hashCode());
    result = prime * result + (isGenericVersion ? 1231 : 1237);
    result = prime * result + (isHwRegex ? 1231 : 1237);
    result = prime * result + (isSwRegex ? 1241 : 1247);
    result = prime * result + svMajorVersion;
    result = prime * result + svMinorVersion;
    result = prime * result + svPatchVersion;
    return result;
  }
}
