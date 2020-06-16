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

package org.commscope.tr069adapter.mapper;

import java.io.Serializable;

public class MOMetaData implements Serializable {

  private static final long serialVersionUID = -2772256485667214776L;

  private String moName;
  private String dataType;

  private boolean isReadOnly;
  private boolean isTabluar;
  private boolean isTabluarObj;
  private String uri;

  public MOMetaData() {}

  public MOMetaData(String moName, String dataType, boolean isReadOnly, boolean isTabluar,
      boolean isTabluarObj) {
    super();
    this.moName = moName;
    this.dataType = dataType;
    this.isReadOnly = isReadOnly;
    this.isTabluar = isTabluar;
    this.isTabluarObj = isTabluarObj;
  }

  public String getMoName() {
    return moName;
  }

  public void setMoName(String moName) {
    this.moName = moName;
  }

  public String getDataType() {
    return dataType;
  }

  public void setDataType(String dataType) {
    this.dataType = dataType;
  }

  public boolean isReadOnly() {
    return isReadOnly;
  }

  public void setReadOnly(boolean isReadOnly) {
    this.isReadOnly = isReadOnly;
  }

  public boolean isTabluar() {
    return isTabluar;
  }

  public void setTabluar(boolean isTabluar) {
    this.isTabluar = isTabluar;
  }

  public boolean isTabluarObj() {
    return isTabluarObj;
  }

  public void setTabluarObj(boolean isTabluarObj) {
    this.isTabluarObj = isTabluarObj;
  }

  public String getURI() {
    return uri;
  }

  public void setURI(String uRI) {
    uri = uRI;
  }

  @Override
  public String toString() {
    return "MOMetaData [moName=" + moName + ", dataType=" + dataType + ", isReadOnly=" + isReadOnly
        + ", isTabluar=" + isTabluar + "]";
  }

}
