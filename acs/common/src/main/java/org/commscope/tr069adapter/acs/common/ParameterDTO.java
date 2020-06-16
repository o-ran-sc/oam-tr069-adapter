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

package org.commscope.tr069adapter.acs.common;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

import java.io.Serializable;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
public class ParameterDTO implements Serializable {

  private static final long serialVersionUID = 1460021542834472410L;

  private String paramName;
  private String paramValue;
  private String dataType;
  private boolean isProcessed;

  public ParameterDTO() {

  }

  public ParameterDTO(String paramName, String paramValue) {
    super();
    this.paramName = paramName;
    this.paramValue = paramValue;
  }

  public ParameterDTO(String paramName, String paramValue, String dataType) {
    super();
    this.paramName = paramName;
    this.paramValue = paramValue;
    this.dataType = dataType;
  }

  public String getParamName() {
    return paramName;
  }

  public void setParamName(String paramName) {
    this.paramName = paramName;
  }

  public String getParamValue() {
    return paramValue;
  }

  public void setParamValue(String paramValue) {
    this.paramValue = paramValue;
  }

  public String getDataType() {
    return dataType;
  }

  public void setDataType(String dataType) {
    this.dataType = dataType;
  }

  public boolean isProcessed() {
    return isProcessed;
  }

  public void setProcessed(boolean isProcessed) {
    this.isProcessed = isProcessed;
  }

  @Override
  public String toString() {
    return "ParameterDTO [paramName=" + paramName + ", paramValue=" + paramValue + ", dataType="
        + dataType + "]";
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    final ParameterDTO other = (ParameterDTO) obj;
    if (paramName == null) {
      if (other.paramName != null)
        return false;
    } else if (!paramName.equals(other.paramName)) {
      return false;
    }
    if (paramValue == null) {
      if (other.paramValue != null)
        return false;
    } else if (!paramValue.equals(other.paramValue)) {
      return false;
    }
    if (dataType == null) {
      if (other.dataType != null)
        return false;
    } else if (!dataType.equals(other.dataType)) {
      return false;
    }
    return true;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((paramName == null) ? 0 : paramName.hashCode());
    result = prime * result + ((paramValue == null) ? 0 : paramValue.hashCode());
    result = prime * result + ((dataType == null) ? 0 : dataType.hashCode());
    return result;
  }

}
