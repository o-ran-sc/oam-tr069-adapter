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
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;

import java.io.Serializable;
import java.util.List;

@JsonTypeInfo(use = Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
public class OperationDetails implements Serializable {

  private static final long serialVersionUID = -3405001463223998757L;

  protected OperationCode opCode;

  private List<ParameterDTO> parmeters;

  public OperationCode getOpCode() {
    return opCode;
  }

  public void setOpCode(OperationCode opCode) {
    this.opCode = opCode;
  }

  public List<ParameterDTO> getParmeters() {
    return parmeters;
  }

  public void setParmeters(List<ParameterDTO> parmeters) {
    this.parmeters = parmeters;
  }

  @Override
  public String toString() {
    return "OperationDetails [opCode=" + opCode + ", parmeters=" + parmeters + "]";
  }

}
