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

package org.commscope.tr069adapter.acs.common.dto;

import java.io.Serializable;
import java.util.List;

import org.commscope.tr069adapter.acs.common.OperationDetails;
import org.commscope.tr069adapter.acs.common.ParameterDTO;

public class TR069OperationDetails extends OperationDetails implements Serializable {

  private static final long serialVersionUID = -7690837012815378233L;

  private List<ParameterDTO> deleteParamList;
  private List<ParameterDTO> modifyParamList;
  private List<ParameterDTO> setParamList;

  public TR069OperationDetails() {
    super();
  }

  public TR069OperationDetails(List<ParameterDTO> deleteParamList,
      List<ParameterDTO> modifyParamList, List<ParameterDTO> setParamList) {
    this.deleteParamList = deleteParamList;
    this.modifyParamList = modifyParamList;
    this.setParamList = setParamList;
  }

  public List<ParameterDTO> getDeleteParamList() {
    return deleteParamList;
  }

  public void setDeleteParamList(List<ParameterDTO> deleteParamList) {
    this.deleteParamList = deleteParamList;
  }

  public List<ParameterDTO> getModifyParamList() {
    return modifyParamList;
  }

  public void setModifyParamList(List<ParameterDTO> modifyParamList) {
    this.modifyParamList = modifyParamList;
  }

  public List<ParameterDTO> getSetParamList() {
    return setParamList;
  }

  public void setSetParamList(List<ParameterDTO> setParamList) {
    this.setParamList = setParamList;
  }

}
