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

import java.util.List;

import org.commscope.tr069adapter.acs.common.ParameterDTO;

public class MapperValidator {
  private MapperValidator() {
    throw new IllegalStateException("Can't be instantiated as MapperValidator is utility class");
  }


  public static boolean isCountDownTimerValid(List<ParameterDTO> paramList) {
    for (ParameterDTO param : paramList) {
      if (Boolean.FALSE.equals(isNullOrEmpty(param.getParamName())) && param.getParamName()
          .toLowerCase().contains(MapperConstants.COUNT_DOWN_TIMER.toLowerCase())) {

        return !isNullOrEmpty(param.getParamValue())
            && param.getParamValue().equalsIgnoreCase(MapperConstants.COUNT_DOWN_TIMER_SET_VAL);
      }
    }
    return true;
  }



  public static boolean isNullOrEmpty(String object) {
    return (null == object || object.isEmpty());
  }

  public static Boolean isNullOrEmpty(List list) {
    return (null == list || list.isEmpty());
  }
}
