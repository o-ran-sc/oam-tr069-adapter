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
