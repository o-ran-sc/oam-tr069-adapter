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

package org.commscope.tr069adapter.vesagent.fault;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.commscope.tr069adapter.acs.common.ParameterDTO;
import org.commscope.tr069adapter.vesagent.InformEventData;
import org.commscope.tr069adapter.vesagent.exception.InvalidFaultOperationException;
import org.commscope.tr069adapter.vesagent.model.PnfRegEventFields;

public class Parser {

  private Log logger = LogFactory.getLog(Parser.class);

  public Map<String, ExpeditedEvent> parseFaultParams(List<ParameterDTO> parameters)
      throws InvalidFaultOperationException {
    Map<String, ExpeditedEvent> dmAlarmParameters = new HashMap<>();

    try {
      for (ParameterDTO parameter : parameters) {
        logger.debug("PARAM NAME " + parameter.getParamName());
        if (parameter.getParamName() == null || !isAlarmEvent(parameter.getParamName())) {
          continue;
        }

        String alarmParamMO = parameter.getParamName();
        String alarmParam = null;
        String alarmParamParentMoWithIndex = null;

        if (null != alarmParamMO) {
          alarmParam = alarmParamMO.substring(alarmParamMO.lastIndexOf('.') + 1);
          alarmParamParentMoWithIndex = alarmParamMO.substring(0, alarmParamMO.lastIndexOf('.'));
        }

        ExpeditedEvent expeditedEvent = null;
        if (dmAlarmParameters.containsKey(alarmParamParentMoWithIndex)) {
          expeditedEvent = dmAlarmParameters.get(alarmParamParentMoWithIndex);
        } else {
          expeditedEvent = new ExpeditedEvent();
          logger.debug("ADDING " + alarmParamParentMoWithIndex);
          dmAlarmParameters.put(alarmParamParentMoWithIndex, expeditedEvent);
        }

        expeditedEvent.parse(parameter, alarmParam);
      }
    } catch (NumberFormatException ex) {
      logger.error("Error while parsing alarm event parameter {}", ex);
      throw new InvalidFaultOperationException(ex.getMessage());
    } catch (Exception e) {
      logger.error("Error occurred while parsing alarm event notification");
      throw new InvalidFaultOperationException(e.getMessage());
    }

    return dmAlarmParameters;
  }

  public PnfRegEventFields parseNotificationParams(List<ParameterDTO> parameters) {
    InformEventData feild = new InformEventData();
    try {
      for (ParameterDTO parameter : parameters) {

        if (isAlarmEvent(parameter.getParamName())) {
          continue;
        }

        feild.parse(parameter, parameter.getParamName());
      }
    } catch (Exception e) {
      logger.error("Error occurred while parsing alarm event notification");
    }

    return feild.getFeilds();
  }

  public static boolean isAlarmEvent(String str) {
    return (str.contains(".FaultMgmt.ExpeditedEvent.")
        || str.startsWith("InternetGatewayDevice.X_0005B9_FaultStatus"));
  }
}
