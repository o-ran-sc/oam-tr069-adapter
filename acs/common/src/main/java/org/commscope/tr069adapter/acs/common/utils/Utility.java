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


package org.commscope.tr069adapter.acs.common.utils;

import java.text.MessageFormat;
import java.util.ResourceBundle;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Utility {

  private Utility() {}

  private static final Logger logger = LoggerFactory.getLogger(Utility.class);

  private static final String ERROR_MESSAGE_PROPS = "config/ErrorMessages_tr069";

  private static ResourceBundle resourceBundle = ResourceBundle.getBundle(ERROR_MESSAGE_PROPS);

  /**
   * Utility method for reading the error messages from the error message property file.
   * 
   * @param key - The key of the message
   * @param args - Arguments of the messages if any
   * @return
   * @throws ReplanExecutorFailedException
   */
  public static String getMessage(String key, String... args) {
    try {
      String result = resourceBundle.getString(key);
      if (args != null && args.length > 0) {
        MessageFormat messageForm = new MessageFormat(result);
        result = messageForm.format(args);
      }
      return result;
    } catch (Exception e) {
      logger.error("Unable to get the message for the given key {}", key);
      return "Unresolved key: " + key;
    }
  }

}
