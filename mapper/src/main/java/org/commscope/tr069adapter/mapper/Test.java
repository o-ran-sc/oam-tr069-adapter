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

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.commscope.tr069adapter.acs.common.DeviceRPCRequest;
import org.commscope.tr069adapter.acs.common.dto.TR069OperationCode;
import org.commscope.tr069adapter.mapper.util.MOMetaDataUtil;
import org.commscope.tr069adapter.mapper.util.NetconfToTr069MapperUtil;
import org.w3c.dom.Element;

public class Test {

  public static final String INDEX_STR = "index";
  public static final String INDEX_REGEX = "[0-9]{1,}";

  public static void main(String[] args) throws IOException {
    File file = new File("rpc_set_input.xml");
    String requestXML = FileUtils.readFileToString(file, "UTF-8");


    StringBuilder buff = new StringBuilder();
    for (int i = 6400; i <= 9600; i++) {
      buff.append(i).append(",");
    }
    System.out.println(buff.toString());

    MOMetaDataUtil util = new MOMetaDataUtil();
    util.loadMetaData();
    Element el = NetconfToTr069MapperUtil.convertStringToDocument(requestXML);

    DeviceRPCRequest req = NetconfToTr069MapperUtil.prepareTR069Request("0005B9AAAAA3", el, "rpc",
        TR069OperationCode.GET_PARAMETER_VALUES);
    req.getOpDetails()
        .setParmeters(util.getSupportedChildParameters(req.getOpDetails().getParmeters()));

    System.out.println(req);

  }

}
