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
