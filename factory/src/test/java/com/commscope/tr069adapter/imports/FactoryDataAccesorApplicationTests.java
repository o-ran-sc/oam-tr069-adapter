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

package com.commscope.tr069adapter.imports;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.commscope.tr069adapter.factory.FactoryDataAccesorApplication;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.RequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = {FactoryDataAccesorApplication.class})
@AutoConfigureMockMvc
public class FactoryDataAccesorApplicationTests {

  private String exampleDeviceDataJson =
      "{\"serialNumber\":\"0005B95196D0\",\"autenticationString\":\"abcxyz\",\"oui\":\"0005B9\",\"productClass\":\"LTE_Enterprise_C-RANSC_Cntrl\"}";

  @Autowired
  private MockMvc mockMvc;

  @Test
  public void basicAuthenticateTest() {

    RequestBuilder requestBuilder =
        MockMvcRequestBuilders.post("/basicAuthenticate").accept(MediaType.APPLICATION_JSON)
            .content(exampleDeviceDataJson).contentType(MediaType.APPLICATION_JSON);

    MvcResult result = null;
    String resultString = null;
    try {
      result = mockMvc.perform(requestBuilder).andReturn();
      resultString = result.getResponse().getContentAsString();
    } catch (Exception e) {
      fail(e.getMessage());
    }

    assertEquals("true", resultString);
  }

  @Test
  public void digestAuthenticateDeviceTest() {

    RequestBuilder requestBuilder =
        MockMvcRequestBuilders.post("/digestAuthenticate").accept(MediaType.APPLICATION_JSON)
            .content(exampleDeviceDataJson).contentType(MediaType.APPLICATION_JSON);

    MvcResult result = null;
    String resultString = null;
    try {
      result = mockMvc.perform(requestBuilder).andReturn();
      resultString = result.getResponse().getContentAsString();
    } catch (Exception e) {
      fail(e.getMessage());
    }

    assertEquals("true", resultString);
  }

  @Test
  public void validateDeviceOUIPCTest() {
    RequestBuilder requestBuilder =
        MockMvcRequestBuilders.post("/validateDevice").accept(MediaType.APPLICATION_JSON)
            .content(exampleDeviceDataJson).contentType(MediaType.APPLICATION_JSON);

    MvcResult result = null;
    String resultString = null;
    try {
      result = mockMvc.perform(requestBuilder).andReturn();
      resultString = result.getResponse().getContentAsString();
    } catch (Exception e) {
      fail(e.getMessage());
    }

    assertEquals("true", resultString);
  }
}
