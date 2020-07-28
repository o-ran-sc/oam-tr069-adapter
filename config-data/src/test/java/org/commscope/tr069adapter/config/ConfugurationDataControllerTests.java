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

package org.commscope.tr069adapter.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;
import org.commscope.tr069adapter.config.repository.ConfigurationDataRepository;
import org.hamcrest.CoreMatchers;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.RequestBuilder;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = {ConfigDataServiceApplication.class}) // , args = "--schemas-dir
                                                                // test-schemas --debug true
                                                                // --starting-port 17830")
@AutoConfigureMockMvc
public class ConfugurationDataControllerTests {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  ConfigurationDataRepository configDataRepository;

  @Test
  public void getMessageTest() {
    RequestBuilder requestBuilder =
        MockMvcRequestBuilders.get("/isActive").accept(MediaType.APPLICATION_JSON);

    MvcResult result = null;
    String resultString = null;
    try {
      result = mockMvc.perform(requestBuilder).andReturn();
      resultString = result.getResponse().getContentAsString();
    } catch (Exception e) {
      fail(e.getMessage());
    }

    assertEquals("Application is running", resultString);
  }

  @Test
  public void uploadMultipleFilesTest() {
    MockMultipartFile multiFile =
        new MockMultipartFile("files", ConfigDataTestsUtils.CONFIG_FILE_NAME,
            MediaType.APPLICATION_XML_VALUE, ConfigDataTestsUtils.getFileContent().getBytes());
    MockHttpServletRequestBuilder requestBuilder =
        MockMvcRequestBuilders.multipart("/importConfig").file(multiFile);// .contentType(MediaType.MULTIPART_FORM_DATA_VALUE);

    MvcResult result = null;
    String resultString = null;
    try {
      result = mockMvc.perform(requestBuilder).andReturn();
      MockHttpServletResponse response = result.getResponse();
      resultString = response.getContentAsString();
    } catch (Exception e) {
      fail(e.getMessage());
    }

    String expectedResult =
        "File " + ConfigDataTestsUtils.CONFIG_FILE_NAME + " imported successfully";
    assertEquals(expectedResult, resultString);
  }

  @Test
  public void viewConfigurationDataTest() {
    Mockito.when(configDataRepository.findByMacId(ConfigDataTestsUtils.macId))
        .thenReturn(ConfigDataTestsUtils.getConfigFileContent());

    MockHttpServletRequestBuilder requestBuilder =
        MockMvcRequestBuilders.post("/getConfig").param("macId", "0005B95196D0")
            .param("swVersion", "4.5").param("hwVersion", "1.1").accept(MediaType.APPLICATION_JSON);

    MvcResult result = null;
    try {
      result = mockMvc.perform(requestBuilder).andReturn();
      MockHttpServletResponse response = result.getResponse();
      assertEquals(200, response.getStatus());
    } catch (Exception e) {
      fail(e.getMessage());
    }
  }
}
