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

package org.commscope.tr069adapter.common.deviceversion;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class DeviceVersionManagerImpl implements DeviceVersionManager {
  private static final Logger LOG = LoggerFactory.getLogger(DeviceVersionManagerImpl.class);

  TreeMap<DeviceVersion, String> deviceVersionMap = new TreeMap<>();
  TreeMap<String, ProfileDefinition> profileDefinitionMap = new TreeMap<>();

  @PostConstruct
  public void loadProfileConfiguration() throws IOException {
    ObjectMapper objectMapper = new ObjectMapper();
    String contents;
    try (
        InputStream inputStream =
            getClass().getResourceAsStream("/profile-definition-mapping.json");
        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
      contents = reader.lines().collect(Collectors.joining(System.lineSeparator()));
    }

    ProfileDefinitions versions;
    try {
      versions = objectMapper.readValue(contents, ProfileDefinitions.class);

      List<ProfileDefinition> definitionList = versions.getProfileDefinition();
      for (ProfileDefinition definition : definitionList) {
        DeviceVersion versionDTO =
            new DeviceVersion(definition.getSoftwareVersion(), definition.getHardwareVersion());
        String profileId = definition.getProfileId();
        deviceVersionMap.put(versionDTO, profileId);
        profileDefinitionMap.put(profileId, definition);
      }
    } catch (IOException e) {
      LOG.info("context", e);

    }
  }



  @Override
  public String getNetconfYangSchemaPath(String swVersion, String hwVersion) {
    String profileId = getAssociatedProfileId(swVersion, hwVersion);
    ProfileDefinition profileDefinition = profileDefinitionMap.get(profileId);
    return profileDefinition.getNetConfSchemaPath();
  }

  @Override
  public String getBaseNetconfYangSchemaPath() {
    return profileDefinitionMap.firstEntry().getValue().getNetConfSchemaPath();
  }

  @Override
  public ProfileDefinition getProfileDefinition(String swVersion, String hwVersion) {
    String profileId = getAssociatedProfileId(swVersion, hwVersion);
    return profileDefinitionMap.get(profileId);
  }

  @Override
  public List<ProfileDefinition> getSupportedProfileDefinitions() {
    List<ProfileDefinition> proDeflist = new ArrayList<>();
    for (Iterator<String> iterator = profileDefinitionMap.keySet().iterator(); iterator
        .hasNext();) {
      String key = iterator.next();
      proDeflist.add(profileDefinitionMap.get(key));
    }
    return proDeflist;
  }

  @Override
  public String getAssociatedProfileId(String swVersion, String hwVersion) {
    String profileId = null;
    if (null != swVersion) // TODO: Consider hardware version also.
      profileId = getProfileName(deviceVersionMap, swVersion, hwVersion);

    if (profileId == null) {
      profileId = profileDefinitionMap.firstEntry().getValue().getProfileId();
    }

    return profileId;
  }

  private String getProfileName(TreeMap<DeviceVersion, String> deviceVersionMap, String swVersion,
      String hwVersion) {
    DeviceVersion deviceVersion = new DeviceVersion(swVersion, hwVersion, false, false);
    ArrayList<DeviceVersion> mSoftwareList = new ArrayList<>();

    for (Iterator<Entry<DeviceVersion, String>> iterator =
        deviceVersionMap.entrySet().iterator(); iterator.hasNext();) {
      Entry<DeviceVersion, String> entry = iterator.next();
      DeviceVersion profileVersion = entry.getKey();
      if (profileVersion.isHwRegex() || profileVersion.isSwRegex()) {
        if (profileVersion.isSwRegex()) {
          if (deviceVersion.getSwVersion().equalsIgnoreCase(profileVersion.getSwVersion())
              || deviceVersion.getSwVersion().matches(profileVersion.getSwVersion())) {
            if (profileVersion.isHwRegex()) {
              if (deviceVersion.getHwVersion() != null) {

                if ("*".equalsIgnoreCase(profileVersion.getHwVersion())
                    || deviceVersion.getHwVersion().equalsIgnoreCase(profileVersion.getHwVersion())
                    || deviceVersion.getHwVersion().matches(profileVersion.getHwVersion())) {
                  return entry.getValue();
                }
              }
            } else {
              // Check Strict match of Hardware
              if ("*".equalsIgnoreCase(profileVersion.getHwVersion())
                  || deviceVersion.getHwVersion().equalsIgnoreCase(profileVersion.getHwVersion())) {
                return entry.getValue();
              }
            }
          }
        } else if (profileVersion.isHwRegex()) {
          if (deviceVersion.getHwVersion() != null) {
            if ("*".equalsIgnoreCase(profileVersion.getHwVersion())
                || deviceVersion.getHwVersion().equalsIgnoreCase(profileVersion.getHwVersion())
                || deviceVersion.getHwVersion().matches(profileVersion.getHwVersion())) {
              // Add all software version which matching
              if (profileVersion.getSwVersion()
                  .compareToIgnoreCase(deviceVersion.getSwVersion()) <= 0) {
                mSoftwareList.add(profileVersion);
              }
            }
          }
        }
      } else {
        // Check Strict match of Hardware
        if ("*".equalsIgnoreCase(profileVersion.getHwVersion())
            || deviceVersion.getHwVersion().equalsIgnoreCase(profileVersion.getHwVersion())) {
          if (profileVersion.getSwVersion()
              .compareToIgnoreCase(deviceVersion.getSwVersion()) <= 0) {
            mSoftwareList.add(profileVersion);
          }
        }
      }
    }

    if (!mSoftwareList.isEmpty()) {
      // return the least matched software version profile
      Collections.sort(mSoftwareList, DeviceVersion.softwareComparator);
      return deviceVersionMap.get(mSoftwareList.get(mSoftwareList.size() - 1));
    }

    return null;
  }
}
