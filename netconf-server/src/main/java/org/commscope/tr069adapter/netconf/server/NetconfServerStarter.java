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

package org.commscope.tr069adapter.netconf.server;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.commscope.tr069adapter.common.deviceversion.DeviceVersionManager;
import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.commscope.tr069adapter.netconf.operations.CustomOperationsCreator;
import org.opendaylight.netconf.test.tool.NetconfDeviceSimulator;
import org.opendaylight.netconf.test.tool.config.Configuration;
import org.opendaylight.netconf.test.tool.config.ConfigurationBuilder;
import org.opendaylight.netconf.test.tool.operations.OperationsCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;
import com.google.common.base.Preconditions;


@Component
@Scope("singleton")
public class NetconfServerStarter {

  private static final Logger LOG = LoggerFactory.getLogger(NetconfServerStarter.class);

  private static Map<String, NetconfDevice> serversMap = new HashMap<>();

  @Autowired
  NetConfServerProperties config;

  @Autowired
  DeviceVersionManager versionManager;

  public boolean startServer(String netConfPort, String macID, String swVersion, String hwVersion) {

    if (netConfPort == null) {
      LOG.error("Invalid NETCONF port for deviceID: {}, port is null.", macID);
      return false;
    }

    LOG.debug(
        "Starting Netconf server for MACID :{}, on port :{}, softwareVersion {}, hardwareVersion {}",
        macID, netConfPort, swVersion, hwVersion);
    boolean result =
        startServer(netConfPort, config.getSchemaDirPath(), macID, swVersion, hwVersion);
    LOG.debug("Completed starting Netconf server for MACID :{} , on port :{}, server status={}",
        macID, netConfPort, result);

    return result;
  }

  @SuppressWarnings({"resource", "deprecation"})
  private boolean startServer(String portStr, String schemaDirPath, String macID, String swVersion,
      String hwVersion) {

    // creating configuration for the netconf instance
    final Configuration configuration = new ConfigurationBuilder().build();
    OperationsCreator operationsCreator = new CustomOperationsCreator(macID, swVersion, hwVersion);
    configuration.setOperationsCreator(operationsCreator);
    configuration.setGenerateConfigsTimeout((int) TimeUnit.MINUTES.toMillis(30));
    if (portStr != null) {
      try {
        int port = Integer.parseInt(portStr);
        configuration.setStartingPort(port);
      } catch (Exception e) {
        LOG.error("Failed to get netconf service instance port for parameters {}", e.toString());
        return false;
      }
    }
    configuration.setDeviceCount(1);
    configuration.setSsh(Boolean.TRUE);
    configuration.setCapabilities(Configuration.DEFAULT_BASE_CAPABILITIES_EXI);
    configuration.setIp("0.0.0.0");

    String versionPath = versionManager.getNetconfYangSchemaPath(swVersion, hwVersion);
    if (versionPath == null && swVersion != null) {
      LOG.error("Failed to get version path for software version {}, calling base version",
          swVersion);
      versionPath = versionManager.getBaseNetconfYangSchemaPath();
    } else if (swVersion == null) {
      LOG.error("Software version is null {}", swVersion);
      return false;
    }
    String schemaVerPath = schemaDirPath + File.separator + versionPath;
    File schemaVerDir = new File(schemaVerPath);
    configuration.setSchemasDir(schemaVerDir);

    try (final NetconfDevice netconfDevice = new NetconfDevice(configuration)) {
      final List<Integer> openDevices = netconfDevice.start();
      if (openDevices.isEmpty()) {
        LOG.debug("Failed to start netconf server instance {}", macID);
        return false;
      }
      netconfDevice.setAutoClose(false);
      serversMap.put(macID, netconfDevice);
    } catch (RuntimeException e) {
      LOG.error("Unhandled exception. Failed to start the server", e);
      return false;
    }

    return true;
  }

  public boolean stopServer(String macID) {
    try {
      LOG.debug("Stopping Netconf server for MACID {}", macID);
      NetconfDevice netconf = serversMap.get(macID);
      netconf.setAutoClose(true);
      netconf.close();
      serversMap.remove(macID);
      LOG.debug("Completed stopping Netconf server for MACID {}", macID);
      return true;
    } catch (Exception e) {
      LOG.debug("Error while stopping Netconf server for MACID {}; error message {}", macID,
          e.getMessage());
    }

    return false;
  }

  protected boolean loadSchemas(File schemasDir) {
    if (schemasDir != null) {
      if (!schemasDir.exists() || !schemasDir.isDirectory() || !schemasDir.canRead()) {
        LOG.error("Failed to load schema. schema location is not valid.");
        return false;
      }

      Pattern yangregex = Pattern.compile("(?<name>.*)@(?<revision>\\d{4}-\\d{2}-\\d{2})\\.yang");
      Pattern revisionregex = Pattern.compile("revision\\s+\"?(\\d{4}-\\d{2}-\\d{2})\"?");

      final File[] filesArray = schemasDir.listFiles();
      final List<File> files =
          filesArray != null ? Arrays.asList(filesArray) : Collections.emptyList();
      for (final File file : files) {
        final Matcher yangMatcher = yangregex.matcher(file.getName());
        if (!yangMatcher.matches()) {
          try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            String line = reader.readLine();
            while (line != null && !revisionregex.matcher(line).find()) {
              line = reader.readLine();
            }
            loadSchemaPattren(line, file, revisionregex);
          } catch (final IOException e) {
            LOG.error("Unhandled exception. Failed to load the schema.{}", e.toString());
            return false;
          }
        }
      }
    }
    return true;
  }

  public boolean isNetConfServerRunning(String deviceId) {
    NetconfDevice nc = serversMap.get(deviceId);
    if (null != nc)
      return true;
    else
      return false;
  }

  private void loadSchemaPattren(String line, File file, Pattern revisionregex) {
    if (line != null) {
      final Matcher m = revisionregex.matcher(line);
      Preconditions.checkState(m.find());
      String moduleName = file.getAbsolutePath();
      if (file.getName().endsWith(".yang")) {
        moduleName = moduleName.substring(0, moduleName.length() - 5);
      }
      final String revision = m.group(1);
      final String correctName = moduleName + "@" + revision + ".yang";
      final File correctNameFile = new File(correctName);
      if (!file.renameTo(correctNameFile)) {
        throw new IllegalStateException("Failed to rename '%s'." + file);
      }
    }
  }

}


class NetconfDevice extends NetconfDeviceSimulator {
  boolean autoClose = true;

  public NetconfDevice(Configuration configuration) {
    super(configuration);
  }

  @Override
  public void close() {
    if (autoClose)
      super.close();
  }

  public void setAutoClose(boolean autoClose) {
    this.autoClose = autoClose;
  }

}
