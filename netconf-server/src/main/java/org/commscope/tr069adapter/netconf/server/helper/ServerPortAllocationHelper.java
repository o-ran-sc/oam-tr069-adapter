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

package org.commscope.tr069adapter.netconf.server.helper;

import java.io.IOException;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.concurrent.Semaphore;

import javax.annotation.PostConstruct;

import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.commscope.tr069adapter.netconf.error.ServerPortAllocationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ServerPortAllocationHelper {

  private static final Logger LOG = LoggerFactory.getLogger(ServerPortAllocationHelper.class);

  private static Map<String, Semaphore> semaphoreMap = new HashMap<>();

  private PriorityQueue<String> availablePorts = new PriorityQueue<>();

  @Autowired
  NetConfServerProperties config;

  @PostConstruct
  public void init() {
    // read the port range and it the available ports.

    Integer startPort = config.getDefaultNetconfStartPort();
    Integer maxServers = config.getDefaultMaxServers();

    try {
      startPort = Integer.parseInt(config.getNetconfServersStartPort());
    } catch (Exception e) {
      LOG.warn(
          "Failed to initialize the starting port from the environment {}. Hence using the default port range.",
          config.getNetconfServersStartPort());
    }

    try {
      maxServers = Integer.parseInt(config.getMaxNumOfNetconfServers());
    } catch (Exception e) {
      LOG.warn(
          "Failed to initialize the max netconf server from the environment {} Hence using the default max servers.",
          config.getMaxNumOfNetconfServers());
    }

    for (int i = startPort + maxServers - 1; i >= startPort; i--) {
      semaphoreMap.put(String.valueOf(i), new Semaphore(1));
      availablePorts.add(String.valueOf(i));
    }
    LOG.debug("Successfully populated available ports list.");
  }

  public String reserveServerPort() throws ServerPortAllocationException {

    if (availablePorts.isEmpty()) {
      LOG.debug(
          "All ports are exhausted. Hence cannot allocate a port to start new netconf server.");
    }

    String port = availablePorts.peek();

    LOG.debug("Trying to reserve port : {}", port);
    if (isServerPortInUse(port)) {
      LOG.debug("Port {} is already in use.", port);
      return reserveServerPort(); // retry if current port is not available
    }

    Semaphore semaphore = semaphoreMap.get(port);
    boolean isAcquired = semaphore.tryAcquire();
    if (isAcquired) {
      LOG.debug("Failed to acquire a lock for port :{}. Hence retrying...", port);
      return reserveServerPort();
    }

    availablePorts.poll();
    semaphore.release();
    LOG.debug("Rserved port is {}", port);
    return port;
  }

  public boolean checkAndReserveServerPort(String port) {

    try {
      Semaphore semaphore = semaphoreMap.get(port);
      semaphore.acquire();
      if (isServerPortInUse(port)) {
        LOG.error("Port {}  already in use.", port);
        semaphore.release();
        return false;
      }
      availablePorts.remove(port);
      semaphore.release();
      LOG.error("Successfully reserved the port {} to start netconf server", port);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      LOG.error("Failed to lock the port {} : Exception :{}", port, e.toString());
      return checkAndReserveServerPort(port); // retry acquiring the lock.
    }

    return true;
  }

  public boolean isServerPortInUse(String port) {
    return checkIfPortAvailable(port);
  }

  private static boolean checkIfPortAvailable(String portStr) {
    Integer port = Integer.parseInt(portStr);
    try (Socket ignored = new Socket("localhost", port)) {
      return true;
    } catch (IOException e) {
      LOG.error("while checkIfPortAvailable {}", e.toString());
      return false;
    }
  }
}
