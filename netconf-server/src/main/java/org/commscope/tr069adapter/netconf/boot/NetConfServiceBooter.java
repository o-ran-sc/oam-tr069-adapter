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

package org.commscope.tr069adapter.netconf.boot;

import org.commscope.tr069adapter.netconf.server.NetConfServerManagerImpl;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.retry.annotation.EnableRetry;

@SpringBootApplication
@ComponentScan({"org.commscope.tr069adapter.netconf", "org.opendaylight.netconf.test"})
@EnableJpaRepositories("org.commscope.tr069adapter.netconf.dao")
@EntityScan("org.commscope.tr069adapter.netconf.entity")
@EnableRetry
public class NetConfServiceBooter {

  private static ApplicationContext appContext;

  public static void main(String[] args) {
    if (args != null)
      appContext = SpringApplication.run(NetConfServiceBooter.class, args);
    NetConfServerManagerImpl serverManager =
        NetConfServiceBooter.getApplicationContext().getBean(NetConfServerManagerImpl.class);
    serverManager.restartServers(); // restart all netconf servers during startup.
  }

  public static ApplicationContext getApplicationContext() {
    return appContext;
  }
}
