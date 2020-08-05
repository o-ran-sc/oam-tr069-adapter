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

import java.util.Arrays;
import javax.jms.ConnectionFactory;
import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.RedeliveryPolicy;
import org.apache.activemq.broker.BrokerService;
import org.apache.activemq.broker.region.policy.RedeliveryPolicyMap;
import org.apache.activemq.command.ActiveMQQueue;
import org.commscope.tr069adapter.netconf.server.NetConfServerManagerImpl;
import org.commscope.tr069adapter.netconf.server.utils.NetConfServerConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.autoconfigure.jms.DefaultJmsListenerContainerFactoryConfigurer;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jms.annotation.EnableJms;
import org.springframework.jms.config.DefaultJmsListenerContainerFactory;
import org.springframework.jms.config.JmsListenerContainerFactory;
import org.springframework.jms.listener.MessageListenerContainer;
import org.springframework.jms.support.converter.MappingJackson2MessageConverter;
import org.springframework.jms.support.converter.MessageConverter;
import org.springframework.jms.support.converter.MessageType;
import org.springframework.retry.annotation.EnableRetry;

@EnableJms
@SpringBootApplication
@ComponentScan({"org.commscope.tr069adapter.netconf", "org.opendaylight.netconf.test",
    "org.commscope.tr069adapter.common"})
@EnableJpaRepositories("org.commscope.tr069adapter.netconf.dao")
@EntityScan("org.commscope.tr069adapter.netconf.entity")
@EnableRetry
public class NetConfServiceBooter {

  private static final Logger LOG = LoggerFactory.getLogger(NetConfServiceBooter.class);

  private static ApplicationContext appContext;

  public static void main(String[] args) {
    if (args != null)
      appContext = SpringApplication.run(NetConfServiceBooter.class, args);
    NetConfServerManagerImpl serverManager =
        NetConfServiceBooter.getApplicationContext().getBean(NetConfServerManagerImpl.class);
    boolean isSchemaLoaded = serverManager.loadSchemas();
    if (!isSchemaLoaded) {
      LOG.error("Loading the schema failed while starting the container");
      System.exit(1);
    }
    serverManager.restartServers();
  }

  public static ApplicationContext getApplicationContext() {
    return appContext;
  }

  /*
   * JMS Configuration Defining the connection factories used in the application Setting the
   * Re-delivery configuration goes here
   */
  @Bean
  public BrokerService broker() throws Exception {
    final BrokerService broker = new BrokerService();
    broker.addConnector("tcp://localhost:61616");
    broker.addConnector("vm://localhost");
    broker.setPersistent(false);
    return broker;
  }

  @Bean
  public ConnectionFactory jmsConnectionFactory() {
    ActiveMQConnectionFactory connectionFactory = new ActiveMQConnectionFactory();
    connectionFactory
        .setTrustedPackages(Arrays.asList("org.commscope", "org.commscope.tr069adapter"));
    connectionFactory.setMaxThreadPoolSize(7);

    ActiveMQQueue notificationQueue = new ActiveMQQueue(NetConfServerConstants.NETCONF_NOTIFICATION_Q);
    RedeliveryPolicy notificationQueuePolicy = new RedeliveryPolicy();
    notificationQueuePolicy.setInitialRedeliveryDelay(2* 60 * 1000L);
    notificationQueuePolicy.setUseCollisionAvoidance(true);
    notificationQueuePolicy.setRedeliveryDelay(2* 60 * 1000L);
    notificationQueuePolicy.setUseExponentialBackOff(false);
    notificationQueuePolicy.setMaximumRedeliveries(3);
    notificationQueuePolicy.setDestination(notificationQueue);

    RedeliveryPolicyMap rdMap = connectionFactory.getRedeliveryPolicyMap();
    rdMap.put(notificationQueue, notificationQueuePolicy);
    return connectionFactory;
  }

  @Bean
  public JmsListenerContainerFactory<MessageListenerContainer> netConfNotificationCF(
      ConnectionFactory connectionFactory,
      DefaultJmsListenerContainerFactoryConfigurer configurer) {
    return handleJMSCommonConfiguration(connectionFactory, configurer);
  }

  public JmsListenerContainerFactory handleJMSCommonConfiguration(
      ConnectionFactory connectionFactory,
      DefaultJmsListenerContainerFactoryConfigurer configurer) {
    DefaultJmsListenerContainerFactory factory = new DefaultJmsListenerContainerFactory();
    configurer.configure(factory, connectionFactory);
    return factory;
  }

  @Bean
  public MessageConverter jacksonJmsMessageConverter() {
    MappingJackson2MessageConverter converter = new MappingJackson2MessageConverter();
    converter.setTargetType(MessageType.TEXT);
    converter.setTypeIdPropertyName("_type");
    return converter;
  }
}
