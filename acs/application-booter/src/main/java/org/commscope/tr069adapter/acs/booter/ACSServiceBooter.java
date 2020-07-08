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

package org.commscope.tr069adapter.acs.booter;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CR_REQ_Q;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.NBI_NOTIFICATION_Q;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.NBI_OP_RESULT_Q;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.TR069_NBI_REQUEST_Q;

import java.util.Arrays;

import javax.jms.ConnectionFactory;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.RedeliveryPolicy;
import org.apache.activemq.broker.BrokerService;
import org.apache.activemq.broker.region.policy.RedeliveryPolicyMap;
import org.apache.activemq.command.ActiveMQQueue;
import org.apache.catalina.connector.Connector;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.autoconfigure.jms.DefaultJmsListenerContainerFactoryConfigurer;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.server.ConfigurableServletWebServerFactory;
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
import org.springframework.web.client.RestTemplate;

@EnableJms
@SpringBootApplication
@ComponentScan({"org.commscope.tr069adapter.acs", "org.commscope.tr069adapter.common"})
@EnableJpaRepositories("org.commscope.tr069adapter.acs")
@EntityScan("org.commscope.tr069adapter.acs.requestprocessor.entity")
public class ACSServiceBooter {

  public static void main(String[] args) {
    SpringApplication.run(ACSServiceBooter.class, args);
  }

  @Bean
  public RestTemplate restTemplate(RestTemplateBuilder builder) {
    return builder.build();
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

    ActiveMQQueue tr069nbiReqQueue = new ActiveMQQueue(TR069_NBI_REQUEST_Q);
    RedeliveryPolicy tr069nbiReqQueuePolicy = new RedeliveryPolicy();
    tr069nbiReqQueuePolicy.setInitialRedeliveryDelay(30000);
    tr069nbiReqQueuePolicy.setUseCollisionAvoidance(true);
    tr069nbiReqQueuePolicy.setRedeliveryDelay(30000);
    tr069nbiReqQueuePolicy.setUseExponentialBackOff(false);
    tr069nbiReqQueuePolicy.setMaximumRedeliveries(3);
    tr069nbiReqQueuePolicy.setDestination(tr069nbiReqQueue);

    ActiveMQQueue crQueue = new ActiveMQQueue(CR_REQ_Q);
    RedeliveryPolicy crQueuePolicy = new RedeliveryPolicy();
    crQueuePolicy.setInitialRedeliveryDelay(18000);
    crQueuePolicy.setUseCollisionAvoidance(true);
    crQueuePolicy.setRedeliveryDelay(18000);
    crQueuePolicy.setUseExponentialBackOff(true);
    crQueuePolicy.setBackOffMultiplier(2);
    crQueuePolicy.setMaximumRedeliveries(4);
    crQueuePolicy.setDestination(crQueue);

    ActiveMQQueue notificationQueue = new ActiveMQQueue(NBI_NOTIFICATION_Q);
    RedeliveryPolicy notificationQueuePolicy = new RedeliveryPolicy();
    notificationQueuePolicy.setInitialRedeliveryDelay(30000);
    notificationQueuePolicy.setUseCollisionAvoidance(true);
    notificationQueuePolicy.setRedeliveryDelay(30000);
    notificationQueuePolicy.setUseExponentialBackOff(false);
    notificationQueuePolicy.setMaximumRedeliveries(5);
    notificationQueuePolicy.setDestination(notificationQueue);

    ActiveMQQueue opResQueue = new ActiveMQQueue(NBI_OP_RESULT_Q);
    RedeliveryPolicy opResQueuePolicy = new RedeliveryPolicy();
    opResQueuePolicy.setInitialRedeliveryDelay(30000);
    opResQueuePolicy.setUseCollisionAvoidance(true);
    opResQueuePolicy.setRedeliveryDelay(30000);
    opResQueuePolicy.setUseExponentialBackOff(false);
    opResQueuePolicy.setMaximumRedeliveries(3);
    opResQueuePolicy.setDestination(opResQueue);

    RedeliveryPolicyMap rdMap = connectionFactory.getRedeliveryPolicyMap();
    rdMap.put(tr069nbiReqQueue, tr069nbiReqQueuePolicy);
    rdMap.put(crQueue, crQueuePolicy);
    rdMap.put(notificationQueue, notificationQueuePolicy);
    rdMap.put(opResQueue, opResQueuePolicy);

    return connectionFactory;
  }
  // End

  @Bean
  public JmsListenerContainerFactory<MessageListenerContainer> tr069NBIRequestCF(
      ConnectionFactory connectionFactory,
      DefaultJmsListenerContainerFactoryConfigurer configurer) {
    return handleJMSCommonConfiguration(connectionFactory, configurer);
  }

  @Bean
  public JmsListenerContainerFactory<MessageListenerContainer> tr069DeviceResponseCF(
      ConnectionFactory connectionFactory,
      DefaultJmsListenerContainerFactoryConfigurer configurer) {
    return handleJMSCommonConfiguration(connectionFactory, configurer);
  }

  @Bean
  public JmsListenerContainerFactory<MessageListenerContainer> nbiNotificationCF(
      ConnectionFactory connectionFactory,
      DefaultJmsListenerContainerFactoryConfigurer configurer) {
    return handleJMSCommonConfiguration(connectionFactory, configurer);
  }

  @Bean
  public JmsListenerContainerFactory<MessageListenerContainer> nbiOpResultCF(
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

  /*
   * JMS Configuration ends here
   */

  /*
   * JMS serializing and de-serializing used Jackson JMS message converter Serialize message content
   * to json using TextMessage
   */
  @Bean
  public MessageConverter jacksonJmsMessageConverter() {
    MappingJackson2MessageConverter converter = new MappingJackson2MessageConverter();
    converter.setTargetType(MessageType.TEXT);
    converter.setTypeIdPropertyName("_type");
    return converter;
  }

  /*
   * Exposing an addition HTTP port for Netconf mapper to access And making the default port to be
   * TLS secured which shall be used in the hems url
   */
  @Value("${server.additional.http.port}")
  private String additionalHttpPort;

  @Value("${server.port}")
  private String serverPort;

  @Bean
  public ConfigurableServletWebServerFactory tomcatEmbeddedServletContainerFactory() {
    final TomcatServletWebServerFactory factory = new TomcatServletWebServerFactory();
    final Connector connector = new Connector(TomcatServletWebServerFactory.DEFAULT_PROTOCOL);
    connector.setScheme("https");
    connector.setPort(Integer.valueOf(this.additionalHttpPort));
    factory.addAdditionalTomcatConnectors(connector);
    return factory;
  }

  @Bean
  public FilterRegistrationBean<FilterInsecuredRequests> loggingFilter() {
    FilterRegistrationBean<FilterInsecuredRequests> registrationBean =
        new FilterRegistrationBean<>();
    registrationBean.setFilter(new FilterInsecuredRequests(serverPort));
    registrationBean.addUrlPatterns("/CPEMgmt/*");
    return registrationBean;
  }

}
