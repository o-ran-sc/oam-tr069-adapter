package org.commscope.tr069adapter.vesagent.timer;

import java.util.function.Function;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

/**
 * 
 * @version 1.0
 * @since June 5, 2020
 * @author Prashant Kumar
 */

@Configuration
public class ServiceConfig {
  @Bean
  public Function<String, HeartBeatTimeoutTask> myPrototypeFactory() {
    return arg -> getBeanInstance(arg);
  }

  @Bean
  @Scope(value = "prototype")
  public HeartBeatTimeoutTask getBeanInstance(String arg) {
    return new HeartBeatTimeoutTask(arg);
  }
}
