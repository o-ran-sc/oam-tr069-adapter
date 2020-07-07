package org.commscope.tr069adapter.vesagent.async;

import java.util.concurrent.Executor;

import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * 
 * @version 1.0
 * @since June 12, 2020
 * @author Prashant Kumar
 */

@Configuration
@EnableAsync
public class AsyncThreadPoolConfig implements AsyncConfigurer {

  @Override
  public Executor getAsyncExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(7);
    executor.setMaxPoolSize(25);
    executor.setQueueCapacity(11);
    executor.setThreadNamePrefix("MyExecutor-");
    executor.initialize();
    return executor;
  }

  // @Override
  // public AsyncUncaughtExceptionHandler getAsyncUncaughtExceptionHandler() {
  // return new MyAsyncUncaughtExceptionHandler();
  // }
}
