package org.commscope.tr069adapter.vesagent.timer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

/**
 * 
 * @version 1.0
 * @since June 5, 2020
 * @author Prashant Kumar
 */

@Configuration
public class ScheduleConfig {
  @Bean
  public TaskScheduler taskScheduler() {
    final ThreadPoolTaskScheduler scheduler = new ThreadPoolTaskScheduler();
    scheduler.setPoolSize(10);
    scheduler.setThreadNamePrefix("scheduled-task-pool-");
    return scheduler;
  }
}
