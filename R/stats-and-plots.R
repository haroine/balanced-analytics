library(tidyverse)
visits_fact_df <- readRDS("data/visits_fact_df.rds")

# Compute stats and plots to check properties
stats_per_day <- visits_fact_df %>% 
  group_by(Date) %>% 
  summarise(visits = n(), revenue = sum(daily_spend_mu), revenue_per_user = sum(daily_spend_mu)/n())

stats_per_day_toplot <- pivot_longer(stats_per_day 
                                     , cols = c(visits, revenue, revenue_per_user)
                                     , names_to = "metric"
                                     , values_to = "value")

stats_plot <- ggplot(data=stats_per_day_toplot %>% filter(metric != 'revenue'), aes(x=Date)) +
  geom_line(aes(y=value, color=metric)) +
  xlab('Date') +
  ylab('Metrics') +
  ggtitle('Metrics for the example dataset') +
  scale_color_manual(values = c('visits' = 'darkgrey', 'revenue_per_user' = 'skyblue')) +
  NULL
print(stats_plot)

revenue_plot <- ggplot(data=stats_per_day_toplot %>% filter(metric == 'revenue'), aes(x=Date)) +
  geom_line(aes(y=value, color=metric)) +
  xlab('Date') +
  ylab('Revenue') +
  ggtitle('Revenue for the example dataset') +
  scale_color_manual(values = c('revenue' = 'darkblue')) +
  NULL
print(revenue_plot)

# Trend: growth + revenue per users mechanically goes down bc newer users spend less
# 


#TODO : Is the effect visible in cohorts???