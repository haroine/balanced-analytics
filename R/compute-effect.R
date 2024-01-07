library(tidyverse)
library(lubridate)
source('R/parameters.R')


wCal_df <- readRDS("data/weights_balanced.rds")
visits_fact_df <- readRDS("data/visits_fact_df.rds") %>% 
  left_join(wCal_df, by=c("day", "Date", "user_id"))

source('R/stats-and-plots.R')

## Compute the effect
stats_per_day_weighted_BEFORE_prep <- stats_per_day_weighted %>% 
  ungroup()
stats_per_day_weighted_BEFORE <- stats_per_day_weighted_BEFORE_prep %>% 
  filter(day < DATE_SHIP_NEW_FEATURE)

lm_rev_per_user_before <- lm(revenue ~ day
                               , data=stats_per_day_weighted_BEFORE)
newdata_BEFORE <- stats_per_day_weighted %>% 
  filter(day >= DATE_SHIP_NEW_FEATURE)

stats_per_day_weighted_BEFORE_prep$revenue_forecasted <- stats_per_day_weighted_BEFORE_prep$revenue
stats_per_day_weighted_BEFORE_prep[stats_per_day_weighted_BEFORE_prep$day >= DATE_SHIP_NEW_FEATURE
                                   ,]$revenue_forecasted <- 
  predict(lm_rev_per_user_before, newdata=newdata_BEFORE)

stats_per_day_weighted_BEFORE_prep <- stats_per_day_weighted_BEFORE_prep %>% 
  mutate(diff_tilde = revenue - revenue_forecasted)
## TODO Plot forecast vs reality (?)


##### Reconstruct total effect

test <- visits_fact_df %>% 
  left_join(stats_per_day_weighted_BEFORE_prep, by=c('day', 'Date')) %>% 
  group_by(day, Date) %>%
  summarise(N_tilde_t = sum(wCal), Nt = n(), revenue_real_t = sum(daily_spend_mu)
            , revenue_weighted_t = sum(daily_spend_mu*wCal)
            , N_ratio = Nt / N_tilde_t
            , N_ratio_inverse  = N_tilde_t / Nt)

test2 <- stats_per_day_weighted_BEFORE_prep %>% 
  left_join(test, by=c('day', 'Date')) %>% 
  mutate(effect_tilde = diff_tilde/revenue_weighted_t,
         effect_real = effect_tilde*N_ratio)

test2_POST <- test2 %>% 
  filter(day >= DATE_SHIP_NEW_FEATURE)

summary(test2_POST$effect_tilde)
summary(test2_POST$effect_real)





