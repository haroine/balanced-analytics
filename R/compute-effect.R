library(tidyverse)
library(lubridate)
source('R/parameters.R')


wCal_df <- readRDS("data/weights_balanced.rds")
visits_fact_df <- readRDS("data/visits_fact_df.rds") %>% 
  left_join(wCal_df, by=c("day", "Date", "user_id"))

COMPUTE_EFFECT <- FALSE
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

stats_per_day_weighted_BEFORE_prep[stats_per_day_weighted_BEFORE_prep$day >= DATE_SHIP_NEW_FEATURE
                                   ,]$revenue <- 
  predict(lm_rev_per_user_before, newdata=newdata_BEFORE)

stats_per_day_weighted_BEFORE_prep$diff <- 0
stats_per_day_weighted_BEFORE_prep[stats_per_day_weighted_BEFORE_prep$day >= DATE_SHIP_NEW_FEATURE
                                   ,]$diff <- 
  stats_per_day_weighted[
    stats_per_day_weighted$day >= DATE_SHIP_NEW_FEATURE,]$revenue -
  stats_per_day_weighted_BEFORE_prep[stats_per_day_weighted_BEFORE_prep$day >= DATE_SHIP_NEW_FEATURE
                                     ,]$revenue

stats_per_day_weighted_BEFORE_prep <- stats_per_day_weighted_BEFORE_prep %>% 
  mutate(effect_raw = diff / revenue)
## TODO Plot effect(?)

## Reconstruct total effect
(mean((stats_per_day_weighted_BEFORE_prep %>% 
  filter(day >= DATE_SHIP_NEW_FEATURE))$effect_raw)) # -0.108
## Is it directly the right result?


COMPUTE_EFFECT <- TRUE
visits_fact_df <- visits_fact_df %>% 
  left_join(stats_per_day_weighted_BEFORE_prep %>% select(Date, diff, effect_raw), by=c('Date')) %>% 
  mutate(effect = effect_raw*1/wCal)


