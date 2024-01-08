library(tidyverse)
library(lubridate)
source('R/parameters.R')


visits_fact_df <- readRDS("data/visits_fact_df.rds")

## Compute the effect
# Weighted Metrics

## TODO: Prettier one-hot encoding
stats_per_day_weighted <- visits_fact_df %>% 
  group_by(day, Date) %>% 
  summarise(visits = sum(wCal), revenue = sum(daily_spend_mu*wCal)
            , revenue_per_user = sum(daily_spend_mu*wCal)/sum(wCal)
            , N_age_1 = sum(case_when(
              age_group == 1 ~ wCal
                , TRUE ~ 0
              ))
            , N_age_2 = sum(case_when(
              age_group == 2 ~ wCal
              , TRUE ~ 0
            ))
            , N_age_3 = sum(case_when(
              age_group == 3 ~ wCal
              , TRUE ~ 0
              ))
            , N_sex_1 = sum(case_when(
              sex == 1 ~ wCal
              , TRUE ~ 0
            ))
            , N_household_size_1 = sum(case_when(
              household_size == 1 ~ wCal
              , TRUE ~ 0
            ))
            , N_household_size_2 = sum(case_when(
              household_size == 2 ~ wCal
              , TRUE ~ 0
            ))
            , N_household_size_3 = sum(case_when(
              household_size == 3 ~ wCal
              , TRUE ~ 0
            ))
            , N_household_size_4 = sum(case_when(
              household_size == 4 ~ wCal
              , TRUE ~ 0
            ))
            , N_household_size_5 = sum(case_when(
              household_size == 5 ~ wCal
              , TRUE ~ 0
            ))
            , N_household_size_6 = sum(case_when(
              household_size == 6 ~ wCal
              , TRUE ~ 0
            ))
            , N_low_traffic = sum(case_when(
              low_traffic_holidays == 1 ~ wCal
              , TRUE ~ 0
            ))
            , N_high_traffic = sum(case_when(
              high_traffic_holidays == 1 ~ wCal
              , TRUE ~ 0
            ))
            )

stats_per_day_unw <- visits_fact_df %>% 
  group_by(day, Date) %>% 
  summarise(visits = n(), revenue = sum(daily_spend_mu)
            , revenue_per_user = sum(daily_spend_mu)/n()
            , N_age_1 = sum(case_when(
              age_group == 1 ~ 1
              , TRUE ~ 0
            ))
            , N_age_2 = sum(case_when(
              age_group == 2 ~ 1
              , TRUE ~ 0
            ))
            , N_age_3 = sum(case_when(
              age_group == 3 ~ 1
              , TRUE ~ 0
            ))
            , N_sex_1 = sum(case_when(
              sex == 1 ~ 1
              , TRUE ~ 0
            ))
            , N_household_size_1 = sum(case_when(
              household_size == 1 ~ 1
              , TRUE ~ 0
            ))
            , N_household_size_2 = sum(case_when(
              household_size == 2 ~ 1
              , TRUE ~ 0
            ))
            , N_household_size_3 = sum(case_when(
              household_size == 3 ~ 1
              , TRUE ~ 0
            ))
            , N_household_size_4 = sum(case_when(
              household_size == 4 ~ 1
              , TRUE ~ 0
            ))
            , N_household_size_5 = sum(case_when(
              household_size == 5 ~ 1
              , TRUE ~ 0
            ))
            , N_household_size_6 = sum(case_when(
              household_size == 6 ~ 1
              , TRUE ~ 0
            ))
            , N_low_traffic = sum(case_when(
              low_traffic_holidays == 1 ~ 1
              , TRUE ~ 0
            ))
            , N_high_traffic = sum(case_when(
              high_traffic_holidays == 1 ~ 1
              , TRUE ~ 0
            ))
  )

names_stats_unw <- names(stats_per_day_unw)
names(stats_per_day_unw) <- paste0(names_stats_unw,"_unw")
stats_per_day_all <- cbind(stats_per_day_weighted, stats_per_day_unw %>% select(-Date_unw, -day_unw))

stats_per_day_prep <- stats_per_day_all %>% 
  ungroup()
stats_per_day_BEFORE <- stats_per_day_prep %>% 
  filter(day < DATE_SHIP_NEW_FEATURE)

lm_weighted <- lm(revenue ~ day + N_age_1 + N_age_2 + N_age_3 + 
                               N_household_size_1 + N_household_size_2 + N_household_size_3 +
                               N_household_size_4 + N_household_size_5 + N_household_size_6 +
                               N_low_traffic + N_high_traffic
                               , data=stats_per_day_BEFORE)

newdata_forpred_weighted <- stats_per_day_prep %>% 
  filter(day > DATE_SHIP_NEW_FEATURE)


stats_per_day_BEFORE_unw <- stats_per_day_prep %>% 
  # filter(day >= DATE_SHIP_NEW_FEATURE) %>% 
  select(paste0(names_stats_unw[names_stats_unw != 'Date'],"_unw"))
names(stats_per_day_BEFORE_unw) <- gsub("_unw","",names(stats_per_day_BEFORE_unw))

lm_unw <- lm(revenue ~ day + N_age_1 + N_age_2 + N_age_3 + 
                    N_household_size_1 + N_household_size_2 + N_household_size_3 +
                    N_household_size_4 + N_household_size_5 + N_household_size_6 +
                    N_low_traffic + N_high_traffic
                  , data=stats_per_day_BEFORE_unw)

newdata_forpred_unw <- stats_per_day_BEFORE_unw %>% 
  filter(day > DATE_SHIP_NEW_FEATURE)
  

stats_per_day_prep$revenue_forecasted1 <- stats_per_day_prep$revenue_unw
stats_per_day_prep$revenue_forecasted2 <- stats_per_day_prep$revenue_unw
stats_per_day_prep[stats_per_day_prep$day > DATE_SHIP_NEW_FEATURE
                                   ,]$revenue_forecasted1 <- 
  predict(lm_weighted, newdata=newdata_forpred_weighted)
stats_per_day_prep[stats_per_day_prep$day > DATE_SHIP_NEW_FEATURE
                   ,]$revenue_forecasted2 <- 
  predict(lm_unw, newdata=newdata_forpred_unw)

## TODO Plot forecast vs reality (?)


##### Reconstruct total effect

# Only works if effect is uniform, which it isn't
# TODO Compute right effect
ratio_affected <- nrow(visits_fact_df %>% filter(affected == 1)) /
  nrow(visits_fact_df %>% filter(day > DATE_SHIP_NEW_FEATURE))
real_effect <- 1-(1-ratio_affected+ratio_affected*SPEND_EFFECT_NEW_FEATURE)
real_effect 

summary(
  stats_per_day_prep %>% 
  mutate(effect = (revenue_forecasted1 - revenue) / revenue) %>% 
  filter(day > DATE_SHIP_NEW_FEATURE) %>% 
  select(effect)
)

summary(
  stats_per_day_prep %>% 
    mutate(effect = (revenue_forecasted2 - revenue_unw) / revenue_unw) %>% 
    filter(day > DATE_SHIP_NEW_FEATURE) %>% 
    select(effect)
)

