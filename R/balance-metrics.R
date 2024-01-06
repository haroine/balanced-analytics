library(tidyverse)
library(icarus)
library(lubridate)
source('R/parameters.R')

visits_fact_df <- readRDS("data/visits_fact_df.rds")

visits_fact_df <- visits_fact_df %>% 
  mutate(first_visit_date_cal = case_when(
    first_visit_date == 0 ~ 1
    , TRUE ~ 2
  ), month_visit = as.numeric(month(Date))
  , is_test_group = case_when(
    first_visit_date >= 450 ~ 1
    , TRUE ~ 2
  )) %>% 
  mutate(low_traffic_holidays = case_when(
    (day >= 90 & day <= 94) ~ 1
    , (day >= 461 & day <= 464) ~ 1
    , TRUE ~ 2
  ), high_traffic_holidays = case_when(
    (day >= 95 & day <= 146) ~ 1
    , (day >= 268 & day <= 276) ~ 1
    , (day >= 457) ~ 1
    , TRUE ~ 2
  ))
N_VISITS <- nrow(visits_fact_df)

# Calibrate
DAYS_HIGH_TRAFFIC <- length(unique((visits_fact_df %>% filter(high_traffic_holidays == 1))$day))
DAYS_LOW_TRAFFIC <- length(unique((visits_fact_df %>% filter(low_traffic_holidays == 1))$day))

marginsMatrix <- newMarginMatrix() %>% 
  addMargin("age_group", TABLE_AGE) %>% 
  addMargin("sex", TABLE_SEX) %>% 
  addMargin("first_visit_date_cal", c(P_OLD_USERS, 1-P_OLD_USERS)) %>% 
  addMargin("household_size", TABLE_HOUSEHOLD_SIZE) %>% 
  addMargin("is_test_group", c(1/10,9/10)) %>%
  addMargin("low_traffic_holidays", c(DAYS_LOW_TRAFFIC/N_DAYS, 1-DAYS_LOW_TRAFFIC/N_DAYS)) %>% 
  addMargin("high_traffic_holidays", c(DAYS_HIGH_TRAFFIC/N_DAYS, 1-DAYS_HIGH_TRAFFIC/N_DAYS))


visits_fact_df$weight1 <- 1
visits_fact_df$wCal <- calibration(visits_fact_df, marginsMatrix, "weight1"
                                   , method="raking", popTotal = N_VISITS, pct=T)

# saveRDS(visits_fact_df %>% select(day, Date, user_id, wCal), file="data/weights_balanced.rds")



