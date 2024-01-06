library(tidyverse)
library(icarus)
source('R/parameters.R')

visits_fact_df <- readRDS("data/visits_fact_df.rds")

visits_fact_df <- visits_fact_df %>% 
  mutate(first_visit_date_cal = case_when(
    first_visit_date == 0 ~ 1
    , TRUE ~ 2
  ))
N_VISITS <- nrow(visits_fact_df)

# Calibrate
marginsMatrix <- newMarginMatrix() %>% 
  addMargin("age_group", TABLE_AGE) %>% 
  addMargin("sex", TABLE_SEX) %>% 
  addMargin("first_visit_date_cal", c(P_OLD_USERS, 1-P_OLD_USERS)) %>% 
  addMargin("household_size", TABLE_HOUSEHOLD_SIZE)

visits_fact_df$weight1 <- 1
visits_fact_df$wCal <- calibration(visits_fact_df, marginsMatrix, "weight1"
                                   , method="raking", popTotal = N_VISITS, pct=T)

