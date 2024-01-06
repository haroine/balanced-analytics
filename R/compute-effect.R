library(tidyverse)
library(lubridate)
source('R/parameters.R')


wCal_df <- readRDS("data/weights_balanced.rds")
visits_fact_df <- readRDS("data/visits_fact_df.rds") %>% 
  left_join(wCal_df, by=c("day", "Date", "user_id"))

##TODO Compute the effect