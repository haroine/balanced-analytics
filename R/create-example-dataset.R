library(tidyverse)
library(lubridate)
library(GenBinomApps)
source('R/functs-generate-dataset.R')

# Final dataset:
# all visits, with info
# date, user_id, purchase_amount, age_group, sex, first_visit_date, household_size
# Metrics: total revenue, *revenue per user*

# To generate
# False concerning slump during the summer (more young people who spend less), which is purely mechanical
# - more proportionally young (and else?) visits during the summer 
# True decline in the last 3 months which is masked by the fact it affects more ppl who spend 
# less (new spenders, youngs) + normal vacations

DATE_START <- as.Date('2023-04-01')
N_DAYS <- 500

N <- 1e4

VALUES_AGE <- 1:4
TABLE_AGE <- c(0.5,0.3,0.15,0.05)
VALUES_SEX <- 1:2 # Deboost visit for 2, slight boost in spend for 2
TABLE_SEX <- c(0.75, 0.25)

# First visit date
# If 0, boost, else spend decrease as first visit date increases

VALUES_HOUSEHOLD_SIZE <- 1:7
TABLE_HOUSEHOLD_SIZE <- c(0.3,0.25,0.175,0.10,0.085,0.065,0.025)
# Spend and visit diminishes with household size



## Create users dimension table
# Users created before start date
P_OLD_USERS <- 0.5
N_OLD_USERS <- floor(P_OLD_USERS*N)
N_NEW_USERS <- N - N_OLD_USERS

## Generate spend variable from Pareto

set.seed(233)
library(truncnorm)
# library(e1071)
library(EnvStats)

shap <- 10
loc <- 1000

Y <- rpareto(N, loc, shap) # Location is mode, shap>>4
mean(Y)
hist(Y)
kurtosis(Y)


old_users_dimension_df <- tibble(
  user_id = 1:(N_OLD_USERS)
  # , purchase_amount = -1
  , age_group = gen_categorical(N_OLD_USERS, TABLE_AGE)
  , sex = gen_categorical(N_OLD_USERS, TABLE_SEX)
  , first_visit_date = 0
  , household_size = gen_categorical(N_OLD_USERS, TABLE_HOUSEHOLD_SIZE)
  , daily_visit_p_base = 2.5/30.
  , daily_spend_mu_base = rpareto(N_OLD_USERS, loc, shap)*1.2
)
new_users_dimension_df <- tibble(
  user_id = 1:(N_NEW_USERS)
  # , purchase_amount = -1
  , age_group = gen_categorical(N_NEW_USERS, TABLE_AGE)
  , sex = gen_categorical(N_NEW_USERS, TABLE_SEX)
  , first_visit_date = gen_categorical(N_NEW_USERS, 1:(N_DAYS-1) * 2 / (N_DAYS*(N_DAYS-1)))
  , household_size = gen_categorical(N_NEW_USERS, TABLE_HOUSEHOLD_SIZE)
  , daily_visit_p_base = 1.5/30.
  , daily_spend_mu_base = rpareto(N_NEW_USERS, loc, shap)
)

users_dimension_df <- rbind(old_users_dimension_df, new_users_dimension_df) %>% 
  mutate(first_visit_date = as.numeric(first_visit_date)) %>% 
  arrange(first_visit_date)

# TODO: Fill probabilities for users



users_dimension_df <- users_dimension_df %>% 
  mutate(daily_visit_p = daily_visit_p_base *
           (1-exp(-1/as.numeric(age_group))) *
           (1-exp(-1/as.numeric(sex))) *
           (1-as.numeric(first_visit_date)/2000) *
           (1-exp(-1/as.numeric(household_size)))
  , daily_spend_mu = daily_spend_mu_base * 
    (exp(-1/as.numeric(age_group))) *
    (exp(-1/as.numeric(sex))) *
    (1-as.numeric(first_visit_date)/2000) *
    (1-exp(-1/as.numeric(household_size)))
  ) %>% 
  mutate(daily_visit_p = daily_visit_p * 2./30. * 1/mean(daily_visit_p)
         , daily_spend_mu = daily_spend_mu * 1000. * 1/mean(daily_spend_mu)) %>% 
  mutate(daily_spend_mu = case_when(
    
  ))

# TODO: Generate visits_fact_df
# Time events, holidays: 
# 90-94 (July 4) - Visits DOWN all
# 95-146 (Summer, age group 1) - Visits UP group 1
# 268-276 (Winter holidays) - Visits UP group 1, DOWN rest, revenue UP all
# 450-500 (Ship underperforming new feature) - revenue DOWN all
# 457-500 (Summer, age group 1) - Visits UP group 1
library(foreach)

visits_fact_df <- foreach(current_day = 1:N_DAYS, .combine = rbind) %do% {
  # current_day <- 3
  eligible_users <- (users_dimension_df %>% filter(first_visit_date <= current_day))
  current_p_vec <- eligible_users$daily_visit_p
  
  if(current_day >= 90 && current_day <= 94) {
    current_p_vec <- current_p_vec*0.5
  }
  
  if(current_day >= 95 && current_day <= 146) {
    current_p_vec[which(eligible_users$age_group == 1)] <- current_p_vec[which(eligible_users$age_group == 1)] * 1.5
  }
  
  if(current_day >= 268 && current_day <= 276) {
    current_p_vec[which(eligible_users$age_group == 1)] <- current_p_vec[which(eligible_users$age_group == 1)] * 1.5
    current_p_vec[which(eligible_users$age_group > 1)] <- current_p_vec[which(eligible_users$age_group > 1)] * 0.75
  }
  
  if(current_day >= 457) {
    current_p_vec[which(eligible_users$age_group == 1)] <- current_p_vec[which(eligible_users$age_group == 1)] * 1.5
  }
  
  cbind(current_day,eligible_users %>% filter(
    rbinom(
     length(current_p_vec)
     , 1
     , current_p_vec
  ) == 1))
}
names(visits_fact_df) <- c("day", names(eligible_users))

visits_fact_df <- visits_fact_df %>% 
  mutate(daily_spend_mu = case_when(
    (day >= 268 & day <= 276) ~ daily_spend_mu*1.5
    , (day >= 450) & (first_visit_date >= 450) ~ daily_spend_mu*0.9
    , TRUE ~ daily_spend_mu
  ))

stats_per_day <- visits_fact_df %>% 
  group_by(day) %>% 
  summarise(visits = n(), revenue = sum(daily_spend_mu), revenue_per_user = sum(daily_spend_mu)/n())

stats_plot <- ggplot(data=stats_per_day, aes(x=day))+
  geom_line(aes(y=visits), color="black") +
  # geom_line(aes(y=revenue), color="blue") +
  geom_line(aes(y=revenue_per_user), color="skyblue") +
  NULL

print(stats_plot)

# Trend: growth + revenue per users mechanically goes down bc newer users spend less
# 

# colMeans(
# matrix(rbinom(7*1000,1,TABLE_HOUSEHOLD_SIZE), ncol=7, byrow=T)
# )

## TODO: Is it visible in cohorts???

# saveRDS(visits_fact_df, file="data/visits_fact_df.rds")