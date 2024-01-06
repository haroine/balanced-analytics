library(testthat)
source("R/parameters.R")

visits_fact_df <- readRDS("data/visits_fact_df.rds")
user_dimension_inferred <- unique(visits_fact_df %>% 
                            select(user_id, age_group, sex, first_visit_date, household_size))
N_INFERRED <- length(unique(user_dimension_inferred$user_id))


expect_equal(N_INFERRED, N, tolerance = 50)

expect_equal(
  as.vector(unname(table(user_dimension_inferred$age_group) / N_INFERRED))
  , TABLE_AGE
  , tolerance = 0.025)

expect_equal(
  as.vector(unname(table(user_dimension_inferred$sex) / N_INFERRED))
  , TABLE_SEX
  , tolerance = 0.025)

expect_equal(
  as.vector(unname(table(user_dimension_inferred$household_size) / N_INFERRED))
  , TABLE_HOUSEHOLD_SIZE
  , tolerance = 0.025)

expect_equal(
  as.vector(unname(table(user_dimension_inferred$first_visit_date) / N_INFERRED))[1]
  , 0.5
  , tolerance = 0.05)