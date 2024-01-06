DATE_START <- as.Date('2023-04-01')
N_DAYS <- 500
SPEND_EFFECT_NEW_FEATURE <- 0.9

N <- 1e4

VALUES_AGE <- 1:4
TABLE_AGE <- c(0.5,0.3,0.15,0.05)
VALUES_SEX <- 1:2 # Deboost visit for 2, slight boost in spend for 2
TABLE_SEX <- c(0.75, 0.25)

VALUES_HOUSEHOLD_SIZE <- 1:7
TABLE_HOUSEHOLD_SIZE <- c(0.3,0.25,0.175,0.10,0.085,0.065,0.025)
# Spend and visit diminishes with household size
P_OLD_USERS <- 0.5