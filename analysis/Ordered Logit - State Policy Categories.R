library(here)
library(tidyverse)

state_policy <- read_csv(here('data', 'state_policy.csv'))

state_policy[is.na(state_policy)] <- 0

state_policy <- state_policy %>%
  mutate(state_policy = -sodomy_illega + -marriage_ban + marriage_equality + civil_union + 
           employment_discrim_so + hate_crime_so + joint_adoption + -adoption_religious_freedom +
           conversion_therapy_ban + housing_discrim_so + state_ban_local_nondiscrimimation) %>%
  select(State, Year, state_policy)

hist(state_policy$state_policy)

state_policy$cat <- cut(state_policy$state_policy, breaks = c(-2,0,2, Inf), labels = c("Repressive", "Neutral", "Progressive"))    

## Getting this onto the ACS data now
acs_dyad <- read_csv(here('data', 'acs_dyad.csv')) %>%
  mutate(mean_year_immig = ifelse(mean_year_immig >= 1991, 
                                  round(mean_year_immig),
                                  1991))
acs_dyad_policy <- acs_dyad %>%
  left_join(state_policy,by = c('mean_year_immig' = 'Year', 'state' = 'State'))

acs_dyad_policy_log <- acs_dyad_policy %>%
  mutate(same_sex_stock = log(same_sex_stock +1),
         opp_sex_stock = log(opp_sex_stock+1),
         state_stock_year = log(state_stock_year+1))

## fit ordered logit model and store results 'ologit'
library(MASS)
ologit <- polr(cat ~ same_sex_stock +  factor(year) , data = acs_dyad_policy_log, Hess=TRUE)

## view a summary of the model
summary(ologit)



