## Analysis for PAA Extended Abstract
## September 23, 2020
## Nathan Hoffmann and Kristopher Velasco

library(RColorBrewer)
library(st)
library(haven)
library(cowplot)
library(srvyr)
library(here)
library(tidyverse)

acs_dyad <- read_csv(here('data', 'acs_dyad.csv')) %>%
  mutate(mean_year_immig = ifelse(mean_year_immig >= 1991, 
                                  round(mean_year_immig),
                                  1991))

lgbt_policy <- read_csv(here('data', 'LGBT Data w IPUMS Code.csv')) %>%
  select(Country, year, Code, origin_score = total_score)
state_policy <- read_csv(here('data', 'state_policy.csv'))

state_policy[is.na(state_policy)] <- 0

state_policy <- state_policy %>%
  mutate(state_policy = -sodomy_illega + -marriage_ban + marriage_equality + civil_union + 
           employment_discrim_so + hate_crime_so + joint_adoption + -adoption_religious_freedom +
           conversion_therapy_ban + housing_discrim_so + state_ban_local_nondiscrimimation) %>%
  select(State, Year, state_policy)

acs_dyad_policy <- acs_dyad %>%
  left_join(lgbt_policy, by = c('mean_year_immig' = 'year', 'bpld_id' = 'Code')) %>%
  left_join(state_policy,by = c('mean_year_immig' = 'Year', 'state' = 'State'))


lm(same_sex_stock ~ origin_score + state_policy, data = acs_dyad_policy) %>%
  summary()
lm(opp_sex_stock ~ origin_score + state_policy, data = acs_dyad_policy) %>%
  summary()

mod_same <- lm(same_sex_stock ~ origin_score + state_policy + state_stock_year, data = acs_dyad_policy) %>%
  summary()
mod_opp <- lm(opp_sex_stock ~ origin_score + state_policy + state_stock_year, data = acs_dyad_policy) %>%
  summary()

stargazer(mod_same, mod_opp, header = F)

# models with logged pop variables
acs_dyad_policy_log <- acs_dyad_policy %>%
  mutate(same_sex_stock = log(same_sex_stock +1),
         opp_sex_stock = log(opp_sex_stock+1),
         state_stock_year = log(state_stock_year+1))


lm(same_sex_stock ~ origin_score + state_policy, data = acs_dyad_policy_log) %>%
  summary()
lm(opp_sex_stock ~ origin_score + state_policy, data = acs_dyad_policy_log) %>%
  summary()
lm(same_sex_stock ~ origin_score + state_policy + state_stock_year, data = acs_dyad_policy_log) %>%
  summary()
lm(opp_sex_stock ~ origin_score + state_policy + state_stock_year, data = acs_dyad_policy_log) %>%
  summary()