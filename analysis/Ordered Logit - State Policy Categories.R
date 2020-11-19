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
acs_coupled_imms <- read.csv(here("data", "acs_coupled_imms.csv")) 
  mutate(yrimmig = ifelse(yrimmig >= 1991, 
                                  round(yrimmig),
                                  1991))

lgbt_policy <- read_csv(here('data', 'LGBT Data w IPUMS Code.csv')) %>%
  select(Country, year, Code, origin_score = total_score)

acs_couple_policy <- acs_coupled_imms  %>%
  left_join(lgbt_policy, by = c('yrimmig' = 'year', 'bpldid' = 'Code')) %>%
  left_join(state_policy,by = c('year' = 'Year', 'state' = 'State'))


## fit ordered logit model and store results 'ologit'
library(MASS)
ologit <- polr(factor(state_policy) ~ age + factor(educ) + factor(same_sex)*origin_score + factor(nchild) + yrimmig,  data = acs_couple_policy, Hess=TRUE)

## view a summary of the model
summary(ologit)

## Plotting Predicted Probabilities
acs.means <- acs_couple_policy %>% 
                    summarize_all(mean, na.rm=T)

#Make a dataframe that's the mode of the factor variables vs. mean - one row data frame
#Two dataframes, one for same-sex and one for not. And then make two plots.

plot.dat <- bind_rows(rep(list(acs.means),14)) %>%
  mutate(origin_score = -3:10)

pprobs <- as.data.frame(predict(ologit, type="probs", newdata=plot.dat)) %>%
  mutate(origin_score=plot.dat$origin_score) 


ggplot(pprobs, aes(x=origin_score, y=phat, col=oblc))


