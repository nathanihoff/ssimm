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


lgbt_policy <- read_csv(here('data', 'LGBT Data w IPUMS Code.csv')) %>%
  select(Country, year, Code, origin_score = total_score)

acs_couple_policy <- acs_coupled_imms  %>%
  mutate(yrimmig = ifelse(yrimmig >= 1991, 
                          round(yrimmig),
                          1991)) %>%
  left_join(lgbt_policy, by = c('yrimmig' = 'year', 'bpldid' = 'Code')) %>%
  left_join(state_policy,by = c('year' = 'Year', 'state' = 'State'))

set.seed(1859)
acs_couple_policy_small <- bind_rows(
  filter(acs_couple_policy, same_sex == T),
  sample_n(filter(acs_couple_policy, same_sex == F), 5e4)
)

## fit ordered logit model and store results 'ologit'
library(MASS)
ologit <- polr(factor(state_policy) ~ age + factor(educ) + factor(same_sex)*origin_score + factor(nchild) + yrimmig,  
               data = acs_couple_policy_small, Hess=TRUE)
ologit_binned <- polr(factor(cat) ~ age + factor(educ) + factor(same_sex)*origin_score + factor(nchild) + yrimmig,  
                      data = acs_couple_policy_small, Hess=TRUE)


lgbt_policy <- read_csv(here('data', 'LGBT Data w IPUMS Code.csv')) %>%
  select(Country, year, Code, origin_score = total_score)

acs_couple_policy <- acs_coupled_imms  %>%
  left_join(lgbt_policy, by = c('yrimmig' = 'year', 'bpldid' = 'Code')) %>%
  left_join(state_policy,by = c('year' = 'Year', 'state' = 'State'))

## view a summary of the model
summary(ologit)

## Plotting Predicted Probabilities

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

acs.means <- acs_couple_policy_small %>% 
  summarize(across(c(age, origin_score, yrimmig), mean, na.rm = T))
acs.modes <- acs_couple_policy_small %>% 
  summarize(across(c(educ, nchild), Mode))



#Make a dataframe that's the mode of the factor variables vs. mean - one row data frame
#Two dataframes, one for same-sex and one for not. And then make two plots.
plot.dat <- bind_rows(rep(list(bind_cols(acs.means, acs.modes)),14)) %>%
  mutate(origin_score = -3:10)

plot.dat.same <- plot.dat %>%
  mutate(same_sex = T)
plot.dat.dif <- plot.dat %>%
  mutate(same_sex = F)



pprobs.same <- as.data.frame(predict(ologit, type="probs", newdata=plot.dat.same)) %>%
  mutate(origin_score=plot.dat$origin_score) %>%
  gather(state_policy, phat, -origin_score) 
pprobs.dif <- as.data.frame(predict(ologit, type="probs", newdata=plot.dat.dif)) %>%
  mutate(origin_score=plot.dat$origin_score)  %>%
  gather(state_policy, phat, -origin_score) 

ggplot(pprobs.same, aes(x=origin_score, y=phat, col=state_policy)) +
  geom_line() + geom_point() + ggtitle('Same-sex')
ggplot(pprobs.dif, aes(x=origin_score, y=phat, col=state_policy)) +
  geom_line() + geom_point() + ggtitle('Different-sex')

# now with binned
pprobs.same2 <- as.data.frame(predict(ologit_binned, type="probs", newdata=plot.dat.same)) %>%
  mutate(origin_score=plot.dat$origin_score) %>%
  gather(cat, phat, -origin_score) 
pprobs.dif2 <- as.data.frame(predict(ologit_binned, type="probs", newdata=plot.dat.dif)) %>%
  mutate(origin_score=plot.dat$origin_score)  %>%
  gather(cat, phat, -origin_score) 

ggplot(pprobs.same2, aes(x=origin_score, y=phat, col=cat)) +
  geom_line() + geom_point() + ggtitle('Same-sex')
ggplot(pprobs.dif2, aes(x=origin_score, y=phat, col=cat)) +
  geom_line() + geom_point() + ggtitle('Different-sex')

# testing with english as outcome
ologit_test <- polr(factor(speakeng) ~ age + factor(educ) + factor(same_sex)*origin_score + factor(nchild) + yrimmig,  
               data = acs_couple_policy_small, Hess=TRUE)

plot.dat <- bind_rows(rep(list(acs.means),14)) %>%
  mutate(origin_score = -3:10)

pprobs <- as.data.frame(predict(ologit, type="probs", newdata=plot.dat)) %>%
  mutate(origin_score=plot.dat$origin_score) 


ggplot(pprobs, aes(x=origin_score, y=phat, col=oblc))

pprobs.same3 <- as.data.frame(predict(ologit_test, type="probs", newdata=plot.dat.same)) %>%
  mutate(origin_score=plot.dat$origin_score) %>%
  gather(speakeng, phat, -origin_score) 
pprobs.dif3 <- as.data.frame(predict(ologit_test, type="probs", newdata=plot.dat.dif)) %>%
  mutate(origin_score=plot.dat$origin_score)  %>%
  gather(speakeng, phat, -origin_score) 

ggplot(pprobs.same3, aes(x=origin_score, y=phat, col=speakeng)) +
  geom_line() + geom_point() + ggtitle('Same-sex')
ggplot(pprobs.dif3, aes(x=origin_score, y=phat, col=speakeng)) +
  geom_line() + geom_point() + ggtitle('Different-sex')
