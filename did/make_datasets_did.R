library(zoo)
library(priceR)
library(kableExtra)
library(sjstats)
library(imputeTS)
library(countrycode)
library(RColorBrewer)
library(haven)
library(cowplot)
library(srvyr)
library(here)
library(tidyverse)


## Reshape ACS ####
acs <- read_dta('/Users/nathan/Data/ACS/acs_2008_2020.dta') %>%
  mutate(
    position = case_when(
      related == 101 ~ 'main',
      related %in% c(201, 1114) ~ 'partner'),
    age = as.numeric(age),
    yrimmig = ifelse(yrimmig == 0, NA, as.numeric(yrimmig)),
    nchild = as.numeric(nchild),
    year = as.numeric(year),
    bpldid = as.numeric(bpld),
    state = stateicp,
    stateicp = as.numeric(stateicp),
    ftotinc = ifelse(ftotinc == 9999999, NA, ftotinc*cpi99/1000),
    inctot = ifelse(inctot == 9999999, NA, inctot*cpi99/1000),
    hwsei = ifelse(hwsei == 0000, NA, hwsei),
    occscore = ifelse(occscore == 00, NA, occscore),
    educ = case_when(
      educ %in% 0:5 ~ '< HS',
      educ == 6 ~ 'HS',
      educ %in% 7:9 ~ 'some col',
      educ %in% 10:11 ~ 'college'),
    bpldid = case_when(bpld %in% c(41000, 41100, 41410) ~ 41300, # England, Scotland, Northern Ireland
                       as_factor(bpld) == 'Korea' ~ 50220,
                       T ~ bpldid)) %>%
  filter(year != 2020)




# Define immigrant as someone born abroad not to US parents
acs_wide <- acs %>%
  # Only  those who immigrated when 18+
  filter(!is.na(position)) %>%
  filter(age - (year - yrimmig) >= 18 | is.na(yrimmig)) %>%
  select(
    # household variables
    year, serial, nchild, hhwt, ssmc,
    cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
    # individual variables
    position, sex, related, yrimmig, bpld, bpldid, citizen, educ, qrelate, qsex, qcitizen, qeduc,
    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, speakeng, age, perwt
    # state_stock_year, state_stock_avg
  ) %>%
  pivot_wider(#id_cols = serial,
    names_from = position,
    values_from = c(sex, related, related, yrimmig, bpld, bpldid, citizen, educ,
                    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, 
                    speakeng, age, perwt, qrelate, qsex, qcitizen, qeduc
                    #state_stock_year, state_stock_avg
    )) %>%
  filter(!is.na(sex_partner), !is.na(sex_main)) %>%
  mutate(same_sex = (sex_main == sex_partner),
         imm_couple = case_when(
           citizen_main %in% c(0,1) & citizen_partner %in% c(0,1) ~ 'none',
           citizen_main %in% c(2,3) & citizen_partner %in% c(2,3) ~ 'two',
           citizen_main %in% c(2,3) | citizen_partner %in% c(2,3) ~ 'one')) %>%
  # convert all labeled variables to factor
  as_factor() %>%
  mutate(married = related_partner == 'Spouse' | 
           qrelate_partner == 'Same sex spouse changed to unmarried partner',
         allocated = (qrelate_main == 'Allocated' | qrelate_partner == 'Allocated' |
                        qsex_main == 'Allocated' | qsex_partner == 'Allocated'))

acs_wide %>%
  select(state, year, same_sex, serial, nchild, hhwt, ssmc,
         cluster, strata, metro, region, stateicp, ftotinc, respmode,
         imm_couple, married, allocated,
         citizen_main, citizen_partner, yrimmig_main, yrimmig_partner, bpld_main, bpld_partner,
         bpldid_main, bpldid_partner, age_main, age_partner,
         perwt_main, perwt_partner, qcitizen_main, qcitizen_partner, qeduc_main, qeduc_partner, 
         allocated) %>%
  filter(allocated == F) %>%
  write_csv(here('data', 'acs_wide_small.csv'))



## Counts for poisson model ####
lgbt_policy <- read.csv(here('data', 'lgb_origin_index.csv')) %>%
  rename(origin_score = origin_couple_score) %>%
  mutate(iso_o = countrycode::countrycode(Country, origin = 'country.name', destination = 'iso3c'))

acs_wide_small <- read_csv(here('data', 'acs_wide_small.csv'))


acs_count_base <- acs_wide_small %>%
  mutate(state = as.character(state),
         mixed_citizenship = (citizen_main == 'Not a citizen' & citizen_partner != 'Not a citizen') | 
           (citizen_main != 'Not a citizen' & citizen_partner == 'Not a citizen'),
         noncit = citizen_main == 'Not a citizen' | citizen_partner == 'Not a citizen',
         bpldnew_main = if_else(is.na(yrimmig_main) & !is.na(yrimmig_partner), bpld_partner, bpld_main),
         bpldnew_partner = if_else(is.na(yrimmig_partner) & !is.na(yrimmig_main), bpld_main, bpld_partner),
         bpldidnew_main = if_else(is.na(yrimmig_main) & !is.na(yrimmig_partner), bpldid_partner, bpldid_main),
         bpldidnew_partner = if_else(is.na(yrimmig_partner) & !is.na(yrimmig_main), bpldid_main, bpldid_partner),
         yrimmigmod_main = ifelse(!is.na(yrimmig_main), yrimmig_main, yrimmig_partner),
         yrimmigmod_partner = ifelse(!is.na(yrimmig_partner), yrimmig_partner, yrimmig_main)) %>%
  filter(allocated == F) %>%
  pivot_longer(-c(year, serial, nchild, hhwt, ssmc,
                  cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
                  same_sex, imm_couple, married, allocated, mixed_citizenship, noncit),
               names_to = c('.value', 'position'),
               names_sep = '_') %>% 
  filter(age >= 18 & age <= 64) %>%
  filter(across(c(state, year, same_sex), 
                ~ !is.na(.x))) %>%
  mutate(yrimmigmod = if_else(yrimmigmod < 1991, 1991, yrimmigmod)) %>%
  left_join(lgbt_policy, by = c('yrimmigmod' = 'year', 'bpldidnew' = 'Code'))

write_csv(acs_count_base, here('data', 'acs_count_base.csv'))


# acs_count_base <- acs_wide %>%
#   mutate(state = as.character(state),
#          mixed_citizenship = (citizen_main == 'Not a citizen' & citizen_partner != 'Not a citizen') | 
#            (citizen_main != 'Not a citizen' & citizen_partner == 'Not a citizen'),
#          noncit = citizen_main == 'Not a citizen' | citizen_partner == 'Not a citizen',
#          bpldnew_main = if_else(is.na(yrimmig_main) & !is.na(yrimmig_partner), bpld_partner, bpld_main),
#          bpldnew_partner = if_else(is.na(yrimmig_partner) & !is.na(yrimmig_main), bpld_main, bpld_partner)) %>%
#   filter(allocated == F) %>%
#   pivot_longer(-c(year, serial, nchild, hhwt, ssmc,
#                   cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
#                   same_sex, imm_couple, married, allocated, mixed_citizenship, noncit),
#                names_to = c('.value', 'position'),
#                names_sep = '_') %>% 
#   filter(age >= 18 & age <= 64) %>%
#   filter(across(c(state, year, same_sex), 
#                 ~ !is.na(.x)))

# acs_count <- acs_count_base %>%
#   group_by(state, year, same_sex, imm_couple) %>%
#   summarize(n = sum(perwt), n_unweighted = n(),
#             n_ar = sum(perwt[qrelate != 'Allocated']), 
#             n_unweighted_ar = sum(qrelate != 'Allocated')) %>%
#   ungroup() %>%
#   complete(state, year, same_sex, imm_couple) %>%
#   replace(is.na(.), 0) %>%
#   mutate(post_2013 = year > 2013,
#          group_fe = paste(state, same_sex, imm_couple, sep = '_'))



acs_count_mixed <- acs_count_base %>%
  rename(mixed = mixed_citizenship) %>%
  # mutate(mixed = case_when(imm_couple == 'one' ~ T,
  #                          imm_couple == 'two' | imm_couple == 'none' ~ F)) %>%
  group_by(state, year, same_sex, mixed, bpldnew) %>%
  summarize(n = sum(perwt), 
            n_unweighted = n(),
            n_ar = sum(perwt[qcitizen != 'Allocated' & qeduc != 'Consistency edit' & allocated == F]), 
            n_unweighted_ar = sum(qcitizen != 'Allocated' & qeduc != 'Consistency edit' & allocated == F)) %>%
  ungroup() %>%
  complete(state, year, same_sex, mixed) %>% 
  mutate(across(n:n_unweighted_ar, function(x) ifelse(is.na(x), 0, x))) %>% 
  mutate(post_2013 = year > 2013,
         group_fe = paste(state, same_sex, mixed, sep = '_')) %>%
  rename(bpld = bpldnew)

acs_count_noncit <- acs_count_base %>%
  rename(mixed = mixed_citizenship) %>%
  # mutate(mixed = case_when(imm_couple == 'one' ~ T,
  #                          imm_couple == 'two' | imm_couple == 'none' ~ F)) %>%
  group_by(state, year, same_sex, noncit, bpldnew) %>%
  summarize(n = sum(perwt), 
            n_unweighted = n(),
            n_ar = sum(perwt[qcitizen != 'Allocated' & qeduc != 'Consistency edit' & allocated == F]), 
            n_unweighted_ar = sum(qcitizen != 'Allocated' & qeduc != 'Consistency edit' & allocated == F)) %>%
  ungroup() %>%
  complete(state, year, same_sex, noncit) %>%
  mutate(across(n:n_unweighted_ar, function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(post_2013 = year > 2013,
         group_fe = paste(state, same_sex, noncit, sep = '_')) %>%
  rename(bpld = bpldnew)


# acs_count_mixed %>%
#   group_by(same_sex, mixed) %>%
#   summarize(sum(n_unweighted),
#             sum(n_unweighted_ar))

acs_count_imm <- acs_count_base %>%
  mutate(immigrant = case_when(imm_couple == 'none' ~ F,
                               imm_couple == 'two' | imm_couple == 'one' ~ T)) %>%
  group_by(state, year, same_sex, immigrant, bpldnew) %>%
  summarize(n = sum(perwt), 
            n_unweighted = n(),
            n_ar = sum(perwt[qcitizen != 'Allocated']), 
            n_unweighted_ar = sum(qcitizen != 'Allocated')) %>%
  ungroup() %>%
  complete(state, year, same_sex, immigrant) %>%
  mutate(across(n:n_unweighted_ar, function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(post_2013 = year > 2013,
         group_fe = paste(state, same_sex, immigrant, sep = '_')) %>%
  rename(bpld = bpldnew)


write_csv(acs_count_mixed, here('data', 'acs_count_mixed.csv'))


## State controls ####
state_policy <- read.csv(here('data', 'state_policy.csv'))

state_policy[is.na(state_policy)] <- 0

state_policy <- state_policy %>%
  mutate(state_policy = -sodomy_illega + -marriage_ban + marriage_equality + 
           civil_union + employment_discrim_so + hate_crime_so + 
           joint_adoption + -adoption_religious_freedom +
           conversion_therapy_ban + housing_discrim_so + 
           state_ban_local_nondiscrimimation) %>%
  select(state = State, year = Year, state_policy, state_couple_laws)

state_policy$state_policy_binned <- cut(state_policy$state_policy, breaks = c(-2,0,2, Inf), 
                                        labels = c("Repressive", "Neutral", "Progressive"))


unemploy <- read.table(here('data', 'la.data.3.AllStatesS.txt'), header = T,
                       fill = T) %>% 
  filter(str_detect(series_id, '00003')) %>%
  as_tibble() %>%
  mutate(value = as.numeric(value),
         state_num = as.numeric(str_extract(series_id, "\\d\\d"))) %>%
  left_join(data.frame(state = c(state.name, 'District of Columbia', 'Puerto Rico'), 
                       state_num =c(1,2,4:6, 8:10, 12, 13, 15:42, 44:51, 53:56, 11, 72))) %>%
  group_by(state, year) %>%
  summarize(state_unemploy = mean(value))

# state_income_df <- read_csv(here('data', 'state_income.csv'), na = c('', '(NA)')) %>%
#   pivot_longer(!1:2, names_to = 'year', values_to = 'state_income') %>%
#   mutate(year = as.integer(year)) %>%
#   rename(state = GeoName) %>% 
#   select(-GeoFips) %>%
#   mutate(state_income = priceR::adjust_for_inflation(state_income, year, "US", to_date = 1999)/1000)

state_income_df <- read_csv(here('data', 'state_income.csv'), na = c('', '(NA)')) %>%
  pivot_longer(!1:2, names_to = 'year', values_to = 'state_income') %>%
  mutate(year = as.integer(year)) %>%
  rename(state = GeoName) %>% 
  select(-GeoFips) %>%
  mutate(state = ifelse(state == 'Alaska *', 'Alaska', state)) %>%
  filter(year >= 2008)

for(year_loop in unique(state_income_df$year)){
  print(year_loop)
  state_income_df$state_income[state_income_df$year == year_loop] <- 
    adjust_for_inflation(state_income_df$state_income[state_income_df$year == year_loop],
                         year_loop,
                         "US",
                         to_date = 1999)/1000
}


state_df <- state_policy %>%
  left_join(unemploy) %>%
  left_join(state_income_df) 
write_csv(state_df, here('data', 'state_df.csv'))

# write_csv(acs_count, here('data', 'acs_count.csv'))
# write_csv(acs_count_noncit, here('data', 'acs_count_noncit.csv'))
# write_csv(acs_count_imm, here('data', 'acs_count_imm.csv'))




# acs_count_countries <- acs_count_base %>%
#   mutate(immigrant = case_when(imm_couple == 'none' ~ F,
#                                imm_couple == 'two' | imm_couple == 'one' ~ T)) %>%
#   group_by(state, bpld, year, same_sex, immigrant) %>%
#   summarize(origin_score = mean(origin_score),
#             n = sum(perwt), 
#             n_unweighted = n(),
#             n_ar = sum(perwt[qcitizen != 'Allocated']), 
#             n_unweighted_ar = sum(qcitizen != 'Allocated')) %>%
#   ungroup() %>%
#   complete(state, year, same_sex, immigrant) %>%
#   replace(is.na(.), 0) %>%
#   mutate(post_2013 = year > 2013,
#          group_fe = paste(state, same_sex, immigrant, sep = '_'))

