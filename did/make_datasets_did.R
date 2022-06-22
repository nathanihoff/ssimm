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

# acs_wide %>%
#   filter(imm_couple != 'none', allocated == F) %>%
#   write_csv(here('data', 'acs_wide.csv'))



## Counts for poisson model ####
acs_count_base <- acs_wide %>%
  mutate(state = as.character(state),
         mixed_citizenship = (citizen_main == 'Not a citizen' & citizen_partner != 'Not a citizen') | 
           (citizen_main != 'Not a citizen' & citizen_partner == 'Not a citizen'),
         noncit = citizen_main == 'Not a citizen' | citizen_partner == 'Not a citizen',
         bpldnew_main = if_else(is.na(yrimmig_main) & !is.na(yrimmig_partner), bpld_partner, bpld_main),
         bpldnew_partner = if_else(is.na(yrimmig_partner) & !is.na(yrimmig_main), bpld_main, bpld_partner)) %>%
  filter(allocated == F) %>%
  pivot_longer(-c(year, serial, nchild, hhwt, ssmc,
                  cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
                  same_sex, imm_couple, married, allocated, mixed_citizenship, noncit),
               names_to = c('.value', 'position'),
               names_sep = '_') %>% 
  filter(age >= 18 & age <= 64) %>%
  filter(across(c(state, year, same_sex), 
                ~ !is.na(.x)))

acs_count <- acs_count_base %>%
  group_by(state, year, same_sex, imm_couple) %>%
  summarize(n = sum(perwt), n_unweighted = n(),
            n_ar = sum(perwt[qrelate != 'Allocated']), 
            n_unweighted_ar = sum(qrelate != 'Allocated')) %>%
  ungroup() %>%
  complete(state, year, same_sex, imm_couple) %>%
  replace(is.na(.), 0) %>%
  mutate(post_2013 = year > 2013,
         group_fe = paste(state, same_sex, imm_couple, sep = '_'))



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

write_csv(acs_count, here('data', 'acs_count.csv'))
write_csv(acs_count_mixed, here('data', 'acs_count_mixed.csv'))
write_csv(acs_count_noncit, here('data', 'acs_count_noncit.csv'))
write_csv(acs_count_imm, here('data', 'acs_count_imm.csv'))




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
  
