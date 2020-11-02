library(RColorBrewer)
library(haven)
library(cowplot)
library(srvyr)
library(here)
library(tidyverse)


acs <- read_dta('/Users/nathan/Data/ACS/acs_2008_2018.dta') %>%
  mutate(position = case_when(
    related == 101 ~ 'main',
    related %in% c(201, 1114) ~ 'partner'),
    year = as.numeric(year),
    bpldid = as.numeric(bpld),
    state = stateicp,
    stateicp = as.numeric(stateicp))

# # Make country-of-origin variables
# # stock by year
# acs_imm <- filter(acs, citizen %in% c(2,3))
# 
# country_year_df <- acs_imm %>%
#   group_by(bpld, state,  year) %>%
#   count(wt = hhwt) %>%
#   rename(state_stock_year = n)
# # sample_n(country_year_df, 10)
# # average stock
# country_df <- acs_imm %>%
#   group_by(bpld, state) %>%
#   count(wt = hhwt) %>%
#   mutate(state_stock_avg = n/11) %>%
#   select(-n)
# 
# acs <- acs %>%
#   left_join(country_year_df) %>%
#   left_join(country_df)

# select(acs, bpld, state, state_stock_year, state_stock_avg) %>% sample_n(10)

# Define immigrant as someone born abroad not to US parents
acs_wide <- acs %>%
  select(
    # household variables
    year, serial, nchild, hhwt, ssmc,
    cluster, strata, metro, region, state, stateicp, ftotinc,
    # individual variables
    position, sex, related, yrimmig, bpld, bpldid, citizen, educ,
    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty
    # state_stock_year, state_stock_avg
  ) %>%
  filter(!is.na(position)) %>%
  mutate(#related = as_factor(related),
    #bpld = as_factor(bpld),
    yrimmig = ifelse(yrimmig == 0, NA, yrimmig)) %>%
  pivot_wider(#id_cols = serial,
    names_from = position,
    values_from = c(sex, related, related, yrimmig, bpld, bpldid, citizen, educ,
                    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty
                    #state_stock_year, state_stock_avg
    )) %>%
  filter(!is.na(sex_partner), !is.na(sex_main)) %>%
  mutate(same_sex = (sex_main == sex_partner),
         imm_couple = case_when(
           citizen_main %in% c(0,1) & citizen_partner %in% c(0,1) ~ 'none',
           citizen_main %in% c(2,3) & citizen_partner %in% c(2,3) ~ 'two',
           citizen_main %in% c(2,3) | citizen_partner %in% c(2,3) ~ 'one')) # %>%
# convert all labeled variables to factor
as_factor()

# Keep only couples with one immigrant
acs_oneimm <- acs_wide %>%
  filter(imm_couple == 'one') %>%
  pivot_longer(-c(year, serial, nchild, hhwt, ssmc,
                  cluster, strata, metro, region, state, stateicp, ftotinc,
                  same_sex, imm_couple
  ),
  names_to = c('.value', 'position'),
  names_sep = '_') %>% 
  mutate(immigrant = case_when(citizen %in% c('N/A', 'Born abroad of American parents') ~ 'nonimmigrant',
                               citizen %in% c('Naturalized citizen', 'Not a citizen') ~ 'immigrant')) %>% 
  filter(!is.na(immigrant)) %>%
  pivot_wider(names_from = immigrant,
              values_from = c(position, sex, related, related, yrimmig, bpld, bpldid, citizen, educ,
                              occ, inctot, occscore, hwsei, empstat, yrnatur, poverty
                              #state_stock_year, state_stock_avg
              )) %>%
  mutate(relation = case_when(
    related_nonimmigrant == "Head/Householder" ~ related_immigrant,
    related_immigrant == "Head/Householder" ~ related_nonimmigrant
  ))




acs_dyad <- acs_wide %>%
  select(bpld_main, bpld_partner, state, year, same_sex, citizen_main, citizen_partner, hhwt, same_sex,
         yrimmig_main, yrimmig_partner) %>%
  pivot_longer(-c(year, state, same_sex, hhwt),
               names_to = c('.value', 'position'),
               names_sep = '_') %>% 
  mutate(immigrant = case_when(citizen %in% c(0,1) ~ 'nonimmigrant',
                               citizen %in% c(2,3) ~ 'immigrant')) %>%
  filter(immigrant == 'immigrant') %>%
  group_by(bpld, state,  year, same_sex) %>%
  count(wt = hhwt, .drop = F) %>%
  rename(stock = n) %>%
  mutate(same_sex = case_when(
    same_sex == T ~ 'same_sex_stock',
    same_sex == F ~ 'opp_sex_stock'
  )) %>%
  pivot_wider(names_from = same_sex,
              values_from = stock, 
              values_fill = 0) %>%
  left_join(country_year_df) %>%
  mutate(bpldid = as.numeric(bpld)) %>%
  as_factor()

acs_dyad_yrimmig <- acs_wide %>%
  select(bpld_main, bpld_partner, state, year, same_sex, citizen_main, citizen_partner, hhwt, same_sex,
         yrimmig_main, yrimmig_partner) %>%
  pivot_longer(-c(year, state, same_sex, hhwt),
               names_to = c('.value', 'position'),
               names_sep = '_') %>% 
  mutate(immigrant = case_when(citizen %in% c(0,1) ~ 'nonimmigrant',
                               citizen %in% c(2,3) ~ 'immigrant')) %>%
  filter(immigrant == 'immigrant')  %>%
  group_by(bpld,  year) %>%
  summarize(mean_year_immig = weighted.mean(yrimmig, w = hhwt, na.rm = T)) %>%
  as_factor()

acs_dyad <- left_join(acs_dyad, acs_dyad_yrimmig)

# Top-10 immigrant countries
top_countries <- acs %>%
  filter(citizen == 2 | citizen == 3) %>%
  group_by(bpld) %>%
  count(wt = hhwt) %>%
  arrange(desc(n)) %>%
  pull(bpld) %>%
  as_factor() %>%
  as.data.frame()

write_csv(top_countries, here('data', 'top_countries.csv'))
write_csv(acs_wide, here('data', 'acs_wide.csv'))
write_csv(acs_oneimm, here('data', 'acs_oneimm.csv'))
write_csv(acs_dyad, here('data', 'acs_dyad.csv'))