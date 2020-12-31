library(RColorBrewer)
library(haven)
library(cowplot)
library(srvyr)
library(here)
library(tidyverse)

acs <- read_dta('/Users/nathan/Data/ACS/acs_2008_2019.dta') %>%
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
    ftotinc = ifelse(ftotinc == 9999999, NA, ftotinc*cpi99),
    inctot = ifelse(inctot == 9999999, NA, inctot*cpi99),
    hwsei = ifelse(hwsei == 0000, NA, hwsei),
    occscore = ifelse(occscore == 00, NA, occscore),
    educ = case_when(
      educ %in% 0:5 ~ '< HS',
      educ == 6 ~ 'HS',
      educ %in% 7:9 ~ 'some col',
      educ %in% 10:11 ~ 'college'))

acs <- acs %>%
  mutate(bpldid = case_when(as_factor(bpld) == 'England' ~ 41300,
                            as_factor(bpld) == 'Korea' ~ 50220,
                            T ~ bpldid))

# filter(acs, as.character(bpld) == 'england')
# Define immigrant as someone born abroad not to US parents
acs_wide <- acs %>%
  # Only  those who immigrated when 18+
  filter(!is.na(position)) %>%
  filter(age - (year - yrimmig) >= 18 | is.na(yrimmig)) %>%
  select(
    # household variables
    year, serial, nchild, hhwt, ssmc,
    cluster, strata, metro, region, state, stateicp, ftotinc,
    # individual variables
    position, sex, related, yrimmig, bpld, bpldid, citizen, educ,
    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, speakeng, age, perwt
    # state_stock_year, state_stock_avg
  ) %>%
  pivot_wider(#id_cols = serial,
    names_from = position,
    values_from = c(sex, related, related, yrimmig, bpld, bpldid, citizen, educ,
                    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, speakeng, age, perwt
                    #state_stock_year, state_stock_avg
    )) %>%
  filter(!is.na(sex_partner), !is.na(sex_main)) %>%
  mutate(same_sex = (sex_main == sex_partner),
         imm_couple = case_when(
           citizen_main %in% c(0,1) & citizen_partner %in% c(0,1) ~ 'none',
           citizen_main %in% c(2,3) & citizen_partner %in% c(2,3) ~ 'two',
           citizen_main %in% c(2,3) | citizen_partner %in% c(2,3) ~ 'one')) %>%
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
                              occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, speakeng, age, perwt
                              #state_stock_year, state_stock_avg
              )) %>%
  mutate(relation = case_when(
    related_nonimmigrant == "Head/Householder" ~ related_immigrant,
    related_immigrant == "Head/Householder" ~ related_nonimmigrant
  ))


# Keep only couples with one immigrant, individual-level
acs_coupled_imms <- acs_wide %>%
  filter(imm_couple == 'one' | imm_couple == 'two') %>%
  pivot_longer(-c(year, serial, nchild, hhwt, ssmc,
                  cluster, strata, metro, region, state, stateicp, ftotinc,
                  same_sex, imm_couple),
               names_to = c('.value', 'position'),
               names_sep = '_') %>% 
  mutate(immigrant = case_when(citizen %in% c('N/A', 'Born abroad of American parents') ~ 'nonimmigrant',
                               citizen %in% c('Naturalized citizen', 'Not a citizen') ~ 'immigrant')) %>% 
  filter(immigrant == 'immigrant') %>%
  mutate(years_in_us = year - yrimmig) 

# Make country-of-origin variables
# stock by year
acs_imm <- filter(acs, citizen %in% c(2,3))

country_year_df <- acs_imm %>%
  group_by(bpld, state,  year) %>%
  count(wt = perwt) %>%
  rename(state_stock_year = n) %>%
  mutate(bpldid = as.numeric(bpld)) %>%
  as_factor()

# sample_n(country_year_df, 10)
# average stock
# country_df <- acs_imm %>%
#   group_by(bpld, state) %>%
#   count(wt = perwt) %>%
#   mutate(state_stock_avg = n/11) %>%
#   select(-n)
# 
# acs <- acs %>%
#   left_join(country_year_df) %>%
#   left_join(country_df)

# select(acs, bpld, state, state_stock_year, state_stock_avg) %>% sample_n(10)


# Macro dataset of dyadic stock by state by year
acs_dyad <- acs_coupled_imms %>%
  group_by(bpld, state,  year, same_sex) %>%
  count(wt = perwt, .drop = F) %>%
  ungroup() %>%
  mutate(same_sex = ifelse(same_sex == T, 'same_sex_stock', 'opp_sex_stock')) %>%
  pivot_wider(names_from = 'same_sex', values_from = 'n') %>%
  mutate(same_sex_stock = ifelse(is.na(same_sex_stock), 0, same_sex_stock),
         opp_sex_stock = ifelse(is.na(opp_sex_stock), 0, opp_sex_stock)) %>%
  left_join(country_year_df) 

acs_dyad_yrimmig <- acs_coupled_imms %>%
  group_by(bpld,  year) %>%
  summarize(mean_year_immig = weighted.mean(yrimmig, w = perwt, na.rm = T)) 

acs_dyad <- left_join(acs_dyad, acs_dyad_yrimmig)


# proportion same sex by year of immigration and country
# no weights
country_yrimmig_df <- acs_imm %>%
  group_by(bpld, yrimmig) %>%
  count(.drop = F) %>%
  rename(n_total = n) %>%
  mutate(bpld = as_factor(bpld),
         yrimmig = as.numeric(yrimmig))

acs_prop_yrimmig <- acs_coupled_imms %>%
  group_by(yrimmig, bpld, same_sex) %>%
  count(.drop = F) %>%
  ungroup() %>%
  mutate(same_sex = ifelse(same_sex == T, 'n_same_sex', 'n_dif_sex')) %>%
  pivot_wider(names_from = 'same_sex', values_from = 'n') %>%
  mutate(n_same_sex = ifelse(is.na(n_same_sex), 0, n_same_sex),
         n_dif_sex = ifelse(is.na(n_dif_sex), 0, n_dif_sex)) %>%
  left_join(country_yrimmig_df) %>%
  left_join(distinct(select(acs_dyad, bpld, bpldid))) %>%
  mutate(prop_same_sex = n_same_sex / n_total,
         se_same_sex = sqrt(prop_same_sex*(1-prop_same_sex)/n_total),
         prop_dif_sex = n_dif_sex / n_total,
         se_dif_sex = sqrt(prop_dif_sex*(1-prop_dif_sex)/n_total)) 





# acs_prop_yrimmig %>%
#   pivot_longer(c(prop_same_sex, prop_dif_sex)) %>%
#   filter(value != 0) %>%
#   ggplot(aes(x = value)) +
#   geom_histogram() +
#   facet_wrap(~name)


# Top-10 immigrant countries
top_countries <- acs %>%
  filter(citizen == 2 | citizen == 3) %>%
  group_by(bpld) %>%
  count(wt = perwt) %>%
  arrange(desc(n)) %>%
  pull(bpld) %>%
  as_factor() %>%
  as.data.frame()








write_csv(top_countries, here('data', 'top_countries.csv'))
write_csv(acs_wide, here('data', 'acs_wide.csv'))
write_csv(acs_oneimm, here('data', 'acs_oneimm.csv'))
write_csv(acs_coupled_imms, here('data', 'acs_coupled_imms.csv'))
write_csv(acs_dyad, here('data', 'acs_dyad.csv'))
