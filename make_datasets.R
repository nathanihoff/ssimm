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
    ftotinc = ifelse(ftotinc == 9999999, NA, ftotinc*cpi99/1000),
    inctot = ifelse(inctot == 9999999, NA, inctot*cpi99/1000),
    hwsei = ifelse(hwsei == 0000, NA, hwsei),
    occscore = ifelse(occscore == 00, NA, occscore),
    educ = case_when(
      educ %in% 0:5 ~ '< HS',
      educ == 6 ~ 'HS',
      educ %in% 7:9 ~ 'some col',
      educ %in% 10:11 ~ 'college'),
    bpldid = case_when(as_factor(bpld) == 'England' ~ 41300,
                       as_factor(bpld) == 'Korea' ~ 50220,
                       T ~ bpldid))


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
    position, sex, related, yrimmig, bpld, bpldid, citizen, educ, qrelate, qsex,
    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, speakeng, age, perwt
    # state_stock_year, state_stock_avg
  ) %>%
  pivot_wider(#id_cols = serial,
    names_from = position,
    values_from = c(sex, related, related, yrimmig, bpld, bpldid, citizen, educ,
                    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, 
                    speakeng, age, perwt, qrelate, qsex
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


# Keep only couples with one immigrant
acs_oneimm <- acs_wide %>%
  filter(imm_couple == 'one') %>%
  pivot_longer(-c(year, serial, nchild, hhwt, ssmc,
                  cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
                  same_sex, imm_couple, allocated),
               names_to = c('.value', 'position'),
               names_sep = '_') %>% 
  mutate(immigrant = case_when(citizen %in% c('N/A', 'Born abroad of American parents') ~ 'nonimmigrant',
                               citizen %in% c('Naturalized citizen', 'Not a citizen') ~ 'immigrant')) %>% 
  filter(!is.na(immigrant)) %>%
  pivot_wider(names_from = immigrant,
              values_from = c(position, sex, related, related, yrimmig, bpld, 
                              bpldid, citizen, educ, qrelate, qsex,
                              occ, inctot, occscore, hwsei, empstat, yrnatur, 
                              poverty, speakeng, age, perwt
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
                  cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
                  same_sex, imm_couple, married, allocated),
               names_to = c('.value', 'position'),
               names_sep = '_') %>% 
  # filter(yrimmig >= 1991) %>%
  mutate(immigrant = case_when(citizen %in% c('N/A', 'Born abroad of American parents') ~ 'nonimmigrant',
                               citizen %in% c('Naturalized citizen', 'Not a citizen') ~ 'immigrant')) %>% 
  filter(immigrant == 'immigrant') %>%
  mutate(years_in_us = year - yrimmig) 

# Make country-of-origin variables
# stock by year
acs_imm <- filter(acs, citizen %in% c(2,3)) %>%
  filter(yrimmig >= 1991) %>%
  mutate(related = as_factor(related))

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


# State proportions: Macro dataset of dyadic stock by state by year ####
acs_dyad <- acs_coupled_imms %>%
  filter(yrimmig >= 1991) %>%
  filter(allocated == F) %>%
  group_by(bpld, state,  year, same_sex) %>%
  summarize(n = sum(perwt), 
            n_spouse = sum(perwt[married == T]),
            n_partner = sum(perwt[married == F]),
            n_spouse_oneimm = sum(perwt[imm_couple == 'one' & married == T]),
            n_partner_oneimm = sum(perwt[imm_couple == 'one' & married == F]),
            n_spouse_adj = sum(perwt[married == T & year == 2019]) +
              sum(perwt[respmode == 'Mail' & married == T & year < 2019])*(1-(.59+.474)/2) +
              sum(perwt[respmode == 'CATI/CAPI' & married == T & year < 2019])*(1-.46) +
              sum(perwt[respmode == 'Internet' & married == T & year < 2019])*(1-.225),
            n_partner_adj = sum(perwt[married == F & year == 2019]) +
              sum(perwt[married == F & respmode == 'Mail' & year < 2019])*(1-(.07+.056)/2) +
              sum(perwt[married == F & respmode == 'CATI/CAPI' & year < 2019])*(1-.13) +
              sum(perwt[married == F & respmode == 'Internet' & year < 2019])*(1-.024),
            n_spouse_2019 = sum(perwt[married == T & year == 2019]),
            n_spouse_pre_2019 = sum(perwt[married == T & year < 2019]),
            n_partner_2019 = sum(perwt[married == F & year == 2019]),
            n_partner_pre_2019 = sum(perwt[married == F & year < 2019])) %>%
  ungroup() %>%
  mutate(same_sex = ifelse(same_sex == T, 'same_sex', 'dif_sex')) %>%
  pivot_wider(names_from = 'same_sex', values_from = 5:ncol(.)) %>% 
  replace(is.na(.), 0) %>%
  left_join(country_year_df) 
  
  
  # count(wt = perwt, .drop = F) %>%
  # ungroup() %>%
  # mutate(same_sex = ifelse(same_sex == T, 'same_sex_stock', 'opp_sex_stock')) %>%
  # pivot_wider(names_from = 'same_sex', values_from = 'n') %>%
  # mutate(same_sex_stock = ifelse(is.na(same_sex_stock), 0, same_sex_stock),
  #        opp_sex_stock = ifelse(is.na(opp_sex_stock), 0, opp_sex_stock)) %>%
  # left_join(country_year_df) 

acs_dyad_yrimmig <- acs_coupled_imms %>%
  filter(yrimmig >= 1991) %>%
  group_by(bpld,  year) %>%
  summarize(mean_year_immig = weighted.mean(yrimmig, w = perwt, na.rm = T)) 

acs_dyad <- left_join(acs_dyad, acs_dyad_yrimmig)

# only immigrants in last year
acs_dyad2 <- acs_coupled_imms %>%
  filter(yrimmig >= 1991) %>%
  filter(allocated == F) %>%
  filter(year - yrimmig <= 1) %>%
  group_by(bpld, state,  year, same_sex) %>%
  count(wt = perwt, .drop = F) %>%
  ungroup() %>%
  mutate(same_sex = ifelse(same_sex == T, 'same_sex_stock', 'opp_sex_stock')) %>%
  pivot_wider(names_from = 'same_sex', values_from = 'n') %>%
  mutate(same_sex_stock = ifelse(is.na(same_sex_stock), 0, same_sex_stock),
         opp_sex_stock = ifelse(is.na(opp_sex_stock), 0, opp_sex_stock)) %>%
  left_join(country_year_df) 

acs_dyad_yrimmig2 <- acs_coupled_imms %>%
  filter(yrimmig >= 1991) %>%
  filter(allocated == F) %>%
  filter(year - yrimmig <= 1) %>%
  group_by(bpld,  year) %>%
  summarize(mean_year_immig = weighted.mean(yrimmig, w = perwt, na.rm = T)) 

acs_dyad2 <- left_join(acs_dyad2, acs_dyad_yrimmig2)


# Proportion same sex by year of immigration and country ####
# no weights
country_yrimmig_df <- acs_imm %>%
  filter(yrimmig >= 1991) %>%
  group_by(bpld, yrimmig) %>%
  summarize(n_total = sum(perwt), 
            n_spouse = sum(perwt[related == 'Spouse']), 
            n_partner = sum(perwt[related == 'Unmarried Partner'])) %>%
  mutate(bpld = as_factor(bpld),
         yrimmig = as.numeric(yrimmig))

acs_prop_yrimmig <- acs_wide %>%
  filter(imm_couple != 'none',
         allocated == F) %>%
  pivot_longer(-c(year, serial, nchild, hhwt, ssmc,
                  cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
                  same_sex, imm_couple, married, allocated),
               names_to = c('.value', 'position'),
               names_sep = '_') %>%
  filter(yrimmig >= 1991) %>%
  group_by(yrimmig, bpld, same_sex) %>%
  summarize(n = sum(perwt), 
            n_spouse = sum(perwt[married == T]),
            n_partner = sum(perwt[married == F]),
            n_spouse_oneimm = sum(perwt[imm_couple == 'one' & married == T]),
            n_partner_oneimm = sum(perwt[imm_couple == 'one' & married == F]),
            n_spouse_adj = sum(perwt[married == T & year == 2019]) +
              sum(perwt[respmode == 'Mail' & married == T & year < 2019])*(1-(.59+.474)/2) +
              sum(perwt[respmode == 'CATI/CAPI' & married == T & year < 2019])*(1-.46) +
              sum(perwt[respmode == 'Internet' & married == T & year < 2019])*(1-.225),
            n_partner_adj = sum(perwt[married == F & year == 2019]) +
              sum(perwt[married == F & respmode == 'Mail' & year < 2019])*(1-(.07+.056)/2) +
              sum(perwt[married == F & respmode == 'CATI/CAPI' & year < 2019])*(1-.13) +
              sum(perwt[married == F & respmode == 'Internet' & year < 2019])*(1-.024),
            n_spouse_2019 = sum(perwt[married == T & year == 2019]),
            n_spouse_pre_2019 = sum(perwt[married == T & year < 2019]),
            n_partner_2019 = sum(perwt[married == F & year == 2019]),
            n_partner_pre_2019 = sum(perwt[married == F & year < 2019])) %>%
  # summarize(n = n(), 
  #           n_spouse = sum(married == T),
  #           n_partner = sum(married == F),
  #           n_spouse_oneimm = sum(imm_couple == 'one' & married == T),
  #           n_partner_oneimm = sum(imm_couple == 'one' & married == F),
  #           # n_spouse_mail = sum(respmode == 'Mail' & married == T), 
  #           # n_spouse_cati = sum(respmode == 'CATI/CAPI' & married == T), 
  #           # n_spouse_internet = sum(respmode == 'Internet' & married == T), 
  #           # n_partner_mail = sum(married == F & respmode == 'Mail'), 
  #           # n_partner_cati = sum(married == F & respmode == 'CATI/CAPI'), 
  #           # n_partner_internet = sum(married == F & respmode == 'Internet'),
  #           n_spouse_adj = sum(married == T & year == 2019) +
  #             sum(respmode == 'Mail' & married == T & year < 2019)*(1-(.59+.474)/2) +
  #             sum(respmode == 'CATI/CAPI' & married == T & year < 2019)*(1-.46) +
  #             sum(respmode == 'Internet' & married == T & year < 2019)*(1-.225),
  #           n_partner_adj = sum(married == F & year == 2019) +
  #             sum(married == F & respmode == 'Mail' & year < 2019)*(1-(.07+.056)/2) +
  #             sum(married == F & respmode == 'CATI/CAPI' & year < 2019)*(1-.13) +
  #             sum(married == F & respmode == 'Internet' & year < 2019)*(1-.024),
  #           n_spouse_2019 = sum(married == T & year == 2019),
  #           n_spouse_pre_2019 = sum(married == T & year < 2019),
  #           n_partner_2019 = sum(married == F & year == 2019),
  #           n_partner_pre_2019 = sum(married == F & year < 2019)) %>%
  ungroup() %>%
  mutate(same_sex = ifelse(same_sex == T, 'same_sex', 'dif_sex')) %>%
  pivot_wider(names_from = 'same_sex', values_from = 4:ncol(.)) %>% 
  replace(is.na(.), 0) %>%
  left_join(country_yrimmig_df) %>%
  left_join(distinct(dplyr::select(acs_dyad, bpld, bpldid))) %>%
  mutate(
    # prop_spouse_same_sex_adj = (n_spouse_mail_same_sex*(1-(.59+.474)/2) +
    #        n_spouse_cati_same_sex*(1-.46) +
    #          n_spouse_internet_same_sex*(1-.225)) / n_total * 100,
    #      prop_partner_same_sex_adj = (n_partner_mail_same_sex*(1-(.07+.056)/2) +
    #                                 n_partner_cati_same_sex*(1-.13) +
    #                                 n_partner_internet_same_sex*(1-.024))
    #        / n_total * 100,
    #prop_same_sex = n_same_sex / n_total*100,
    prop_spouse_same_sex_adj = n_spouse_adj_same_sex / n_total * 100,
    prop_partner_same_sex_adj = n_partner_adj_same_sex / n_total * 100,
    prop_same_sex_adj = prop_spouse_same_sex_adj + prop_partner_same_sex_adj,
    prop_dif_sex = n_dif_sex / n_total * 100,
    prop_spouse_same_sex = n_spouse_same_sex / n_total * 100,
    prop_partner_same_sex = n_partner_same_sex / n_total * 100,
    prop_same_sex = prop_spouse_same_sex + prop_partner_same_sex,
    prop_same_sex_oneimm = (n_spouse_oneimm_same_sex + n_partner_oneimm_same_sex) / n_total * 100)




# acs_prop_yrimmig %>%
#   pivot_longer(c(prop_same_sex, prop_dif_sex)) %>%
#   filter(value != 0) %>%
#   ggplot(aes(x = value)) +
#   geom_histogram() +
#   facet_wrap(~name)


# Top-10 immigrant countries
# top_countries <- acs %>%
#   filter(citizen == 2 | citizen == 3) %>%
#   group_by(bpld) %>%
#   count(wt = perwt) %>%
#   arrange(desc(n)) %>%
#   pull(bpld) %>%
#   as_factor() %>%
#   as.data.frame()
# write_csv(top_countries, here('data', 'top_countries.csv'))

write_csv(acs_prop_yrimmig, here('data', 'acs_prop_yrimmig.csv'))
write_csv(acs_coupled_imms, here('data', 'acs_coupled_imms.csv'))
write_csv(acs_dyad, here('data', 'acs_dyad.csv'))
write_csv(acs_dyad2, here('data', 'acs_dyad2.csv'))

acs_wide %>%
  filter(imm_couple != 'none', allocated == F) %>%
  write_csv(here('data', 'acs_wide.csv'))
write_csv(acs_oneimm, here('data', 'acs_oneimm.csv'))



## Making final datasets ####
acs_coupled_imms <- read.csv(here('data', 'acs_coupled_imms.csv'))
acs_prop_yrimmig <- read.csv(here('data', 'acs_prop_yrimmig.csv'))
acs_dyad <- read.csv(here('data', 'acs_dyad.csv'))


dist_dat <- read_dta(here('data', 'dist_cepii.dta')) %>%
  filter(iso_d == 'USA') %>%
  select(-iso_d)

# Wage difference = USA - country of origin
penn_wages <- read.csv(here('data', 'penn_wages.csv')) %>%
  mutate(pc_income = rgdpe /pop / 1000) %>%
  select(iso_o = countrycode, country, year, pc_income, rgdpe, pop)
usa_wages <- filter(penn_wages, iso_o == 'USA')
penn_wages$wage_dif <- NA
for(country_loop in unique(penn_wages$country)){
  for(year in penn_wages$year[penn_wages$country == country_loop]){
    penn_wages$wage_dif[penn_wages$year == year & penn_wages$country == country_loop] <-
      usa_wages$pc_income[usa_wages$year == year] -
      penn_wages$pc_income[penn_wages$year == year & penn_wages$country == country_loop]
    # penn_wages$usa_pc_income[penn_wages$year == year & penn_wages$country == country_loop] <- 
    #   usa_wages$pc_income[usa_wages$year == year]
  }
}
penn_wages <- penn_wages %>%
  mutate(wage_dif = adjust_for_inflation(wage_dif, 2017, "US", to_date = 1999))

# filter(penn_wages, is.na(wage_dif) & !is.na(rgdpe)) %>%
#   View()
wb_unemp <- read_csv(here('data', 'world_bank_unemployment.csv')) %>%
  pivot_longer(!1:4, names_to = 'year', values_to = 'unemployment') %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  rename(country = `Country Name`, iso_o = `Country Code`) %>%
  mutate(year = as.numeric(year))
usa_unemp <- filter(wb_unemp, iso_o == 'USA')
wb_unemp$unemp_dif <- NA
for(year in usa_unemp$year){
  for(country in unique(wb_unemp$country)){
    wb_unemp$unemp_dif[wb_unemp$year == year & wb_unemp$country == country] <-
      usa_unemp$unemployment[usa_unemp$year == year] -
      wb_unemp$unemployment[wb_unemp$year == year & wb_unemp$country == country]
  }
}

# vdem <- read.csv(here('data', 'V-Dem-CY-Core-v10.csv'))

vdem <- read_csv(here('data', 'V-Dem-CY-Core-v11.1.csv')) %>%
  select(country_name, country_text_id, COWcode, year, v2x_libdem) %>%
  mutate(iso_o1 = countrycode(COWcode, origin = 'cowc', destination = 'iso3c'),
         iso_o2 = countrycode(country_name, origin = 'country.name', destination = 'iso3c'),
         iso_o = ifelse(!is.na(iso_o1), iso_o1, iso_o2)) %>%
  select(iso_o, country = country_name, year, vdem = v2x_libdem) %>%
  filter(!is.na(iso_o))


# polity5 <- read.csv(here('data', 'polity5.csv')) %>%
#   mutate(iso_o1 = countrycode(scode, origin = 'cowc', destination = 'iso3c'),
#          iso_o2 = countrycode(country, origin = 'country.name', destination = 'iso3c'),
#          iso_o = ifelse(!is.na(iso_o1), iso_o1, iso_o2)) %>%
#   select(iso_o, country, year, polity5 = polity2) %>%
#   filter(!is.na(iso_o))
# 
# polity5 <- polity5 %>%
#   group_by(iso_o) %>%
#   expand(year = full_seq(min(year):2020, 1)) %>%
#   left_join(polity5)
# 
# # Linearly interpolate polity5 missing values during regime change
# polity5_list <- list()
# for(country_loop in unique(polity5$iso_o)){
#   polity5_loop <- with(filter(polity5, iso_o == country_loop), 
#                        zoo(polity5, min(year):2020)) %>%
#     na_interpolation(option = "linear") #%>%
#   #na_locf()
#   
#   polity5_list[[country_loop]] <- bind_cols(iso_o = country_loop,
#                                             year = index(polity5_loop), 
#                                             polity5 = as.data.frame(polity5_loop)[[1]])
# }
# polity5 <- bind_rows(polity5_list)

# Policy variables
lgbt_policy <- read.csv(here('data', 'lgb_origin_index.csv')) %>%
  select(Country, year, Code, origin_score = origin_couple_score) %>%
  mutate(iso_o = countrycode(Country, origin = 'country.name', destination = 'iso3c')) %>%
  left_join(select(penn_wages, -country)) %>%
  left_join(select(wb_unemp, -country)) %>%
  left_join(vdem) %>%
  left_join(dist_dat) %>%
  arrange(Country, year)


# migrant stock by year
# UN data: 1990-2017, every 5 years
un_mig <- read_csv(here('data', 'un_migration_1990_2017.csv')) %>%
  filter(`Major area, region, country or area of destination` == 'United States of America') %>%
  select(-c(2:6), year = Year) %>%
  pivot_longer(-year, names_to = 'country', values_to = 'n') %>%
  mutate(n = as.numeric(gsub("\\,", "", n)),
         iso_o = countrycode(country, origin = 'country.name', destination = 'iso3c'))
un_mig$prop <- NA
for(year_loop in unique(un_mig$year)){
  for(country_loop in unique(un_mig$country)){
    un_mig$prop[un_mig$year == year_loop & un_mig$country == country_loop] <- 
      un_mig$n[un_mig$year == year_loop & un_mig$country == country_loop] /
      un_mig$n[un_mig$year == year_loop & un_mig$country == 'Total']
  }
}

# World bank data, 1960-1980
gbmd <- read_csv(here('data', 'gbmd.csv')) %>%
  select(country = `Country Origin Name`,
         iso_o = `Country Origin Code`,
         7:9) %>%
  pivot_longer(-c(country, iso_o), names_to = 'year', values_to = 'n') %>%
  mutate(year = as.numeric(substr(year, 1, 4)),
         n = as.numeric(n))
gbmd$prop <- NA
total <- list()
for(year_loop in unique(gbmd$year)){
  total[[as.character(year_loop)]] <- sum(gbmd$n[gbmd$year == year_loop], na.rm = T)
}
for(i in 1:nrow(gbmd)){
  gbmd$prop[i] <- gbmd$n[i] / total[[as.character(gbmd$year[i])]]
}

# combine stock datasets and linearly interpolate
yearly_prop <- bind_rows(gbmd, select(un_mig, names(gbmd))) %>%
  drop_na()
yearly_prop <- yearly_prop %>%
  group_by(iso_o) %>%
  expand(year = full_seq(1960:2020, 1)) %>%
  left_join(yearly_prop)
yearly_prop_list <- list()
for(country_loop in unique(yearly_prop$iso_o)){
  yearly_prop_loop <- with(filter(yearly_prop, iso_o == country_loop), 
                           zoo(prop, 1960:2020)) %>%
    na_interpolation(option = "linear")
  yearly_prop_list[[country_loop]] <- bind_cols(iso_o = country_loop,
                                                year = index(yearly_prop_loop), 
                                                stock_prop = as.data.frame(yearly_prop_loop)[[1]])
}
yearly_prop <- bind_rows(yearly_prop_list) %>%
  left_join(select(lgbt_policy, iso_o, bpldid = Code) %>% distinct())


state_policy <- read.csv(here('data', 'state_policy.csv'))

state_policy[is.na(state_policy)] <- 0

state_policy <- state_policy %>%
  mutate(state_policy = -sodomy_illega + -marriage_ban + marriage_equality + civil_union + 
           employment_discrim_so + hate_crime_so + joint_adoption + -adoption_religious_freedom +
           conversion_therapy_ban + housing_discrim_so + state_ban_local_nondiscrimimation) %>%
  select(State, Year, state_policy, state_couple_laws)

acs_prop_yrimmig_policy <- acs_prop_yrimmig %>%
  # mutate(pre_1991 = yrimmig <1991,
  #        yrimmig = ifelse(yrimmig < 1991, 1991, yrimmig)) %>%
  left_join(lgbt_policy, by = c('yrimmig' = 'year', 'bpldid' = 'Code')) %>%
  left_join(yearly_prop, by = c('yrimmig' = 'year', 'iso_o' = 'iso_o')) %>%
  filter(!is.na(origin_score)) %>%
  mutate(prop_same_sex = prop_same_sex,
         prop_dif_sex = prop_dif_sex,
         prop_same_std = (prop_same_sex - mean(prop_same_sex))/sd(prop_same_sex),
         prop_dif_std = (prop_dif_sex - mean(prop_dif_sex))/sd(prop_dif_sex),
         origin_std = (origin_score - mean(origin_score))/sd(origin_score),
         yr_fac = as.factor(yrimmig)) %>%
  # include only complete cases for regressions
  drop_na(prop_same_sex, distw, contig, comlang_off, comlang_ethno, colony,
          wage_dif, unemp_dif, vdem, origin_score, bpld)


# State-level analysis
acs_dyad_policy1 <- acs_dyad %>%
  mutate(mean_year_immig = round(mean_year_immig)) %>%
  left_join(yearly_prop, by = c('mean_year_immig' = 'year', 'bpldid')) %>%
  filter(mean_year_immig >= 1991) %>%
  # mutate(pre_1991 = mean_year_immig < 1991,
  #   mean_year_immig = ifelse(mean_year_immig >= 1991, 
  #                                 mean_year_immig,
  #                                 1991)) %>%
  left_join(lgbt_policy, by = c('mean_year_immig' = 'year', 'bpldid' = 'Code')) %>%
  left_join(state_policy,by = c('year' = 'Year', 'state' = 'State')) %>%
  filter(!is.na(origin_score)) %>%
  mutate(
    prop_spouse_same_sex_adj = n_spouse_adj_same_sex / state_stock_year * 100,
    prop_partner_same_sex_adj = n_partner_adj_same_sex / state_stock_year * 100,
    prop_same_sex_adj = prop_spouse_same_sex_adj + prop_partner_same_sex_adj,
    dif_prop = n_dif_sex / state_stock_year * 100,
    prop_spouse_same_sex = n_spouse_same_sex / state_stock_year * 100,
    prop_partner_same_sex = n_partner_same_sex / state_stock_year * 100,
    same_prop = prop_spouse_same_sex + prop_partner_same_sex,
    prop_same_sex_oneimm = (n_spouse_oneimm_same_sex + n_partner_oneimm_same_sex) / state_stock_year * 100)
  
  
  # mutate(same_prop = same_sex_stock / state_stock_year * 100,
  #        dif_prop = opp_sex_stock / state_stock_year * 100)


# acs_dyad_policy1_old <- acs_dyad %>%
#   mutate(mean_year_immig = round(mean_year_immig)) %>%
#   left_join(yearly_prop, by = c('mean_year_immig' = 'year', 'bpldid')) %>%
#   filter(mean_year_immig >= 1991) %>%
#   # mutate(pre_1991 = mean_year_immig < 1991,
#   #   mean_year_immig = ifelse(mean_year_immig >= 1991, 
#   #                                 mean_year_immig,
#   #                                 1991)) %>%
#   left_join(lgbt_policy, by = c('mean_year_immig' = 'year', 'bpldid' = 'Code')) %>%
#   left_join(state_policy,by = c('mean_year_immig' = 'Year', 'state' = 'State')) %>%
#   filter(!is.na(origin_score)) %>%
#   mutate(same_prop = same_sex_stock / state_stock_year * 100,
#          dif_prop = opp_sex_stock / state_stock_year * 100)


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

state_income_df <- read_csv(here('data', 'state_income.csv'), na = c('', '(NA)')) %>%
  pivot_longer(!1:2, names_to = 'year', values_to = 'state_income') %>%
  mutate(year = as.integer(year)) %>%
  rename(state = GeoName) %>% 
  select(-GeoFips) %>%
  mutate(state_income = adjust_for_inflation(state_income, year, "US", to_date = 1999)/1000)

acs_dyad_policy <- acs_dyad_policy1 %>%
  left_join(unemploy,  by = c('year', 'state')) %>%
  left_join(state_income_df,  by = c('year', 'state'))  %>%
  drop_na(state_policy, origin_score, distw, contig, comlang_off, comlang_ethno, colony,
          wage_dif, unemp_dif, vdem, state_unemploy, state_income, Country, state)

# acs_dyad_policy_old <- acs_dyad_policy1_old %>%
#   left_join(unemploy,  by = c('mean_year_immig' = 'year', 'state')) %>%
#   left_join(state_income_df,  by = c('mean_year_immig' = 'year', 'state'))  %>%
#   drop_na(state_policy, origin_score, distw, contig, comlang_off, comlang_ethno, colony,
#           wage_dif, unemp_dif, vdem, state_unemploy, state_income, Country, state)

# Individual dataset
# Binning state policy into three categories
state_policy$state_policy_binned <- cut(state_policy$state_policy, breaks = c(-2,0,2, Inf), 
                                        labels = c("Repressive", "Neutral", "Progressive"))    

acs_couple_policy <- acs_coupled_imms %>%
  left_join(yearly_prop, by = c('yrimmig' = 'year', 'bpldid')) %>%
  # filter(yrimmig >= 1991) %>%
  # mutate(pre_1991 = yrimmig < 1991,
  #   yrimmig = ifelse(yrimmig >= 1991, 
  #                         round(yrimmig),
  #                         1991)) %>%
  left_join(lgbt_policy, by = c('yrimmig' = 'year', 'bpldid' = 'Code')) %>%
  left_join(state_policy,by = c('year' = 'Year', 'state' = 'State')) %>%
  left_join(unemploy) %>%
  left_join(state_income_df) %>%
  # adding (log) income variables
  mutate(pos_income = ifelse(inctot >= 0, inctot, 0),
         no_income = (inctot <= 0),
         log_income = log(pos_income+1)) %>%
  drop_na(state_policy_binned, same_sex, origin_score, sex, age, educ, qrelate, qsex,
          nchild, log_income, no_income, yrimmig, year) %>%
  filter(allocated == F)


write_rds(acs_couple_policy, here('data', 'acs_couple_policy.rds'))
write_csv(acs_prop_yrimmig_policy, here('data', 'acs_prop_yrimmig_policy.csv'))
write_csv(acs_dyad_policy, here('data', 'acs_dyad_policy.csv'))
# write_csv(acs_dyad_policy_old, here('data', 'acs_dyad_policy_old.csv'))


acs_prop_yrimmig_policy_old <- read_csv(here('data', 'acs_prop_yrimmig_policy_old.csv'))


