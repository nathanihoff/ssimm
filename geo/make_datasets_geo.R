library(zoo)
library(priceR)
library(kableExtra)
library(sjstats)
library(imputeTS)
library(countrycode)
library(RColorBrewer)
library(haven)
library(srvyr)
library(here)
library(tidyverse)


## Reshape ACS ####
acs <- read_dta('/Users/nathan/Data/ACS/acs_2008_2022.dta') %>%
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
    hhid = paste0(year, '_', serial),
    educ = case_when(
      educ %in% 0:5 ~ '< HS',
      educ == 6 ~ 'HS',
      educ %in% 7:9 ~ 'some col',
      educ %in% 10:11 ~ 'college'),
    bpldid = case_when(bpld %in% c(41000, 41100, 41410) ~ 41300, # England, Scotland, Northern Ireland
                       as_factor(bpld) == 'Korea' ~ 50220,
                       # as_factor(bpld) == 'Hong Kong' ~ 50000,
                       T ~ bpldid)) %>%
  filter(year != 2020)


# PUMA variables

# set.seed(100)
acs_geo <- acs %>%
  # filter(year == 2019) %>%
  # sample_n(1000) %>%
  # as_survey_design(weights = perwt) %>%
  group_by(puma, year) %>%
  summarize(bachelors_puma = sum(perwt[educ == 'college' & age >= 25], na.rm = T)/sum(perwt, na.rm = T),
            black_puma = sum(perwt[race == 2], na.rm = T)/sum(perwt, na.rm = T),
            hispanic_puma = sum(perwt[hispan != 0], na.rm = T)/sum(perwt, na.rm = T),
            inctot_puma = sum(perwt*inctot, na.rm = T)/sum(perwt, na.rm = T),
            log_inctot_puma = log(inctot_puma),
            owned_puma = sum(perwt[ownershp == 1], na.rm = T)/sum(perwt, na.rm = T),
            age_puma = sum(perwt*age, na.rm = T)/sum(perwt, na.rm = T),
            immigrant_puma = sum(perwt[citizen %in% c(2,3)], na.rm = T)/sum(perwt, na.rm = T),
            poverty_100_puma = sum(perwt[poverty <= 100 & poverty != 0], na.rm = T)/sum(perwt, na.rm = T),
            poverty_200_puma = sum(perwt[poverty <= 200 & poverty != 0], na.rm = T)/sum(perwt, na.rm = T),
            hwsei_puma = sum(perwt*hwsei, na.rm = T)/sum(perwt, na.rm = T),
            unemployed_puma = sum(perwt[empstat == 2 & age >= 25], na.rm = T)/sum(perwt, na.rm = T),
            valueh_puma = sum(perwt*valueh, na.rm = T)/sum(perwt, na.rm = T),
            rent_puma = sum(perwt*rent, na.rm = T)/sum(perwt, na.rm = T),
            costelec_puma = sum(perwt*costelec, na.rm = T)/sum(perwt, na.rm = T)
  )
  
write_csv(acs_geo, here('data', 'acs_geo.csv'))


# acs_geo_survey <- acs %>%
#   # filter(year == 2019) %>%
#   # sample_n(1000) %>%
#   as_survey_design(weights = perwt) %>%
#   group_by(puma, year) %>%
#   summarize(bachelors_puma = survey_mean(educ == 'college' & age >= 25, na.rm = T),
#             black_puma = survey_mean(race == 2, na.rm = T),
#             hispanic_puma = survey_mean(hispan != 0, na.rm = T),
#             inctot_puma = survey_mean(inctot, na.rm = T),
#             log_inctot_puma = log(inctot_puma),
#             owned_puma = survey_mean(ownershp == 1, na.rm = T),
#             age_puma = survey_mean(age, na.rm = T),
#             immigrant_puma = survey_mean(citizen %in% c(2,3), na.rm = T),
#             poverty_100_puma = survey_mean(poverty <= 100 & poverty != 0, na.rm = T),
#             poverty_200_puma = survey_mean(poverty <= 200 & poverty != 0, na.rm = T),
#             hwsei_puma = survey_mean(hwsei, na.rm = T),
#             unemployed_puma = survey_mean(empstat == 2 & age >= 25, na.rm = T),
#             valueh_puma = survey_mean(valueh, na.rm = T),
#             rent_puma = survey_mean(rent, na.rm = T),
#             costelec_puma = survey_mean(costelec, na.rm = T)
#)


acs_geo_survey_mean <- acs %>%
  # filter(year == 2019) %>%
  # sample_n(1000) %>%
  group_by(puma, year) %>%
  summarize(bachelors_puma = weighted.mean(educ == 'college' & age >= 25, w = perwt, na.rm = T),
            black_puma = weighted.mean(race == 2, w = perwt, na.rm = T),
            hispanic_puma = weighted.mean(hispan != 0, w = perwt, na.rm = T),
            inctot_puma = weighted.mean(inctot, w = perwt, na.rm = T),
            log_inctot_puma = log(inctot_puma),
            owned_puma = weighted.mean(ownershp == 1, w = perwt, na.rm = T),
            age_puma = weighted.mean(age, w = perwt, na.rm = T),
            immigrant_puma = weighted.mean(citizen %in% c(2,3), w = perwt, na.rm = T),
            poverty_100_puma = weighted.mean(poverty <= 100 & poverty != 0, w = perwt, na.rm = T),
            poverty_200_puma = weighted.mean(poverty <= 200 & poverty != 0, w = perwt, na.rm = T),
            hwsei_puma = weighted.mean(hwsei, w = perwt, na.rm = T),
            unemployed_puma = weighted.mean(empstat == 2 & age >= 25, w = perwt, na.rm = T),
            valueh_puma = weighted.mean(valueh, w = perwt, na.rm = T),
            rent_puma = weighted.mean(rent, w = perwt, na.rm = T),
            costelec_puma = weighted.mean(costelec, w = perwt, na.rm = T)
            )

# Cochrane SE
# Donald F. Gatz and Luther Smith https://www.sciencedirect.com/science/article/pii/135223109400210C
# https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation

weighted.var.se <- function(x, w, na.rm=F){
  #  Computes the variance of a weighted mean following Cochran 1977 definition
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}

acs_geo_survey_se <- acs %>%
  # filter(year == 2019) %>%
  # sample_n(1000) %>%
  group_by(puma, year) %>%
  summarize(bachelors_puma = weighted.var.se(educ == 'college' & age >= 25, w = perwt, na.rm = T),
            black_puma = weighted.var.se(race == 2, w = perwt, na.rm = T),
            hispanic_puma = weighted.var.se(hispan != 0, w = perwt, na.rm = T),
            inctot_puma = weighted.var.se(inctot, w = perwt, na.rm = T),
            log_inctot_puma = sqrt(log(inctot_puma^2)),
            owned_puma = weighted.var.se(ownershp == 1, w = perwt, na.rm = T),
            age_puma = weighted.var.se(age, w = perwt, na.rm = T),
            immigrant_puma = weighted.var.se(citizen %in% c(2,3), w = perwt, na.rm = T),
            poverty_100_puma = weighted.var.se(poverty <= 100 & poverty != 0, w = perwt, na.rm = T),
            poverty_200_puma = weighted.var.se(poverty <= 200 & poverty != 0, w = perwt, na.rm = T),
            hwsei_puma = weighted.var.se(hwsei, w = perwt, na.rm = T),
            unemployed_puma = weighted.var.se(empstat == 2 & age >= 25, w = perwt, na.rm = T),
            valueh_puma = weighted.var.se(valueh, w = perwt, na.rm = T),
            rent_puma = weighted.var.se(rent, w = perwt, na.rm = T),
            costelec_puma = weighted.var.se(costelec, w = perwt, na.rm = T)
  )


acs_geo_survey <- acs_geo_survey_mean %>%
  # rename_with(~paste0(.x, '_mean'), !any_of(c('puma', 'year'))) %>%
  left_join(
    acs_geo_survey_se %>%
    rename_with(~paste0(.x, '_se'), !any_of(c('puma', 'year')))
    ) %>%
  # use delta method to get approximation for variance of log
  # https://stats.stackexchange.com/questions/418313/variance-of-x-and-variance-of-logx-how-to-relate-them
  mutate(log_inctot_puma_se = sqrt(inctot_puma_se^2/inctot_puma^2)) 

write_csv(acs_geo_survey, here('data', 'acs_geo_survey.csv'))        

            
  



# Define immigrant as someone born abroad not to US parents
acs_wide <- acs %>%
  # Only  those who immigrated when 18+
  filter(!is.na(position)) %>%
  filter(age - (year - yrimmig) >= 18 | is.na(yrimmig)) %>%
  select(
    # household variables
    year, serial, nchild, hhwt, ssmc,
    cluster, strata, metro, region, state, stateicp, ftotinc, respmode,
    metro, density, pctmetro, puma, met2013, city,
    # individual variables
    position, sex, related, yrimmig, bpld, bpldid, citizen, educ, qrelate, qsex, qcitizen, qeduc,
    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, speakeng, age, perwt, race, hispan
    # state_stock_year, state_stock_avg
  ) %>%
  pivot_wider(#id_cols = serial,
    names_from = position,
    values_from = c(sex, related, related, yrimmig, bpld, bpldid, citizen, educ,
                    occ, inctot, occscore, hwsei, empstat, yrnatur, poverty, 
                    speakeng, age, perwt, qrelate, qsex, qcitizen, qeduc, race, hispan
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
  filter(imm_couple != 'none', allocated == F) %>%
  write_csv(here('data', 'acs_wide.csv'))



## Make final datasets ####
acs_wide <- read.csv(here('data', 'acs_wide.csv')) 

# acs_wide <- acs_wide %>%
#   mutate(bpldid_main = case_when(bpld_main %in% c(41000, 41100, 41410) ~ 41300, # England, Scotland, Northern Ireland
#                      as_factor(bpld_main) == 'Korea' ~ 50200,
#                      T ~ bpldid_main),
#          bpldid_partner = case_when(bpld_partner %in% c(41000, 41100, 41410) ~ 41300, # England, Scotland, Northern Ireland
#                                  as_factor(bpld_partner) == 'Korea' ~ 50200,
#                                  T ~ bpldid_partner)) 






dist_dat <- read_dta(here('data', 'dist_cepii.dta')) %>%
  filter(iso_d == 'USA') %>%
  select(-iso_d) %>%
  mutate(iso_o = ifelse(iso_o == 'ROM', 'ROU', iso_o))

# Wage difference = USA - country of origin
penn_wages <- read.csv(here('data', 'penn_wages.csv')) %>%
  mutate(pc_income = rgdpe /pop / 1000) %>%
  select(iso_o = countrycode, country, year, pc_income, rgdpe, pop)
usa_wages <- filter(penn_wages, iso_o == 'USA')
penn_wages$wage_dif <- NA
for(country_loop in unique(penn_wages$country)){
  for(year in penn_wages$year[penn_wages$country == country_loop]){
    penn_wages$wage_dif[penn_wages$year == year & penn_wages$country == country_loop] <-
      penn_wages$pc_income[penn_wages$year == year & penn_wages$country == country_loop] -
      usa_wages$pc_income[usa_wages$year == year]
    # penn_wages$usa_pc_income[penn_wages$year == year & penn_wages$country == country_loop] <- 
    #   usa_wages$pc_income[usa_wages$year == year]
  }
}
# penn_wages <- penn_wages %>%
#   mutate(wage_dif = adjust_for_inflation(wage_dif, 2017, "US", to_date = 1999))

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
      wb_unemp$unemployment[wb_unemp$year == year & wb_unemp$country == country] -
      usa_unemp$unemployment[usa_unemp$year == year]
  }
}

vdem <- read_csv(here('data', 'V-Dem-CY-Core-v11.1.csv')) %>%
  select(country_name, country_text_id, COWcode, year, v2x_libdem) %>%
  mutate(iso_o1 = countrycode(COWcode, origin = 'cowc', destination = 'iso3c'),
         iso_o2 = countrycode(country_name, origin = 'country.name', destination = 'iso3c'),
         iso_o = ifelse(!is.na(iso_o1), iso_o1, iso_o2)) %>%
  select(iso_o, country = country_name, year, vdem = v2x_libdem) %>%
  filter(!is.na(iso_o))


# Policy variables
# For now use 2019 values for 2020-2022
lgbt_policy <- read.csv(here('data', 'lgb_origin_index.csv'))

lgbt_policy_2019 <- lgbt_policy %>%
  filter(year == 2019) 

lgbt_policy <- bind_rows(lgbt_policy,
                         lgbt_policy_2019 %>%
                           slice(rep(1:n(), each = 3)) %>%
                           mutate(year = c(rep(c(2020, 2021, 2022), times = nrow(lgbt_policy_2019))))
) %>% 
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
  expand(year = full_seq(1960:2022, 1)) %>%
  left_join(yearly_prop)
yearly_prop_list <- list()
for(country_loop in unique(yearly_prop$iso_o)){
  yearly_prop_loop <- with(filter(yearly_prop, iso_o == country_loop), 
                           zoo(prop, 1960:2022)) %>%
    na_interpolation(option = "linear")
  yearly_prop_list[[country_loop]] <- bind_cols(iso_o = country_loop,
                                                year = index(yearly_prop_loop), 
                                                stock_prop = as.data.frame(yearly_prop_loop)[[1]])
}
yearly_prop <- bind_rows(yearly_prop_list) %>%
  left_join(select(lgbt_policy, iso_o, bpldid = Code) %>% distinct())


lgbt_policy_lag <- lgbt_policy %>%
  select(origin_score_lag = origin_score, year, Code) %>%
  mutate(year = year + 1)


state_policy <- read.csv(here('data', 'state_policy.csv')) 

state_policy[is.na(state_policy)] <- 0

state_policy <- state_policy %>%
  mutate(state_policy = -sodomy_illega + -marriage_ban + marriage_equality + civil_union + 
           employment_discrim_so + hate_crime_so + joint_adoption + -adoption_religious_freedom +
           conversion_therapy_ban + housing_discrim_so + state_ban_local_nondiscrimimation) %>%
  select(state = State, year = Year, state_policy, state_couple_laws)
state_policy_2020 <- state_policy %>%
  filter(year == 2020) 

state_policy <- bind_rows(state_policy,
                          state_policy_2020 %>%
                           slice(rep(1:n(), each = 2)) %>%
                           mutate(year = c(rep(c(2021, 2022), times = nrow(state_policy_2020))))
)


acs_geo <- read_csv(here('data', 'acs_geo_survey.csv'))


immigrants <- bind_rows(
  acs_wide %>% 
    filter(!is.na(yrimmig_main)) %>%
    mutate(citizen_cat_mate = ifelse(citizen_partner == 'N/A', 'U.S.-born', citizen_partner),
           citizen_mate = citizen_cat_mate != 'Not a citizen',
           college_mate = educ_partner == 'college',
           income_mate = inctot_partner) %>%
    select(!ends_with('_partner')) %>%
    rename_with(function(x) str_remove(x, '_main')),
  acs_wide %>% 
    filter(!is.na(yrimmig_partner)) %>%
    mutate(citizen_cat_mate = ifelse(citizen_main == 'N/A', 'U.S.-born', citizen_main),
           citizen_mate = citizen_cat_mate != 'Not a citizen',
           college_mate = educ_main == 'college',
           income_mate = inctot_main) %>%
    select(!ends_with('_main')) %>%
    rename_with(function(x) str_remove(x, '_partner'))
  ) %>% 
  filter(age >= 18 & age < 65) %>%
  mutate(yrimmig = ifelse(yrimmig < 1991, 1991, yrimmig)) %>%
  left_join(yearly_prop, by = c('yrimmig' = 'year', 'bpldid')) %>%
  left_join(select(lgbt_policy, -iso_o), by = c('yrimmig' = 'year', 'bpldid' = 'Code')) %>%
  left_join(acs_geo) %>%
  left_join(state_policy)


write_csv(immigrants, here('data', 'immigrants_geo.csv'))


