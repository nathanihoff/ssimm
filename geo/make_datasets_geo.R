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
acs <- read_dta('/Users/nathan/Data/ACS/acs_2008_2023.dta') %>%
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
    ftotinc = ifelse(ftotinc >= 9999998, NA, ftotinc*cpi99*1.829/1000),
    inctot = ifelse(inctot >= 9999998, NA, inctot*cpi99*1.829/1000),
    valueh = ifelse(valueh >= 9999998 | valueh == 0, NA, valueh*cpi99*1.829/1000),
    hwsei = ifelse(hwsei == 0000, NA, hwsei),
    occscore = ifelse(occscore == 00, NA, occscore),
    hhid = paste0(year, '_', serial),
    educ = case_when(
      educ %in% 0:5 ~ '< HS',
      educ == 6 ~ 'HS',
      educ %in% 7:9 ~ 'some col',
      educ %in% 10:11 ~ 'college'),
    bpldid = case_when(bpld %in% c(41000, 41100, 41410) ~ 41300, # England, Scotland, Northern Ireland
                       as_factor(bpld) == 'korea' ~ 50220,
                       # as_factor(bpld) == 'Hong Kong' ~ 50000,
                       T ~ bpldid)) %>%
  filter(year != 2020)


# PUMA variables

# set.seed(100)
# acs_geo <- acs %>%
#   # filter(year == 2019) %>%
#   # sample_n(1000) %>%
#   # as_survey_design(weights = perwt) %>%
#   group_by(state, puma, year) %>%
#   summarize(bachelors_puma = sum(perwt[educ == 'college' & age >= 25], na.rm = T)/sum(perwt, na.rm = T),
#             black_puma = sum(perwt[race == 2], na.rm = T)/sum(perwt, na.rm = T),
#             hispanic_puma = sum(perwt[hispan != 0], na.rm = T)/sum(perwt, na.rm = T),
#             inctot_puma = sum(perwt*inctot, na.rm = T)/sum(perwt, na.rm = T),
#             log_inctot_puma = log(inctot_puma),
#             owned_puma = sum(perwt[ownershp == 1], na.rm = T)/sum(perwt, na.rm = T),
#             age_puma = sum(perwt*age, na.rm = T)/sum(perwt, na.rm = T),
#             immigrant_puma = sum(perwt[citizen %in% c(2,3)], na.rm = T)/sum(perwt, na.rm = T),
#             poverty_100_puma = sum(perwt[poverty <= 100 & poverty != 0], na.rm = T)/sum(perwt, na.rm = T),
#             poverty_200_puma = sum(perwt[poverty <= 200 & poverty != 0], na.rm = T)/sum(perwt, na.rm = T),
#             hwsei_puma = sum(perwt*hwsei, na.rm = T)/sum(perwt, na.rm = T),
#             unemployed_puma = sum(perwt[empstat == 2 & age >= 25], na.rm = T)/sum(perwt, na.rm = T),
#             valueh_puma = sum(perwt*valueh, na.rm = T)/sum(perwt, na.rm = T),
#             rent_puma = sum(perwt*rent, na.rm = T)/sum(perwt, na.rm = T),
#             costelec_puma = sum(perwt*costelec, na.rm = T)/sum(perwt, na.rm = T),
#             density_puma = mean(density, na.rm = T)
#   )
#   
# write_csv(acs_geo, here('data', 'acs_geo.csv'))


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

# State LGB policy

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
) %>%
  mutate(state = tolower(state))

lgbt_nonprofits <- read_csv(here('data', 'lgbt nonprofits by zipcode.csv')) %>%
  filter(year >= 2008) %>%
  mutate(zip = str_pad(zip_final, 5, pad = '0')) %>%
  select(-zip_final)
lgbt_nonprofits <- lgbt_nonprofits %>%
  bind_rows(lgbt_nonprofits %>%
              filter(year == 2021) %>%
              mutate(year = 2022))

# from census.gov https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html
# 2017 data works well (only about 100 missing after merging)
zip_to_tract_2010 <- readxl::read_excel(here('geo/data', 'ZIP_TRACT_032010.xlsx')) %>%
  rename_with(tolower) %>%
  mutate(statefp = substr(tract, 1, 2),
         countyfp = substr(tract, 3, 5),
         tractce = substr(tract, 6, 11))

# zip_to_county_2010 <- readxl::read_excel(here('geo/data', 'ZIP_COUNTY_032010.xlsx')) %>%
#   rename_with(tolower) %>%
#   mutate(statefp = substr(tract, 1, 2),
#          countyfp = substr(tract, 3, 5),
#          tractce = substr(tract, 6, 11))

zip_to_tract <- readxl::read_excel(here('geo/data', 'ZIP_TRACT_032017.xlsx')) %>%
  rename_with(tolower) %>%
  mutate(statefp = substr(tract, 1, 2),
         countyfp = substr(tract, 3, 5),
         tractce = substr(tract, 6, 11))
# from OPDR https://www.huduser.gov/apps/public/uspscrosswalk/home
tract_to_puma <- read_csv(here('geo/data', '2010_Census_Tract_to_2010_PUMA.txt')) %>%
  rename_with(tolower) 

# https://staff.washington.edu/glynn/StateFIPSicsprAB.pdf
statefip <- readxl::read_excel(here('geo/data', 'StateFIPSicsprAB.xls')) %>%
  mutate(statefp = str_pad(FIPS, 2, pad = '0')) %>%
  select(state = NAME, statefp)

zip_to_puma <- left_join(zip_to_tract, tract_to_puma) %>%
  group_by(zip, statefp, puma5ce) %>%
  summarize(bus_ratio = sum(bus_ratio)) %>%
  ungroup()

lgbt_nonprofits_puma <- left_join(lgbt_nonprofits, zip_to_puma) %>%
  mutate(lgbt_np_weighted = lgbt_np*bus_ratio) %>%
  group_by(year, statefp, puma5ce) %>%
  summarize(lgbt_nonprofits = sum(lgbt_np_weighted)) %>%
  ungroup() %>%
  left_join(statefip) %>%
  mutate(state = tolower(state))


imm_nonprofits <- readxl::read_excel(here('geo/data', 'immigrant nonprofits.xlsx')) %>%
  filter(year >= 2008) %>%
  mutate(zip = str_pad(zipcode, 5, pad = '0')) %>%
  select(-zipcode) %>%
  left_join(zip_to_puma) %>%
  mutate(immigrant_np_weighted = immigrant_np*bus_ratio) %>%
  group_by(year, statefp, puma5ce) %>%
  summarize(immigrant_nonprofits = sum(immigrant_np_weighted)) %>%
  ungroup() %>%
  left_join(statefip) %>%
  mutate(state = tolower(state))

write_csv(lgbt_nonprofits_puma, here('geo/files', 'nonprofits_puma.csv'))
write_csv(imm_nonprofits, here('geo/files', 'imm_nonprofits.csv'))


acs_geo_survey_mean <- acs %>%
  # filter(year == 2008) %>%
  # sample_n(1000) %>%
  group_by(state, puma, year) %>%
  summarize(bachelors_puma = 100*weighted.mean(educ == 'college' & age >= 25, w = perwt, na.rm = T),
            black_puma = 100*weighted.mean(race == 2, w = perwt, na.rm = T),
            hispanic_puma = 100*weighted.mean(hispan != 0, w = perwt, na.rm = T),
            inctot_puma = weighted.mean(inctot, w = perwt, na.rm = T),
            log_inctot_puma = log(inctot_puma),
            owned_puma = 100*weighted.mean(ownershp == 1, w = perwt, na.rm = T),
            age_puma = weighted.mean(age, w = perwt, na.rm = T),
            immigrant_puma = 100*weighted.mean(citizen %in% c(2,3), w = perwt, na.rm = T),
            poverty_100_puma = 100*weighted.mean(poverty <= 100 & poverty != 0, w = perwt, na.rm = T),
            poverty_200_puma = 100*weighted.mean(poverty <= 200 & poverty != 0, w = perwt, na.rm = T),
            hwsei_puma = weighted.mean(hwsei, w = perwt, na.rm = T),
            unemployed_puma = 100*weighted.mean(empstat == 2 & age >= 25, w = perwt, na.rm = T),
            valueh_puma = weighted.mean(valueh, w = perwt, na.rm = T),
            rent_puma = weighted.mean(rent, w = perwt, na.rm = T),
            costelec_puma = weighted.mean(costelec, w = perwt, na.rm = T),
            density_puma = mean(density, na.rm = T)
            ) %>%
  mutate(state = as.character(as_factor(state))) %>%
  left_join(state_policy) %>%
  mutate(puma5ce = str_pad(puma, 5, pad = '0')) %>%
  left_join(select(lgbt_nonprofits_puma, -statefp)) %>%
  mutate(lgbt_nonprofits = ifelse(is.na(lgbt_nonprofits), 0, lgbt_nonprofits)) %>%
  left_join(select(imm_nonprofits, -statefp)) %>%
  mutate(immigrant_nonprofits = ifelse(is.na(immigrant_nonprofits), 0, immigrant_nonprofits))


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
  filter(year == 2019) %>%
  sample_n(1000) %>%
  group_by(state, puma, year) %>%
  summarize(bachelors_puma = weighted.var.se(100*(educ == 'college' & age >= 25), w = perwt, na.rm = T),
            black_puma = weighted.var.se(100*(race == 2), w = perwt, na.rm = T),
            hispanic_puma = weighted.var.se(100*(hispan != 0), w = perwt, na.rm = T),
            inctot_puma = weighted.var.se(inctot, w = perwt, na.rm = T),
            log_inctot_puma = sqrt(log(inctot_puma^2)),
            owned_puma = weighted.var.se(100*(ownershp == 1), w = perwt, na.rm = T),
            age_puma = weighted.var.se(age, w = perwt, na.rm = T),
            immigrant_puma = weighted.var.se(100*(citizen %in% c(2,3)), w = perwt, na.rm = T),
            poverty_100_puma = weighted.var.se(100*(poverty <= 100 & poverty != 0), w = perwt, na.rm = T),
            poverty_200_puma = weighted.var.se(100*(poverty <= 200 & poverty != 0), w = perwt, na.rm = T),
            hwsei_puma = weighted.var.se(hwsei, w = perwt, na.rm = T),
            unemployed_puma = weighted.var.se(100*(empstat == 2 & age >= 25), w = perwt, na.rm = T),
            valueh_puma = weighted.var.se(valueh, w = perwt, na.rm = T),
            rent_puma = weighted.var.se(rent, w = perwt, na.rm = T),
            costelec_puma = weighted.var.se(costelec, w = perwt, na.rm = T)
  ) %>%
  mutate(state = as.character(as_factor(state)))


acs_geo_survey <- acs_geo_survey_mean %>%
  # rename_with(~paste0(.x, '_mean'), !any_of(c('puma', 'year'))) %>%
  left_join(
    acs_geo_survey_se %>%
    rename_with(~paste0(.x, '_se'), !any_of(c('puma', 'year', 'state')))
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
  mutate(married = related_partner == 'spouse' | 
           qrelate_partner == 'same sex spouse changed to unmarried partner',
         allocated = (qrelate_main == 'allocated' | qrelate_partner == 'allocated' |
                        qsex_main == 'allocated' | qsex_partner == 'allocated'))


acs_wide %>%
  filter(imm_couple != 'none' | same_sex == T, allocated == F) %>%
  write_csv(here('data', 'acs_wide_geo.csv'))



## Make final datasets ####
acs_wide <- read.csv(here('data', 'acs_wide_geo.csv')) 

# acs_wide <- acs_wide %>%
#   mutate(bpldid_main = case_when(bpld_main %in% c(41000, 41100, 41410) ~ 41300, # England, Scotland, Northern Ireland
#                      as_factor(bpld_main) == 'Korea' ~ 50200,
#                      T ~ bpldid_main),
#          bpldid_partner = case_when(bpld_partner %in% c(41000, 41100, 41410) ~ 41300, # England, Scotland, Northern Ireland
#                                  as_factor(bpld_partner) == 'Korea' ~ 50200,
#                                  T ~ bpldid_partner)) 






# Final dataset
acs_geo <- read_csv(here('data', 'acs_geo_survey.csv'))


immigrants <- bind_rows(
  acs_wide %>% 
    filter(!is.na(yrimmig_main)) %>%
    select(!ends_with('_partner')) %>%
    rename_with(function(x) str_remove(x, '_main')),
  acs_wide %>% 
    filter(!is.na(yrimmig_partner)) %>%
    select(!ends_with('_main')) %>%
    rename_with(function(x) str_remove(x, '_partner')),
  acs_wide %>% 
    filter(same_sex == T & imm_couple == 'none') %>%
    select(!ends_with('_partner')) %>%
    rename_with(function(x) str_remove(x, '_main')),
  acs_wide %>% 
    filter(same_sex == T & imm_couple == 'none') %>%
    select(!ends_with('_main')) %>%
    rename_with(function(x) str_remove(x, '_partner'))
  ) %>% 
  filter(age >= 18 & age < 65) %>%
  mutate(yrimmig = ifelse(yrimmig < 1991, 1991, yrimmig)) %>%
  left_join(acs_geo) 
  


write_csv(immigrants, here('data', 'immigrants_geo.csv'))


