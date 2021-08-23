

# install necessary packages
library(rworldmap)
library(ggplot2)
library(dplyr)
require(maps)
theme_set(
  theme_void()
)


acs_couple_policy <- readRDS("~/OneDrive - Princeton University/Same-Sex Immigrant Couples/acs_couple_policy.rds") 
acs_couple_policy$bpld[acs_couple_policy$bpld == "United Kingdom, ns"] <- "England"

country_tab <- acs_couple_policy %>%
  group_by(bpld, same_sex) %>%
  count(wt = perwt) %>%
  pivot_wider(names_from = 'same_sex', values_from = 'n') %>%
  mutate(prop_samesex = round(100*`TRUE` / `FALSE`, 2)) 

country_tab <- rename(country_tab, Country = bpld)

country_tab$Country <- recode(country_tab$Country,
                              'Burma (Myanmar)' = 'Myanmar',
                              'England' = 'UK',
                              'Cambodia (Kampuchea)' = 'Cambodia',
                              'Egypt/United Arab Rep.' = 'Egypt', 
                              'Guyana/British Guiana' = 'Guyana',
                              'Korea' = 'South Korea',
                              'Republic of Georgia' = 'Georgia',
                              'South Africa (Union of)' = 'South Africa',
                              'Sri Lanka (Ceylon)' = 'Sri Lanka',
                              'Yemen Arab Republic (North)' = 'Yemen'
)

country_tab$region <- country_tab$Country


#### Creating the Map
# Adding immigrant data to the maps package data
sPDF <- joinCountryData2Map(country_tab,
                            joinCode = "NAME",
                            nameJoinColumn = "Country")

# Dropping Antarctica
new_world <- subset(sPDF, continent != "Antarctica")

mapDevice() #create world map shaped window

mapParams <- mapCountryData(new_world,
                            nameColumnToPlot="prop_samesex",
                            catMethod = "fixedWidth",
                            numCats = 100, 
                            colourPalette = "white2Black",
                            addLegend = FALSE,
                            mapTitle = "Percent (%) Same-Sex Couples")

do.call(addMapLegend,
        c(mapParams,
          legendLabels = "limits",
          legendIntervals = "page"))


## Now for U.S. States
acs_wide <- read.csv("~/OneDrive - Princeton University/Same-Sex Immigrant Couples/acs_wide.csv")

state_tab <- acs_wide %>%
  filter(state != 'District of Columbia') %>%
  group_by(state, same_sex) %>%
  count(wt = hhwt) %>%
  pivot_wider(names_from = 'same_sex', values_from = 'n') %>%
  mutate(prop_samesex = round(100*`TRUE` / `FALSE`, 2))

state_tab$region <- tolower(state_tab$state)

# Getting the map ready
us_states <- map_data("state")

state_imm <- left_join(us_states, state_tab)


ggplot(data = state_imm,
       mapping = aes(x = long, y = lat, group = group, fill = prop_samesex)) +
       geom_polygon(color = "gray90", size = 0.1) +
       coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
       labs(title = "Percent (%) Same-Sex Couples") + theme_map() + labs(fill = "Percent")+ scale_fill_gradient(low = "white", high = "black") +
       theme_map() + labs(fill = "Percent")
