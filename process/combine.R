library(tidyverse)
library(imputeTS)
library(lubridate)

setwd('~/fires-covid/data/')

#######################
# Deaths & Cases
#########################

#################
# Get covid data

#https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
cases <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv') %>%
  filter(countyFIPS > 2,
         stateFIPS %in% c(6, 53, 41)) %>%
  gather(date, cases, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
  mutate(date = mdy(gsub('X', '', date)),
         FIPS = substr(100000 + countyFIPS, 2, 6)) %>%
  group_by(FIPS) %>%
  arrange(date) %>%
  mutate(cases = cases - lag(cases, n=1, default=0)) %>%
  select(-County.Name, -State, -stateFIPS, -countyFIPS) %>%
  unique

deaths <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv') %>%
  filter(countyFIPS > 2,
         stateFIPS %in% c(6, 53, 41)) %>%
  gather(date, deaths, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
  mutate(date = mdy(gsub('X', '', date)),
         FIPS = substr(100000 + countyFIPS, 2, 6)) %>%
  group_by(FIPS) %>%
  arrange(date) %>%
  mutate(deaths = deaths - lag(deaths, n=1, default=0)) %>%
  select(-County.Name, -State, -stateFIPS, -countyFIPS) %>%
  unique

pop <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv') %>%
  filter(State %in% c('CA', 'OR', 'WA')) %>%
  mutate(FIPS = substr(100000 + countyFIPS, 2, 6)) %>%
  select(FIPS, population)

#https://public.tableau.com/profile/oregon.health.authority.covid.19#!/vizhome/OregonCOVID-19HospitalCapacitySummaryTables_15965754787060/HospitalizationbyRegionSummaryTable
ca_hosp <- read.csv('~/fires-covid/data/CA_hospitalizations.csv')
ca_fips <- read.csv('~/fires-covid/data/CA_FIPS.csv') %>%
  mutate(FIPS = paste0("0", FIPS))
ca <- merge(ca_hosp, ca_fips, all.x=T, all.y=F) %>%
  mutate(date = ymd(substr(todays_date, 1, 10))) %>%
  select(date, FIPS, hospitalizations=hospitalized_covid_confirmed_patients)

covid_county <- Reduce(function(x, y) merge(x, y, all.x=T, all.y=T),
            list(pop, deaths, cases, ca)) %>%
  mutate(death_rate = deaths/population,
         case_rate = cases/population,
         hosp_rate = hospitalizations/population) %>%
  filter(date > ymd('2020-03-14'),
         substr(FIPS, 3, 5) != '000')

##########################
# Get Air Pollution Data
# Sentinel data from Google Earth Engine
ee <- read.csv('ee_res.csv', stringsAsFactors=F) %>%
  filter(id != 'id', date != '') %>%
  separate(id, c('source', 'FIPS'), sep='_') %>%
  rename(aerosols=mean) %>%
  mutate(aerosols = as.numeric(aerosols),
         date = ymd(date)) %>%
  filter(source == 'cnty', grepl('^06|^53|^41', FIPS)) %>%
  select(-source) %>%
  unique %>%
  complete(nesting(FIPS), date = seq(min(date), max(date), by='day')) %>%
  group_by(FIPS) %>%
  arrange(date) %>%
  mutate(aerosols = na_interpolation(aerosols))

#From AirNow, via Ben Sabath
st <- read.csv('daily_pm_2020.csv') %>%
  rename(date=day) %>%
  mutate(FIPS = substr(100000 + fips, 2, 6)) %>%
  filter(grepl('^06|^53|^41', FIPS)) %>%
  group_by(date, FIPS) %>%
  summarize(pm25 = median(pm25)) %>%
  group_by(FIPS) %>%
  arrange(date) %>%
  mutate(pm25 = na_interpolation(pm25))

pm25_county <- merge(ee, st, all=T) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(date)) %>%
  group_by(FIPS) %>%
  arrange(date)

########################
# Get Census data
#From IPUMS NHGIS American Community Survey 2018
cen <- read.csv('census_res.csv') %>%
  filter(FIPS_STATE %in% c(6, 41, 53)) %>%
  mutate(FIPS = paste0(substr(100 + FIPS_STATE, 2, 3), substr(1000 + FIPS_COUNTY, 2, 4))) %>%
  select(-X, -FIPS_STATE, -FIPS_COUNTY)

#########################
# Combine All
comb <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
               list(covid_county, pm25_county, cen)) %>%
  select(FIPS, date, aerosols, pm25, deaths, cases, death_rate, case_rate,
         matches('RACE'), INCOME_PERCAP, population, hospitalizations, hosp_rate)

########################################################
# Now do Oregon Hospitalizations at the Region Level
#########################################################
# 
# #https://public.tableau.com/profile/oregon.health.authority.covid.19#!/vizhome/OregonCOVID-19HospitalCapacitySummaryTables_15965754787060/HospitalizationbyRegionSummaryTable
# or_hosp <- read.table('~/fires-covid/data/Hospitalization by Region OR.csv', fileEncoding = 'UCS-2LE', header=T) %>%
#   mutate(date = paste0('2020-', substr(100 + match(Month, month.name), 2, 3), '-', substr(100 + Date, 2, 3))) %>%
#   select(-Month, -Date) %>%
#   gather(Region, hospitalizations, -date) %>%
#   mutate(Region = str_extract(Region, '\\d'),
#          date = ymd(date))
# or_fips <- read.csv('~/fires-covid/data/or_fips.csv') %>%
#   merge(pop) %>%
#   select(FIPS, Region, population)
# or_dat <- merge(pm25_county, or_fips) %>%
#   group_by(Region, date) %>%
#   summarize(aerosols=weighted.mean(aerosols, w=population, na.rm=T),
#             pm25=weighted.mean(pm25, w=population, na.rm=T),
#             population=sum(population))
# or <- merge(or_hosp, or_dat) %>%
#   mutate(hosp_rate = hospitalizations/population,
#          FIPS = paste0("41RE", Region)) %>%
#   select(-Region)
# 
#############################
# Write All
#all <- bind_rows(or, comb)
write.csv(comb, 'moddat.csv', row.names=F)
