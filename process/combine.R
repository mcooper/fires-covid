library(tidyverse)
library(lubridate)

setwd('~/fires-covid/data/')

#######################
# Deaths & Cases
#########################

#################
# Get covid data
readUSAfacts <- function(x){
  d <- read.csv(x) %>%
    gather(date, value, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
    mutate(date = mdy(gsub('X', '', date)))
}

#https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
cases <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv') %>%
  gather(date, cases, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
  mutate(date = mdy(gsub('X', '', date)))
deaths <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv') %>%
  gather(date, deaths, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
  mutate(date = mdy(gsub('X', '', date)))
pop <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv')

m <- Reduce(function(x, y) merge(x, y, all.x=T, all.y=T),
            list(pop, deaths, cases))

m2 <- m %>%
  filter(countyFIPS > 2) %>%
  mutate(GEOID = substr(100000 + countyFIPS, 2, 6),
         death_rate = deaths/population,
         case_rate = cases/population) %>%
  filter(date > ymd('2020-03-14'),
         substr(GEOID, 3, 5) != '000')

##########################
# Get Air Pollution Data
ee <- read.csv('ee_res.csv', stringsAsFactors=F) %>%
  filter(id != 'id') %>%
  separate(id, c('source', 'GEOID'), sep='_') %>%
  rename(aerosols=mean, day=date) %>%
  mutate(aerosols = as.numeric(aerosols)) %>%
  filter(source == 'cnty')

st <- read.csv('daily_pm_2020.csv') %>%
  mutate(GEOID = substr(100000 + fips, 2, 6)) %>%
  group_by(day, GEOID) %>%
  summarize(pm25 = median(pm25))

m3 <- merge(ee, st, all=T) %>%
  rename(date=day) %>%
  mutate(date = ymd(date))

#########################
# Combine All
comb <- merge(m3, m2, all=T) %>%
  select(countyFIPS=GEOID, date, aerosols, pm25, State, deaths, cases, death_rate, case_rate)

write.csv(comb, 'moddat.csv', row.names=F)

#########################
# Now do hospitalizations
#########################

ca_hosp <- read.csv('~/fires-covid/data/CA_hospitalizations.csv')
ca_fips <- read.csv('~/fires-covid/data/CA_FIPS.csv') %>%
  mutate(FIPS = paste0("0", FIPS))
ca <- merge(ca_hosp, ca_fips, all.x=T, all.y=F) %>%
  mutate(date = ymd(substr(todays_date, 1, 10))) %>%
  select(county, date, FIPS, hospitalizations=hospitalized_covid_confirmed_patients) %>%
  merge(m3 %>%
        select(FIPS=GEOID, date, aerosols, pm25))

or_hosp <- read.table('~/fires-covid/data/Hospitalization by Region OR.csv', fileEncoding = 'UCS-2LE', header=T) %>%
  mutate(date = paste0('2020-', substr(100 + match(Month, month.name), 2, 3), '-', substr(100 + Date, 2, 3))) %>%
  select(-Month, -Date) %>%
  gather(Region, hospitalizations, -date) %>%
  mutate(Region = str_extract(Region, '\\d'),
         date = ymd(date))
or_fips <- read.csv('~/fires-covid/data/or_fips.csv') %>%
  merge(pop) %>%
  select(FIPS=countyFIPS, Region, pop=population)
or_dat <- merge(m3 %>%
                  rename(FIPS=GEOID), 
                or_fips) %>%
  group_by(Region, date) %>%
  summarize(aerosols=weighted.mean(aerosols, w=pop, na.rm=T),
            pm25=weighted.mean(pm25, w=pop, na.rm=T))

or <- merge(or_hosp, or_dat)

#############################
# Write hospitalizations
hosp <- bind_rows(or %>% rename(FIPS=Region),
                  ca %>% select(-county))
write.csv(hosp, 'hospdat.csv', row.names=F)
