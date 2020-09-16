library(tidyverse)
library(lubridate)

setwd('~/fires-covid/data/')

#################
# Get covid data
##################
readUSAfacts <- function(x){
  d <- read.csv(x) %>%
    gather(date, value, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
    mutate(date = mdy(gsub('X', '', date)))
}

cases <- read.csv('covid_confirmed_usafacts.csv') %>%
  gather(date, cases, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
  mutate(date = mdy(gsub('X', '', date)))
deaths <- read.csv('covid_deaths_usafacts.csv') %>%
  gather(date, deaths, -countyFIPS, -County.Name, -State, -stateFIPS) %>%
  mutate(date = mdy(gsub('X', '', date)))
pop <- read.csv('covid_county_population_usafacts.csv')

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
#########################

ee <- read.csv('ee_res.csv', stringsAsFactors=F) %>%
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
  mutate(date = ymd(date)) %>%
  filter(date > ymd('2020-03-14'))

#########################
# Combine All
##########################
comb <- merge(m3, m2, all=T) %>%
  filter(State %in% c("CA", "WA", "OR")) %>%
  select(countyFIPS=GEOID, date, aerosols, pm25, State, deaths, cases, death_rate, case_rate)

write.csv(comb, 'moddat.csv', row.names=F)








