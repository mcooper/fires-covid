library(tidyverse)
library(sf)
library(lubridate)

setwd('~/fires-covid/data/')

# Get covid data
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

#Get share of each state total since June that is unallocated
m3 <- m %>%
  filter(date >= ymd('2020-06-01')) %>%
  mutate(Unallocated = grepl('Unallocated', County.Name)) %>%
  group_by(State, Unallocated) %>%
  summarize(deaths = sum(deaths, na.rm=T),
            cases = sum(cases, na.rm=T)) %>%
  pivot_wider(names_from = c(Unallocated), values_from=c(deaths,cases)) %>%
  mutate(perc.death.unall = deaths_TRUE/deaths_FALSE,
         perc.cases.unall = cases_TRUE/cases_FALSE) %>%
  select(State, matches('perc')) %>%
  gather(var, val, -State) %>%
  mutate(var = gsub('perc.|.unall', '', var))


st <- read_sf('states_21basic', 'states') %>%
  select(State=STATE_ABBR) %>%
  filter(!State %in% c('AK', 'HI'))

comb <- merge(st, m3)

ggplot(comb)  + 
  geom_sf(aes(fill=val)) + 
  facet_grid(. ~ var) + 
  theme_void() + 
  scale_fill_gradientn(colors=ihme_cols) + 
  ggtitle("Perc of cases and deaths unassigned to state level") + 
  theme(plot.title = element_text(hjust = 0.5))

#Visalize deaths and cases before and after June
m2 <- m %>%
  filter(countyFIPS > 2) %>% #Filter unallocated counts
  mutate(time = ifelse(date >= ymd('2020-07-01'), 'Late Pandemic', 'Early Pandemic')) %>%
  filter(!is.na(time)) %>%
  group_by(time, countyFIPS) %>%
  summarize(death_rate = sum(deaths, na.rm=T)/unique(population),
         case_rate = sum(cases, na.rm=T)/unique(population)) %>%
  mutate(GEOID = substr(100000 + countyFIPS, 2, 6)) %>%
  gather(rate, value, -time, -countyFIPS, -GEOID) %>%
  mutate(rate = gsub('rate_', '', rate))

cty <- read_sf('cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  filter(!STATEFP %in% c('02', '15', '60', '66', '69', '72', '78'))

comb <- merge(cty, m2)

ggplot(comb)  + 
  geom_sf(aes(fill=value), color=NA) + 
  facet_grid(rate ~ time) + 
  theme_void() + 
  scale_fill_gradientn(colors=ihme_cols) + 
  ggtitle("Average Daily Rate of Cases and Deaths") + 
  theme(plot.title = element_text(hjust = 0.5))

