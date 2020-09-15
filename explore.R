library(tidyverse)
library(lubridate)
library(sf)

setwd('~/fires-covid')

d <- read.csv('data/daily_pm_2020.csv')

recent <- d %>%
  filter(day == '2020-09-10')

ggplot(recent) + 
  geom_point(aes(x=X, y=Y))

cali <- d %>%
  filter(state=='CA') %>%
  group_by(day) %>%
  summarize(pm25=mean(pm25))

ggplot(cali) + 
  geom_point(aes(x=day, y=pm25))

cases <- read.csv('data/statewide_cases_CA.csv')

#https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
deaths <- read.csv('data/covid_deaths_usafacts.csv')
cases <- read.csv('data/covid_confirmed_usafacts.csv')
pop <- read.csv('data/covid_county_population_usafacts.csv')

cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m')
