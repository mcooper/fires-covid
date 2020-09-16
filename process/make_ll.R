library(tidyverse)
library(lubridate)
library(sf)

setwd('~/fires-covid')

# Read in all sensors
d <- read.csv('data/daily_pm_2020.csv') %>%
  select(full_aqs_id, X, Y) %>%
  unique %>%
  mutate(id = paste0('sens_', full_aqs_id)) %>%
  select(-full_aqs_id)

# Read in all counties, get lat-long
cty <- read_sf('data/cb_2018_us_county_5m', 'cb_2018_us_county_5m') %>%
  st_centroid %>%
  cbind(. , st_coordinates(.)) %>%
  mutate(id = paste0('cnty_', GEOID)) %>%
  st_drop_geometry %>%
  select(X, Y, id)

comb <- bind_rows(d, cty)

write.csv(comb, 'station_county_ll.csv', row.names=F)
