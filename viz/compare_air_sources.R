library(tidyverse)
library(lubridate)
library(sf)

setwd('~/fires-covid/data')

##Compare observed values


ee <- read.csv('ee_res.csv', stringsAsFactors=F) %>%
  select(id, date, aerosols=mean) %>%
  mutate(aerosols = as.numeric(aerosols))
st <- read.csv('daily_pm_2020.csv') %>%
  mutate(id = paste0('sens_', full_aqs_id)) %>%
  select(id, date=day, pm25)

m <- merge(ee, st, all.x=F, all.y=F) %>%
  filter(ymd(date) >= ymd('2020-08-15'))

ggplot(m) + 
  geom_bin2d(aes(x=aerosols, y=pm25), bins=50)
ggsave('../res/pm_aerosols_corr.png')
ggplot(m) + 
  geom_bin2d(aes(x=log(aerosols), y=log(pm25)), bins=50)
ggsave('../res/pm_aerosols_corr_log.png')


##Compare Aug 10 to Sept 10 for stations vs satellite
cty <- read_sf('cb_2018_us_county_5m', 'cb_2018_us_county_5m')

ee <- read.csv('ee_res.csv', stringsAsFactors=F) %>%
  separate(id, c('source', 'GEOID'), sep='_') %>%
  rename(aerosols=mean, day=date) %>%
  mutate(aerosols = as.numeric(aerosols)) %>%
  filter(source == 'cnty',
         day %in% c('2020-08-10', '2020-09-10'))

st <- read.csv('daily_pm_2020.csv') %>%
  mutate(GEOID = substr(100000 + fips, 2, 6)) %>%
  group_by(day, GEOID) %>%
  summarize(pm25 = median(pm25)) %>%
  filter(day %in% c('2020-08-10', '2020-09-10'))

m <- merge(ee, st, all=T)

ctym <- merge(cty, m) %>%
  filter(!STATEFP %in% c('02', '15', '60', '66', '69', '72', '78')) %>%
  select(day, aerosols, pm25)

ggplot(ctym) + 
  geom_sf(aes(fill=aerosols), color=NA) + 
  facet_grid(. ~ day) + 
  theme_void() + 
  scale_fill_gradientn(colors=ihme_cols)
ggsave('../res/aerosols.pdf')
system('pdfcrop ../res/aerosols.pdf')

ggplot(ctym) + 
  geom_sf(aes(fill=pm25), color=NA) + 
  facet_grid(. ~ day) + 
  theme_void() + 
  scale_fill_gradientn(colors=ihme_cols)
ggsave('../res/pm25.pdf')
system('pdfcrop ../res/pm25.pdf')

