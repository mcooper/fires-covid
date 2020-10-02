library(tidyverse)
library(lubridate)
library(lme4)

setwd('~/fires-covid/data/')

options(stringsAsFactors=F)

dat <- read.csv('moddat.csv') %>%
  filter(date >= ymd('2020-08-15'),
         date < ymd('2020-09-14'))

death_pm25_re <- lmer(death_rate ~ pm25 + (1|countyFIPS) + (1|date), data = dat)
case_pm25_re <- lmer(case_rate ~ pm25 + (1|countyFIPS) + (1|date), data = dat)
death_aerosols_re <- lmer(death_rate ~ aerosols + (1|countyFIPS) + (1|date), data = dat)
case_aerosols_re <- lmer(case_rate ~ aerosols + (1|countyFIPS) + (1|date), data = dat)

summary(death_pm25_re)
summary(case_pm25_re)
summary(death_aerosols_re)
summary(case_aerosols_re)
