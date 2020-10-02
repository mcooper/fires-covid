library(tidyverse)

setwd('~/Desktop/nhgis0003_csv/')

#National Communicty Survey for 2018, downloaded from IPUMS NHGIS
dat <- read.csv('nhgis0003_ds239_20185_2018_tract_E.csv')

names(dat)[38:94] <- c("TOTAL_POPULATION", "RACE_TOT", "RACE_NHISP_TOTXX", "RACE_WHITE", 
                       "RACE_BLACK", "RACE_INDIAN", "RACE_ASIAN", "RACE_OTHER1", "RACE_OTHER2",
                       "RACE_OTHER3", "RACE_OTHER4", "RACE_OTHER5", "RACE_HISP", 
                       "RACE_HISPXX1", "RACE_HISPXX2", "RACE_HISPXX3", "RACE_HISPXX4", 
                       "RACE_HISPXX5", "RACE_HISPXX6", "RACE_HISPXX7", "RACE_HISPXX8", 
                       "RACE_HISPXX9", "TRAN_TOT", "TRAN_CAR1", "TRAN_CAR2", "TRAN_CAR3", 
                       "TRAN_CAR4", "TRAN_CAR5", "TRAN_CAR6", "TRAN_CAR7", "TRAN_CAR8", 
                       "TRAN_PUB1", "TRAN_PUB2", "TRAN_PUB3", "TRAN_PUB4", "TRAN_PUB5", 
                       "TRAN_PUB6", "TRAN_CAR9", "TRAN_EXP1", "TRAN_EXP2", "TRAN_EXP3", 
                       "TRAN_OTHER", "TRAN_WFH", "TIME_TOT", "TIME_LT5", "TIME_5_9",
                       "TIME_10_14", "TIME_15_19", "TIME_20_24", "TIME_25_29", "TIME_30_34", 
                       "TIME_35_39", "TIME_40_44", "TIME_45_59", "TIME_60_89", "TIME_GT90",
                       "INCOME_PERCAP")

res <- dat %>%
  group_by(STATEA, COUNTYA) %>%
  summarize_at(vars(matches('RACE'), 'TOTAL_POPULATION', 'INCOME_PERCAP'),sum) %>%
  mutate(RACE_WHITE = RACE_WHITE/RACE_TOT,
         RACE_BLACK = RACE_BLACK/RACE_TOT,
         RACE_NATAM = RACE_INDIAN/RACE_TOT,
         RACE_ASIAN = RACE_ASIAN/RACE_TOT,
         RACE_OTHER = (RACE_OTHER1 + RACE_OTHER2 + RACE_OTHER3 + RACE_OTHER4 + 
                       RACE_OTHER4 + RACE_OTHER5)/RACE_TOT,
         RACE_HISP = RACE_HISP/RACE_TOT,
         FIPS_COUNTY = substr(1000 + COUNTYA, 2, 5),
         FIPS_STATE = substr(100 + STATEA, 2, 3)) %>%
  ungroup %>%
  select(RACE_WHITE, RACE_BLACK, RACE_NATAM, RACE_OTHER, RACE_HISP, INCOME_PERCAP,
         FIPS_STATE, FIPS_COUNTY)

write.csv(res, '~/fires-covid/data/census_res.csv')
