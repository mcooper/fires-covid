library(tidyverse)
library(lubridate)
library(imputeTS)
library(broom)

#APPROACHES:
  #Binary "Smoke Wave" vs Continuous PM2.5
  #Deaths, hospitalizations, and cases
  #Satellite vs Stations

dat <- read.csv('~/fires-covid/data/moddat.csv') %>%
  #Filter all to first day with a case
  group_by(FIPS) %>%
  arrange(date) %>%
  mutate(cum = cumsum(cases)) %>%
  ungroup %>%
  filter(cum > 5) %>%
  select(-cum) %>%
  mutate(aerosols_bin = aerosols > 5,
         pm25_bin = pm25 > 10)

vars <- expand.grid(list(out=c('hosp_rate', 'death_rate', 'case_rate'),
                         var=c('aerosols', 'pm25', 'aerosols_bin', 'pm25_bin'),
                         tim=c('recent', 'long.term'),
                         lag=0:14), stringsAsFactors=F)

vars$estimate <- NA
vars$std.error <- NA
vars$p.value <- NA
vars$aic <- NA

vars <- vars %>%
  arrange(out, var, tim)

for (i in 1:nrow(vars)){
  print(i)
  sel <- data.frame(dat)
  
  sel$outcome <- sel[ , vars$out[i]]
  sel$predictor <- sel[ , vars$var[i]]
  
  sel <- sel %>%
    group_by(FIPS) %>%
    arrange(date) %>%
    mutate(predictor = dplyr::lag(predictor, n=vars$lag[i], default=NA))
  
  if (vars$tim[i] == 'recent'){
    sel <- sel %>%
      filter(ymd(date) > ymd('2020-09-01'))
  } else{
    sel <- sel %>%
      filter(ymd(date) > ymd('2020-04-01'))
  }
  
  sel <- sel %>%
    select(outcome, predictor, FIPS, date) %>%
    na.omit()
  
  mod <- glm(outcome ~ predictor + FIPS + as.factor(date), data=sel)
  m <- tidy(mod)
  
  vars$estimate[i] <- m$estimate[grepl('predictor', m$term)]
  vars$std.error[i] <- m$std.error[grepl('predictor', m$term)]
  vars$p.value[i] <- m$p.value[grepl('predictor', m$term)]
  vars$aic[i] <- mod$aic
  vars$nrow[i] <- nrow(sel)
}

write.csv(vars, '~/fires-covid/data/modres.csv', row.names=F)

best <- vars %>%
  group_by(out, var, tim) %>%
  filter(aic == min(aic)) %>%
  arrange(out, var, tim)

ggplot(best %>% filter(grepl('bin', var))) + 
  geom_bar(aes(x=out, y=estimate, fill=out, alpha=p.value < 0.001), stat='identity', position='dodge') + 
  geom_text(aes(x=out, y=estimate, label=lag)) + 
  facet_grid(var ~ tim) + 
  scale_alpha_manual(values=c(0.2, 1)) + 
  theme_bw() + 
  labs(title='Effect of Aerosols and PM2.5 on Covid Outcomes, Modeled as Smoke Waves')
ggsave('~/fires-covid/viz/mod_smoke_wave.png')

ggplot(best %>% filter(!grepl('bin', var))) + 
  geom_bar(aes(x=out, y=estimate, fill=out, alpha=p.value < 0.001), stat='identity', position='dodge') + 
  geom_text(aes(x=out, y=estimate, label=lag)) + 
  facet_grid(var ~ tim) + 
  scale_alpha_manual(values=c(0.2, 1)) + 
  theme_bw() + 
  labs(title='Effect of Aerosols and PM2.5 on Covid Outcomes, Modeled Continuously')
ggsave('~/fires-covid/viz/mod_continuous.png')
