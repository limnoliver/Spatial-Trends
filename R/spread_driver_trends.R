# calculate trends in water temp
library(plyr)
library(dplyr)
library(tidyr)

setwd("~/Spatial-Trends")
driver.trends <- readr::read_csv('cached_data/driver_trends.csv')



airtemp.wide <- driver.trends %>%
  select(site_id, AirTemp_mean, timescale) %>%
  spread(timescale, AirTemp_mean) %>%
  rename(c('site_id' = 'site_id', 'amj' = 'amj_AirTemp_slope', 'annual' = 'annual_AirTemp_slope', 'jas' = 'jas_AirTemp_slope', 'jfm' = 'jfm_Airtemp_slope', 'ond' = 'ond_AirTemp_slope'))

ShortWave.wide <- driver.trends %>%
  select(site_id, ShortWave_mean, timescale) %>%
  spread(timescale, ShortWave_mean) %>%
  rename(c('site_id' = 'site_id', 'amj' = 'amj_ShortWave_slope', 'annual' = 'annual_ShortWave_slope', 'jas' = 'jas_ShortWave_slope', 'jfm' = 'jfm_ShortWave_slope', 'ond' = 'ond_ShortWave_slope'))

LongWave.wide <- driver.trends %>%
  select(site_id, LongWave_mean, timescale) %>%
  spread(timescale, LongWave_mean) %>%
  rename(c('site_id' = 'site_id', 'amj' = 'amj_LongWave_slope', 'annual' = 'annual_LongWave_slope', 'jas' = 'jas_LongWave_slope', 'jfm' = 'jfm_LongWave_slope', 'ond' = 'ond_LongWave_slope'))

RelHum.wide <- driver.trends %>%
  select(site_id, RelHum_mean, timescale) %>%
  spread(timescale, RelHum_mean) %>%
  rename(c('site_id' = 'site_id', 'amj' = 'amj_RelHum_slope', 'annual' = 'annual_RelHum_slope', 'jas' = 'jas_RelHum_slope', 'jfm' = 'jfm_RelHum_slope', 'ond' = 'ond_RelHum_slope'))

WindSpeed.wide <- driver.trends %>%
  select(site_id, WindSpeed_mean, timescale) %>%
  spread(timescale, WindSpeed_mean) %>%
  rename(c('site_id' = 'site_id', 'amj' = 'amj_WindSpeed_slope', 'annual' = 'annual_WindSpeed_slope', 'jas' = 'jas_WindSpeed_slope', 'jfm' = 'jfm_WindSpeed_slope', 'ond' = 'ond_WindSpeed_slope'))

driver.trends.wide <- bind_cols(airtemp.wide, ShortWave.wide[,-1], LongWave.wide[,-1], RelHum.wide[,-1], WindSpeed.wide[,-1])

write.csv(driver.trends.wide, 'cached_data/driver_trends_wide.csv', row.names = F)
