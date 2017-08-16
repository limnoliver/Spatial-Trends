# load libraries
library(readr)
library(lakeattributes)
library(lubridate)
library(data.table)
library(dplyr)
# read in driver data

setwd("C:/Users/soliver/Documents/data/NLDAS")

# get file list
files <- list.files()
files.short <- files[1:100]
n <- length(files)
drop.cols <- c('Rain', 'Snow')
cutoffs <- c(1, seq(from = 1000, to = 10000, by = 1000), n)

slope = function(x,y){
  coefficients(lm(y~x))[[2]]*10
}

col.lakes = function(lengths, names){
  test <- lapply(lake.names, rep(lake.names, lengths))
}

for (i in 2:length(cutoffs)) {
  # read in subset of csvs
  temp.files <- files[cutoffs[i-1]:cutoffs[i]]
  lake.names <- gsub('\\.csv', '', temp.files)
  myfiles <- lapply(temp.files, readr::read_csv)
  df <- rbindlist(myfiles)
  
  # fill in lake ids
  lengths <- as.numeric(lapply(myfiles, nrow))
  df$site_id <- rep(lake.names, lengths)
  
  temp.dat <- df %>%
    select(-c(Rain, Snow)) %>%
    mutate(month = month(time)) %>%
    mutate(year = year(time))
  
  # edit some of these calculations - as some don't make sense (e.g., mins and maxes may not be useful, but maybe for air temp?)
  jas <- temp.dat %>% 
    filter(month %in% 7:9) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(ShortWave_min = slope(year, ShortWave_min), LongWave_min = slope(year, LongWave_min), AirTemp_min = slope(year, AirTemp_min), RelHum_min = slope(year, RelHum_min), WindSpeed_min = slope(year, WindSpeed_min),
              ShortWave_max = slope(year, ShortWave_max), LongWave_max = slope(year, LongWave_max), AirTemp_max = slope(year, AirTemp_max), RelHum_max = slope(year, RelHum_max), WindSpeed_max = slope(year, WindSpeed_max),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean), AirTemp_mean = slope(year, AirTemp_mean), RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(scale = 'jas')
  
  ond <- temp.dat %>% 
    filter(month %in% 10:12) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(ShortWave_min = slope(year, ShortWave_min), LongWave_min = slope(year, LongWave_min), AirTemp_min = slope(year, AirTemp_min), RelHum_min = slope(year, RelHum_min), WindSpeed_min = slope(year, WindSpeed_min),
              ShortWave_max = slope(year, ShortWave_max), LongWave_max = slope(year, LongWave_max), AirTemp_max = slope(year, AirTemp_max), RelHum_max = slope(year, RelHum_max), WindSpeed_max = slope(year, WindSpeed_max),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean), AirTemp_mean = slope(year, AirTemp_mean), RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(scale = 'ond')
  
  jfm <- temp.dat %>% 
    filter(month %in% 1:3) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(ShortWave_min = slope(year, ShortWave_min), LongWave_min = slope(year, LongWave_min), AirTemp_min = slope(year, AirTemp_min), RelHum_min = slope(year, RelHum_min), WindSpeed_min = slope(year, WindSpeed_min),
              ShortWave_max = slope(year, ShortWave_max), LongWave_max = slope(year, LongWave_max), AirTemp_max = slope(year, AirTemp_max), RelHum_max = slope(year, RelHum_max), WindSpeed_max = slope(year, WindSpeed_max),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean), AirTemp_mean = slope(year, AirTemp_mean), RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(scale = 'jfm')
  
  amj <- temp.dat %>% 
    filter(month %in% 4:6) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(ShortWave_min = slope(year, ShortWave_min), LongWave_min = slope(year, LongWave_min), AirTemp_min = slope(year, AirTemp_min), RelHum_min = slope(year, RelHum_min), WindSpeed_min = slope(year, WindSpeed_min),
              ShortWave_max = slope(year, ShortWave_max), LongWave_max = slope(year, LongWave_max), AirTemp_max = slope(year, AirTemp_max), RelHum_max = slope(year, RelHum_max), WindSpeed_max = slope(year, WindSpeed_max),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean), AirTemp_mean = slope(year, AirTemp_mean), RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(scale = 'amj')
  
  annual <- temp.dat %>% 
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(ShortWave_min = slope(year, ShortWave_min), LongWave_min = slope(year, LongWave_min), AirTemp_min = slope(year, AirTemp_min), RelHum_min = slope(year, RelHum_min), WindSpeed_min = slope(year, WindSpeed_min),
              ShortWave_max = slope(year, ShortWave_max), LongWave_max = slope(year, LongWave_max), AirTemp_max = slope(year, AirTemp_max), RelHum_max = slope(year, RelHum_max), WindSpeed_max = slope(year, WindSpeed_max),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean), AirTemp_mean = slope(year, AirTemp_mean), RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(scale = 'annual')
  
  trends[[i-1]] <- rbind(jas, ond, jfm, amj, annual)
  
}
myfiles <- lapply(files, readr::read_csv)
temp <- readr::read_csv(files[1])
# merge driver data

# set vars of interest

# calculate trends in driver data

# map trends in driver data


