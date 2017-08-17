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
n <- length(files)
drop.cols <- c('Rain', 'Snow')
cutoffs <- c(1, seq(from = 1000, to = 10000, by = 1000), n)

slope = function(x,y){
  coefficients(lm(y~x))[[2]]*10
}

trends <- data.frame(site_id = NA, AirTemp_min = NA, AirTemp_max = NA, AirTemp_mean = NA, ShortWave_mean = NA, 
                     LongWave_mean = NA, RelHum_mean = NA, WindSpeed_mean = NA, timescale = NA)

for (i in 2:length(cutoffs)) {
  # read in subset of csvs
  temp.files <- files[cutoffs[i-1]:cutoffs[i]]
  lake.names <- gsub('\\.csv', '', temp.files)
  myfiles <- lapply(temp.files, readr::read_csv)
  df <- rbindlist(myfiles)
  
  # fill in lake ids
  lengths <- as.numeric(lapply(myfiles, nrow))
  df$site_id <- rep(lake.names, lengths)
  
  #clear up memory for processing
  remove(myfiles)
  
  # remove vars we are not interested in (rain, snow)
  # create new columns for month and year
  temp.dat <- df %>%
    select(-c(Rain, Snow)) %>%
    mutate(month = month(time)) %>%
    mutate(year = year(time))
  
  # keep only July, August, September, 
  # calculate means (+ min and max for temperature)
  # calculate slope of trend between means~year
  jas <- temp.dat %>% 
    filter(month %in% 7:9) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(AirTemp_min = slope(year, AirTemp_min), AirTemp_max = slope(year, AirTemp_max), AirTemp_mean = slope(year, AirTemp_mean),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean),  RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(timescale = 'jas')
  
  # same as above except for October, November, December
  ond <- temp.dat %>% 
    filter(month %in% 10:12) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(AirTemp_min = slope(year, AirTemp_min), AirTemp_max = slope(year, AirTemp_max), AirTemp_mean = slope(year, AirTemp_mean),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean),  RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(timescale = 'ond')
  
  # same as above except for January, February, March
  jfm <- temp.dat %>% 
    filter(month %in% 1:3) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(AirTemp_min = slope(year, AirTemp_min), AirTemp_max = slope(year, AirTemp_max), AirTemp_mean = slope(year, AirTemp_mean),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean),  RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(timescale = 'jfm')
  
  # same as above except for April, May, June
  amj <- temp.dat %>% 
    filter(month %in% 7:9) %>%
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(AirTemp_min = slope(year, AirTemp_min), AirTemp_max = slope(year, AirTemp_max), AirTemp_mean = slope(year, AirTemp_mean),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean),  RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(timescale = 'amj')
  
  # Same as above but for all year
  annual <- temp.dat %>% 
    group_by(site_id, year) %>%
    select(-c(time, month)) %>%
    summarise_all(funs(min = min, max = max, mean = mean)) %>%
    summarise(AirTemp_min = slope(year, AirTemp_min), AirTemp_max = slope(year, AirTemp_max), AirTemp_mean = slope(year, AirTemp_mean),
              ShortWave_mean = slope(year, ShortWave_mean), LongWave_mean = slope(year, LongWave_mean),  RelHum_mean = slope(year, RelHum_mean), WindSpeed_mean = slope(year, WindSpeed_mean)) %>%
    mutate(timescale = 'annual')
  
  #clear up space for processing 
  remove(df)
  
  # add to trends df
  trends <- rbind(trends, jas, ond, jfm, amj, annual)
  
}
trends <- trends[-1, ]
setwd("~/Spatial-Trends")
write.csv(trends, 'cached_data/driver_trends.csv', row.names = FALSE)




