# read in thermal properties for 6 climte projections
library(readr)
library(dplyr)

setwd()
acc <- read_tsv('ACCESS_thermal_metrics.tsv')

# set years of interest
h.years <- 1982:2000
p1.years <- 2041:2059
p2.years <- 2081:2099


# group by lake and years to calculate median values
acc.meds.h <- acc %>%
  filter(year %in% h.years) %>%
  group_by(site_id) %>%
  summarise_all(function(x) median(x, na.rm = TRUE))

acc.meds.p1 <- acc %>%
  filter(year %in% p1.years) %>%
  group_by(site_id) %>%
  summarise_all(function(x) median(x, na.rm = TRUE))

acc.meds.p2 <- acc %>%
  filter(year %in% p2.years) %>%
  group_by(site_id) %>%
  summarise_all(function(x) median(x, na.rm = TRUE))

# find any lakes that aren't in all summaries

drop <- which(acc.meds.h$site_id %in% acc.meds.p2$site_id == FALSE)
acc.meds.h <- acc.meds.h[-drop, ]

# test that data grames are identical (lakeid order and columns)
identical(names(acc.meds.h), names(acc.meds.p1))
identical(names(acc.meds.h), names(acc.meds.p2))
identical(acc.meds.h$site_id, acc.meds.p1$site_id)
identical(acc.meds.h$site_id, acc.meds.p2$site_id)

# still working on subtracting the two data frames
acc.h1p1 <- left_join(acc.meds.h, acc.meds.p1, by = 'site_id')
acc.h1p1.diff <- acc.h1p1[,grepl("*\\.x$", names(acc.h1p1)) - acc.h1p1[,grepl("*\\.y$", names(acc.h1p1))]]
  
