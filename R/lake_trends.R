detach('package:plyr', unload = TRUE)
library(dplyr)

lake.dat <- readr::read_tsv('NLDAS_thermal_metrics.tsv')

slope = function(x,y){
  coefficients(lm(y~x))[[2]]*10
}

lake.trends <- lake.dat %>%
  select(c(year, site_id, mean_surf_jas)) %>%
  filter(year %in% 1980:2015) %>%
  group_by(site_id) %>%
  summarise(mean_surf_jas_slope = slope(year, mean_surf_jas))
setwd("~/Spatial-Trends")
write.csv(lake.trends, 'cached_data/lake_trends.csv', row.names = F)
