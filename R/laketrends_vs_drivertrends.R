# lake temp trends vs driver trends
library(randomForest)
library(dplyr)
library(tidyr)
library(ggplot2)
lake.trends <- readr::read_csv('cached_data/lake_trends.csv')
driver.trends.wide <- readr::read_csv('cached_data/driver_trends_wide.csv')

drop <- grep('ond|amj|jfm', names(trends))
drop <- c(1, drop)

trends <- left_join(lake.trends, driver.trends.wide)
trends.rf <- trends %>%
  select(-drop)

trends.long <- trends.rf %>%
  gather(key = var)

ggplot(trends.long, aes(x = var, y = trends.long$value[trends.long$var == 'mean_surf_jas_slope'])) +
  geom_point() +
  facet_wrap(~ var, scales = 'free') +
  theme_bw()

plot(trends$mean_surf_jas_slope~trends$jas_LongWave_slope)

library(randomForest)

rf <- randomForest(mean_surf_jas_slope ~ ., data = trends.rf, importance = T)
trends.rf <- as.data.frame(trends.rf)
partialPlot(rf, trends.rf, annual_RelHum_slope)
