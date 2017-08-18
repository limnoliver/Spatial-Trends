# script to map driver trends
library(ggplot2)
library(mapproj)
library(ggthemes)
library(cowplot)
source('R/maps.R')
trends <- readr::read_csv('cached_data/driver_trends.csv')
trends <- unique(trends)

timeperiods <- c('jas', 'ond', 'jfm', 'amj')
vars.clean <- c('Air Temperature (min of daily)', 'Air Temperature (max of daily)', 'Air Temperature (mean of daily)', 'Shortwave', 'Longwave', 'Relative Humidity', 'Windspeed')
vars.actual <- names(trends)[2:9]
var.position <- c(2:8)

# filter data for timeperiod and variable


jas <- trends %>%
  filter(timescale == 'jas')
ond <- trends %>%
  filter(timescale == 'ond')
jfm <- trends %>%
  filter(timescale == 'jfm')
amj <- trends %>%
  filter(timescale == 'amj')
annual <- trends %>%
  filter(timescale == 'annual')

# this is a bad way to map - should just add all features to attributes table
# but works for now, and won't speed up the actual plotting
jas.map <- map.prep(stats = jas)
ond.map <- map.prep(stats = ond)
jfm.map <- map.prep(stats = jfm)
amj.map <- map.prep(stats = amj)
annual.map <- map.prep(stats = annual)

states <- map_data('state')
study.site <- subset(states, region %in% c('minnesota', 'michigan', 'wisconsin'))

fig.w = 10
fig.h = 6.25
res = 200

plot.trends <- function(map.dat, var, var.legend, season, round.val = 0) {
  ggplot() +
    geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'white') +
    geom_polygon(data = map.dat, aes_string('long', 'lat', group = 'group', fill = factor(round(map.dat[,var], round.val)), color = factor(round(map.dat[,var], round.val)))) +
    scale_fill_brewer(type = 'div', palette = 'Spectral', direction = -1, name = var.legend) +
    scale_color_brewer(type = 'div', palette = 'Spectral', direction = -1, guide = F) +
    coord_map('albers', lat0 = 41, lat1 = 50) +
    #facet_wrap(~timescale, scales = 'fixed', nrow = 2, ncol = 2)
    theme_map() +
    theme(legend.position = c(0, 0), 
          legend.justification = c(0, 0),
          legend.direction = 'horizontal',
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5, label.position = 'bottom', nrow = 1)) +
    labs(title = season)
}

png('figures/shortwave_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends(jfm.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'JFM'),
plot.trends(amj.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'AMJ'),
plot.trends(jas.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'JAS'),
plot.trends(ond.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'OND'), nrow = 2)
dev.off()


bins <- seq(-0.4, 1.6, by = 0.2)
jfm.map$AirTemp_mean_bins <- bins[findInterval(jfm.map$AirTemp_mean, bins)]
amj.map$AirTemp_mean_bins <- bins[findInterval(amj.map$AirTemp_mean, bins)]
jas.map$AirTemp_mean_bins <- bins[findInterval(jas.map$AirTemp_mean, bins)]
ond.map$AirTemp_mean_bins <- bins[findInterval(ond.map$AirTemp_mean, bins)]

# make seasonal plots of all vars

png('figures/airtemp_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends(jfm.map, 'AirTemp_mean_bins', 'Air Temp Decadal Trend', 'JFM', 1),
          plot.trends(amj.map, 'AirTemp_mean_bins', 'Air Temp Decadal Trend', 'AMJ', 1),
          plot.trends(jas.map, 'AirTemp_mean_bins', 'Air Temp Decadal Trend', 'JAS', 1),
          plot.trends(ond.map, 'AirTemp_mean_bins', 'Air Temp Decadal Trend', 'OND', 1), nrow = 2)
dev.off()

png('figures/longwave_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends(jfm.map, 'LongWave_mean', 'Longwave Decadal Trend', 'JFM'),
          plot.trends(amj.map, 'LongWave_mean', 'Longwave Decadal Trend', 'AMJ'),
          plot.trends(jas.map, 'LongWave_mean', 'Longwave Decadal Trend', 'JAS'),
          plot.trends(ond.map, 'LongWave_mean', 'Longwave Decadal Trend', 'OND'), nrow = 2)
dev.off()

png('figures/relhumidity_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends(jfm.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'JFM'),
          plot.trends(amj.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'AMJ'),
          plot.trends(jas.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'JAS'),
          plot.trends(ond.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'OND'), nrow = 2)
dev.off()

png('figures/windspeed_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends(jfm.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'JFM', 1),
          plot.trends(amj.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'AMJ', 1),
          plot.trends(jas.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'JAS', 1),
          plot.trends(ond.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'OND', 1), nrow = 2)
dev.off()

png('figures/drivervars_annual.png', res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends(annual.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'annual', 0), 
          plot.trends(annual.map, 'LongWave_mean', 'Longwave Decadal Trends', 'annual', 0), 
          plot.trends(annual.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'annual', 0),
          plot.trends(annual.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'annual', 1), nrow = 2)
dev.off()





lakes.df$col <- as.factor(as.character())
png('figures/ShortWave_trends.png', res = res, height = fig.h, width = fig.w, units = 'in')
par(mfrow=c(2,2))
ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'white') +
  geom_polygon(data = jas.map, aes(long, lat, group = group, fill = factor(round(ShortWave_mean, 0)), color = factor(round(ShortWave_mean, 0)))) +
  scale_fill_brewer(type = 'div', palette = 'Spectral', direction = -1, name = 'Shortwave Decadal Trend') +
  scale_color_brewer(type = 'div', palette = 'Spectral', direction = -1, guide = F) +
  coord_map('albers', lat0 = 41, lat1 = 50) +
  #facet_wrap(~timescale, scales = 'fixed', nrow = 2, ncol = 2)
  theme_map() +
  theme(legend.position = c(1, 1), 
        legend.justification = c(1, 1),
        legend.direction = 'horizontal') +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5, label.position = 'bottom', nrow = 1))
dev.off()


############
# plot trends function




m2 <- m1 +
  ggplot(lakes.df) +
  aes(long, lat, group = group, fill = ShortWave_mean) +
  geom_polygon() +
  coord_map() +

########## non ggplot answer
study.states <- c('minnesota', 'michigan', 'wisconsin')

study.site <- map('state', regions = study.states)
plot(x = jas.map$long, y = jas.map$lat, col = jas.map$ShortWave_mean)
study.site <- subset(study.site, names %in% c('minnesota', 'michigan', 'wisconsin'))
