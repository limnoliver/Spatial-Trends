# script to map driver trends
library(ggplot2)
library(mapproj)
library(ggthemes)
library(cowplot)
library(leaflet)

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

get.cols <- function(var){
bins = pretty(quantile(var, probs = c(0.01, 0.99), na.rm = T), 100)
key.bins = pretty(quantile(var, probs = c(0.01,0.99), na.rm = T), 5, min.n = 4)
pal = colorRampPalette(rev(brewer.pal(9, 'RdYlBu')))(length(bins))
bin.cols = bins[findInterval(var, bins, all.inside = TRUE)]
return(list(bin.cols, key.bins, pal))
}

plot.trends.cont <- function(map.dat, var, var.legend, season, col.bins) {
  ggplot() +
    geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'gray', color = 'white') +
    geom_polygon(data = map.dat, aes_string('long', 'lat', group = 'group', fill = factor(col.bins[[1]]), color = factor(col.bins[[1]]))) +
    scale_fill_manual(values = col.bins[[3]], name = var.legend, breaks = col.bins[[2]]) + 
    #scale_fill_brewer(type = 'div', palette = 'Spectral', direction = -1, name = var.legend) +
    #scale_color_brewer(type = 'div', palette = 'Spectral', direction = -1, guide = F) +
    scale_color_manual(values = col.bins[[3]], guide = F) + 
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

plot.trends.cont(annual.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'annual', col.bins = get.cols(annual.map$ShortWave_mean))

png('figures/shortwave_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends.cont(jfm.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'JFM', col.bins = get.cols(jfm.map$ShortWave_mean)),
plot.trends.cont(amj.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'AMJ', col.bins = get.cols(amj.map$ShortWave_mean)),
plot.trends.cont(jas.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'JAS', col.bins = get.cols(jas.map$ShortWave_mean)),
plot.trends.cont(ond.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'OND',  col.bins = get.cols(ond.map$ShortWave_mean)), nrow = 2)
dev.off()

png('figures/longwave_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends.cont(jfm.map, 'LongWave_mean', 'Longwave Decadal Trend', 'JFM', col.bins = get.cols(jfm.map$LongWave_mean)),
          plot.trends.cont(amj.map, 'LongWave_mean', 'Longwave Decadal Trend', 'AMJ', col.bins = get.cols(amj.map$LongWave_mean)),
          plot.trends.cont(jas.map, 'LongWave_mean', 'Longwave Decadal Trend', 'JAS', col.bins = get.cols(jas.map$LongWave_mean)),
          plot.trends.cont(ond.map, 'LongWave_mean', 'Longwave Decadal Trend', 'OND',  col.bins = get.cols(ond.map$LongWave_mean)), nrow = 2)
dev.off()


png('figures/relhumidity_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends.cont(jfm.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'JFM 1980-2015', col.bins = get.cols(jfm.map$RelHum_mean)),
          plot.trends.cont(amj.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'AMJ 1980-2015', col.bins = get.cols(amj.map$RelHum_mean)),
          plot.trends.cont(jas.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'JAS 1980-2015', col.bins = get.cols(jas.map$RelHum_mean)),
          plot.trends.cont(ond.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'OND 1980-2015',  col.bins = get.cols(ond.map$RelHum_mean)), nrow = 2)
dev.off()

png('figures/windspeed_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends.cont(jfm.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'JFM 1980-2015', col.bins = get.cols(jfm.map$WindSpeed_mean)),
          plot.trends.cont(amj.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'AMJ 1980-2015', col.bins = get.cols(amj.map$WindSpeed_mean)),
          plot.trends.cont(jas.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'JAS 1980-2015', col.bins = get.cols(jas.map$WindSpeed_mean)),
          plot.trends.cont(ond.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'OND 1980-2015',  col.bins = get.cols(ond.map$WindSpeed_mean)), nrow = 2)
dev.off()

png('figures/airtemp_byseason.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends.cont(jfm.map, 'AirTemp_mean', 'Air Temp Decadal Trend', 'JFM 1980-2015', col.bins = get.cols(jfm.map$AirTemp_mean)),
          plot.trends.cont(amj.map, 'AirTemp_mean', 'Air Temp Decadal Trend', 'AMJ 1980-2015', col.bins = get.cols(amj.map$AirTemp_mean)),
          plot.trends.cont(jas.map, 'AirTemp_mean', 'Air Temp Decadal Trend', 'JAS 1980-2015', col.bins = get.cols(jas.map$AirTemp_mean)),
          plot.trends.cont(ond.map, 'AirTemp_mean', 'Air Temp Decadal Trend', 'OND 1980-2015',  col.bins = get.cols(ond.map$AirTemp_mean)), nrow = 2)
dev.off()


png('figures/drivervars_annual.png',  res = res, height = fig.h, width = fig.w, units = 'in')
plot_grid(plot.trends.cont(annual.map, 'ShortWave_mean', 'Shortwave Decadal Trend', 'JFM 1980-2015', col.bins = get.cols(annual.map$ShortWave_mean)),
          plot.trends.cont(annual.map, 'LongWave_mean', 'Longwave Decadal Trend', 'AMJ 1980-2015', col.bins = get.cols(annual.map$LongWave_mean)),
          plot.trends.cont(annual.map, 'RelHum_mean', 'Rel Humidity Decadal Trend', 'JAS 1980-2015', col.bins = get.cols(annual.map$RelHum_mean)),
          plot.trends.cont(annual.map, 'WindSpeed_mean', 'Windspeed Decadal Trend', 'OND 1980-2015',  col.bins = get.cols(annual.map$WindSpeed_mean)), nrow = 2)
dev.off()

png('figures/airtemp_annual.png', res = res, height = fig.h, width = fig.w, units = 'in')
plot.trends.cont(annual.map, 'AirTemp_mean_bins', 'Air Temp Decadal Trend', 'Annual 1980-2015', col.bins = get.cols(annual.map$AirTemp_mean))
dev.off()





