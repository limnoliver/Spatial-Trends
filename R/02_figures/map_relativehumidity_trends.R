library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(ggthemes)


# map the RH yearly trends
rh.raster <- raster('cached_data/rh_trends.tif')
rh.spdf <- as(rh.raster, "SpatialPixelsDataFrame")
rh.df <- as.data.frame(rh.spdf)

head(rh.raster)
plot(rh.raster)

# ggplot the trends

p <- ggplot() +
  geom_tile(data = rh.df, aes(x = x, y = y, fill = rh_trends)) +
  scale_fill_gradient2(low = '#313695', mid = '#ffffbf', high = '#a50026', name = 'Relative Humidity \nTrend (% per year)') +
  geom_rect(aes(xmin = -97.6, xmax = -81, ymin = 41, ymax = 50), fill = NA, color = 'red', size = 2) +
  theme_map() +
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA, color = NA), 
        legend.direction = 'horizontal', legend.position = c(0.72, 0.19)) +
  guides(fill = guide_colorbar(barwidth = 12, barheight = 2, title.position = 'top')) +
  #coord_quickmap(ylim = c(23, 49), xlim = c(-125, -65))
  coord_map('albers', lat0 = 23, lat1 = 49, ylim = c(23, 49), xlim = c(-125, -65)) 
  
ggsave('figures/RH_trend_US_map.png', p)

