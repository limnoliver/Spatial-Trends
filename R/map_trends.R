# script to map driver trends
library(ggplot2)
library(mapproj)
library(ggthemes)
trends <- readr::read_csv('cached_data/driver_trends.csv')
trends <- unique(trends)
jas <- trends %>%
  filter(timescale == 'jas')

jas.map <- map.prep(stats = jas)
states <- map_data('state')
study.site <- subset(states, region %in% c('minnesota', 'michigan', 'wisconsin'))

fig.w = 10
fig.h = 6.25
res = 200

lakes.df$col <- as.factor(as.character())
png('figures/ShortWave_trends.png', res = res, height = fig.h, width = fig.w, units = 'in')
ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'white') +
  geom_polygon(data = lakes.df, aes(long, lat, group = group, fill = factor(round(ShortWave_mean, 0)), color = factor(round(ShortWave_mean, 0)))) +
  scale_fill_brewer(type = 'div', palette = 'Spectral', direction = -1, name = 'Shortwave Decadal Trend') +
  scale_color_brewer(type = 'div', palette = 'Spectral', direction = -1, guide = F) +
  coord_map('albers', lat0 = 41, lat1 = 50) +
  theme_map() +
  theme(legend.position = c(1, 1), 
        legend.justification = c(1, 1),
        legend.direction = 'horizontal') +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5, label.position = 'bottom', nrow = 1))
dev.off()

               
  coord_map()


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
