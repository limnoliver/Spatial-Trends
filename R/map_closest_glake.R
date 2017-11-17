library(rgdal)
library(dplyr)
library(maptools)
library(maps)
library(ggplot2)
library(ggthemes)
# map which lakes are closest to which great lake
lakes.sp <- readOGR(dsn = "H:/Projects/spatial temp trends data", layer = "model_lakes") 
names(lakes.sp)[1] = 'id'

# get great lakes
greatlakes <- map_data('lakes', region = "Great Lakes")

# fortify in prep for ggploting
lakes.sp.f <- fortify(lakes.sp, region = 'id')

# merge lakes with distances df
distance <- read.csv('cached_data/distance_lake_glakes_ID.csv', stringsAsFactors = F)
names(distance)[1] <- 'id'
 
lakes.dat <- merge(lakes.sp.f, distance, by = 'id', all.x = TRUE)
head(lakes.dat)

states <- map_data('state')
study.site <- subset(states, region %in% c('minnesota', 'michigan', 'wisconsin'))
study.site.surround <- subset(states, region %in% c('iowa', 'illinois', 'indiana', 'ohio', 'pennsylvania', 'new york',
                                                    'north dakota', 'south dakota', 'nebraska'))
fig.w = 10
fig.h = 6.25
res = 200
library(RColorBrewer)
my.cols <- brewer.pal(4, 'Dark2')
lakes.dat$nearest_lake <- as.factor(lakes.dat$nearest_lake)
lakes.dat$nearest_lake <- factor(lakes.dat$nearest_lake, levels(lakes.dat$nearest_lake)[c(1,4,3,2)])

p <- ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'darkgray') +
  #geom_polygon(data = study.site.surround, aes(long, lat, group = group),fill = 'lightgray', color = 'white') + 
  geom_polygon(data = greatlakes, aes(long, lat, group = group), fill = 'lightblue', color = 'lightblue', size =1) +
  scale_color_manual(values = my.cols, guide = F) +
  scale_fill_manual(values = my.cols) +
  geom_polygon(data = lakes.dat, aes_string('long', 'lat', group = 'group', fill = 'nearest_lake', color = 'nearest_lake')) +
  coord_map('albers', lat0 = 41, lat1 = 50, xlim = c(-97.6, -81), ylim = c(41.5, 49.5)) +
  theme_map() +
  theme(legend.position = c(0.05, 0)) +
  guides(fill = guide_legend(title = 'Nearest Great Lake', title.position = 'top',  label.position = 'right', ncol = 2))

ggsave('figures/map_nearest_glake.png', p)
