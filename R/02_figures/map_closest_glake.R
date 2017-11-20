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
distance_all <- read.csv('cached_data/distance_lake_each_glake.csv', stringsAsFactors = F)

distance <- left_join(distance, distance_all)
names(distance)[1] <- 'id'
distance$nearest_lake2 <- as.character(distance$nearest_lake)
distance$nearest_lake2[distance$lakes_within_50km >1] <- '2+ Lakes'
 
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
my.cols <- brewer.pal(5, 'Set1')
lakes.dat$nearest_lake2 <- as.factor(lakes.dat$nearest_lake2)
#lakes.dat$nearest_lake2 <- factor(lakes.dat$nearest_lake2, levels(lakes.dat$nearest_lake2)[c(1,4,3,2)])

p <- ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'darkgray') +
  #geom_polygon(data = study.site.surround, aes(long, lat, group = group),fill = 'lightgray', color = 'white') + 
  geom_polygon(data = greatlakes, aes(long, lat, group = group), fill = 'lightblue', color = 'lightblue', size =1) +
  scale_color_manual(values = my.cols, guide = F) +
  scale_fill_manual(values = my.cols) +
  geom_polygon(data = lakes.dat, aes_string('long', 'lat', group = 'group', fill = 'nearest_lake2', color = 'nearest_lake2')) +
  coord_map('albers', lat0 = 41, lat1 = 50, xlim = c(-97.6, -81), ylim = c(41.5, 49.5)) +
  theme_map() +
  theme(legend.position = c(0.05, 0)) +
  guides(fill = guide_legend(title = 'Nearest Great Lake', title.position = 'top',  label.position = 'right', ncol = 2))

# make a map of anomolous data (in MN?) that is ~300km from Lake Superior and has 
# elevated trends (see distance ~ normalized trend plot)
test <- filter(distance, nearest_lake == "Lake Superior") %>%
  filter(distance_km > 250) %>%
  filter(distance_km < 350) %>%
  filter(mean_surf_jas_slope >0.5)
names(lakes.dat)
distance$anomaly <- ifelse(distance$nearest_lake == "Lake Superior" & distance$distance_km >250  & distance$mean_surf_jas_slope > 0.25, 'yes', 'no')
lakes.dat <- merge(lakes.dat, distance[,c('id','anomaly')], all.x = TRUE)

p.anomaly <- ggplot() +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'darkgray') +
  #geom_polygon(data = study.site.surround, aes(long, lat, group = group),fill = 'lightgray', color = 'white') + 
  #geom_polygon(data = greatlakes, aes(long, lat, group = group), fill = 'lightblue', color = 'lightblue', size =1) +
  scale_color_manual(values = c('black', 'red'), guide = F) +
  scale_fill_manual(values = c('black', 'red')) +
  geom_polygon(data = lakes.dat, aes_string('long', 'lat', group = 'group', fill = 'anomaly', color = 'anomaly')) +
  coord_map('albers', lat0 = 41, lat1 = 50, xlim = c(-97.6, -81), ylim = c(41.5, 49.5)) +
  theme_map() +
  theme(legend.position = c(0.05, 0))
  

# create a small US map + great lakes with bounding box around study region
usa <- map_data("usa")
states <- mape_data("states")
p2 <- ggplot(data = usa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = NA, color = 'darkgray') +
  geom_polygon(data = greatlakes, aes(long, lat, group = group), fill = 'lightblue', color = 'lightblue', size =1) +
  geom_polygon(data = states, aes(long, lat, group = group),fill = NA, color = 'darkgray') +
  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'white') +  geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'darkgray', color = 'white') +
  geom_rect(aes(xmin = -97.6, xmax = -81, ymin = 41, ymax = 50), fill = NA, color = 'red', size = 2) +
  coord_map('albers', lat0 = 23, lat1 = 49, ylim = c(23, 49), xlim = c(-125, -65)) +
  theme_map() +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA))

ggsave('figures/map_nearest_glake.png', p)
ggsave('figures/study_site_map.png', p2, bg = 'transparent')
ggsave('figures/LakeSuperior_300km_anomalies.png', p.anomaly)
