# script to calculate distance from great lakes
# for each lake in dataset

library(lakeattributes)
library(dplyr)
library(httr)
library(sf)
library(maps)
library(sbtools)
library(geosphere)
library(ggplot2)

# log into sb
authenticate_sb()
id <- "530f8a0ee4b0e7e46bd300dd"
id <- "52e6a0a0e4b012954a1a238a"
id_wfs <- item_get_wfs(id)

id_children <- item_list_children(id)
sb.files <- item_list_files(id)

# individual lake shape files - because I want to know which lake is nearest to which
# great lake. Downloaded from here: https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd

erie <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeErie/hydro_p_LakeErie.shp")
superior <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeSuperior/hydro_p_LakeSuperior.shp")
michigan <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeMichigan/hydro_p_LakeMichigan.shp")
huron <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeHuron/hydro_p_LakeHuron.shp")
ontario <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeOntario/hydro_p_LakeOntario.shp")

greatlakes <- rbind(erie, superior, michigan, huron, ontario)

# read in lake trends with IDs
lake.trends <- readr::read_csv('cached_data/lake_trends.csv')

# read in air temp trends with IDs
driver.trends.wide <- readr::read_csv('cached_data/driver_trends_wide.csv')

# merge lake and driver trends
all.trends <- lake.trends %>%
  left_join(driver.trends.wide) %>%
  mutate(jas_laketrend_norm = mean_surf_jas_slope/jas_AirTemp_slope)

# calculate distances between point and shapefiles, also indicate which shape file
# the point is closest to
distance <- geosphere::dist2Line(p = lake.trends[,c('long', 'lat')], line = greatlakes) # this takes a long time to run (~ 1 hour)
write.csv(distance, 'cached_data/distance_lake_glakes.csv', row.names = F)

lake.trends$distance <- distance[,1]
lake.trends$nearest_lake <- greatlakes$NAMEEN[distance[,4]]
lake.trends$distance_km <- lake.trends$distance/1000

library(RColorBrewer)
my.cols <- brewer.pal(4, 'Dark2')

p <- ggplot(lake.trends, aes(x = distance_km, y = mean_surf_jas_slope)) +
  geom_point(alpha = 0.1, shape = 16, size = 2) +
  facet_wrap(~ nearest_lake, ncol = 2) +
  scale_color_manual(values = my.cols) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Distance to nearest Great Lake (km)", y = "JAS surface temp trend")

p2 <- ggplot(all.trends, aes(x = distance_km, y = jas_laketrend_norm)) +
  geom_point(alpha = 0.1, shape = 16, size = 1) +
  facet_wrap(~ nearest_lake, ncol = 2, scales = 'free_y') +
  scale_color_manual(values = my.cols) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Distance to nearest Great Lake (km)", y = "JAS Surface:Air Temp Trend")

#all.trends$distance_bins <- cut(all.trends$distance_km, c(0, 25, 50, 75, 100, 125, 150, 200, 600))
all.trends$distance_bins <- cut(all.trends$distance_km, c(0, 50, 100, 150, 600))

#levels(all.trends$distance_bins) <- c('0-25 km', '25-50 km', '50-75 km', '75-100 km', '100-125 km', '125-150 km', '150-200 km', '>200 km')
levels(all.trends$distance_bins) <- c('0-50 km', '50-100 km', '100-150 km', ">200 km")

# get rid of one Lake Erie point that has air temp trend < 0.1
all.trends <- filter(all.trends, jas_AirTemp_slope > 0.1)
source('R/ggplot_smooth_func.R')
p3 <- ggplot(all.trends, aes(x = jas_AirTemp_slope, y = mean_surf_jas_slope)) +
  geom_point(alpha = 0.3, shape = 16, size = 1, aes(color = nearest_lake)) +
  scale_color_manual(values = my.cols) +
  geom_abline(intercept = 0, slope = 1) +
  stat_smooth_func(geom = 'text', method = 'lm', parse = TRUE, ypos = 0.8, xpos = 0.45) +
  geom_smooth(method = 'lm', color = 'red') +
  facet_wrap(~distance_bins, ncol = 2) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  labs(x = "JAS Air Temp Trend", y = "JAS Lake Surface Trend", color = "Nearest Lake")

p3
ggsave('figures/distance_vs_trends.png', p)
ggsave('figures/distance_vs_trends_normalized.png', p2)
ggsave('figures/air_vs_water_bydistance.png', p3)

# great lakes
test <- mape('lakes')
plot.names <- test$names[grep("Superior", test$names)]
test2 <- map('lakes', region = plot.names)
test <- map('lakes', region = 'Great Lakes')
# get great lakes shapefiles
getLakes <- function(){
  shapefile_loc <- "http://geo.glin.net/gis/shps/glin_gl_mainlakes.zip"
  
  destination = file.path(tempdir(),"glin_gl_mainlakes.zip")
  file <- GET(shapefile_loc, write_disk(destination, overwrite=T))
  filePath <- tempdir()
  unzip(destination, exdir = filePath)
  
  lakes <- st_read(filePath, layer = "gl_mainlakes")
  return(lakes)
}
greatlakes <- getLakes()

# get lat/long from lake IDs
latlon <- function(id, param) {
  out <- rep(NA, length(id))
  for (i in 1:length(id)){
    out[i] <- lakeattributes::get_latlon(id[i])[[param]]
  }
  return(out)
}

lat <- latlon(lake.trends$site_id, 'lat')
long <- latlon(lake.trends$site_id, 'lon')

lake.trends <- lake.trends %>%
  mutate(lat = lat, long = long)

