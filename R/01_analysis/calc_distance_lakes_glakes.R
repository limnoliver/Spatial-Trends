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
library(rgdal)

# individual lake shape files - because I want to know which lake is nearest to which
# great lake. Downloaded from here: https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd

erie <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeErie/hydro_p_LakeErie.shp")
superior <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeSuperior/hydro_p_LakeSuperior.shp")
michigan <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeMichigan/hydro_p_LakeMichigan.shp")
huron <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeHuron/hydro_p_LakeHuron.shp")
ontario <- readOGR("raw_data/GreatLakesandWa/hydro_p_LakeOntario/hydro_p_LakeOntario.shp")

greatlakes <- rbind(erie, superior, michigan, huron, ontario)

# read in lake trends with IDs
lake.trends <- read.csv('cached_data/lake_trends.csv')

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

# calculate distances between point and shapefiles, also indicate which shape file
# the point is closest to
distance <- geosphere::dist2Line(p = lake.trends[,c('long', 'lat')], line = greatlakes) # this takes a long time to run (~ 1 hour)

system.time(distance_erie <- geosphere::dist2Line(p = lake.trends[,c('long', 'lat')], line = erie))
system.time(distance_superior <- geosphere::dist2Line(p = lake.trends[,c('long', 'lat')], line = superior))
system.time(distance_michigan <- geosphere::dist2Line(p = lake.trends[,c('long', 'lat')], line = michigan))
system.time(distance_huron <- geosphere::dist2Line(p = lake.trends[,c('long', 'lat')], line = huron))

distance.all.lakes <- data.frame(site_id = lake.trends$site_id,
                                 d_erie = distance_erie[,1],
                                 d_superior = distance_superior[,1], 
                                 d_michigan = distance_michigan[,1],
                                 d_huron = distance_huron[,1])
write.csv(distance, 'cached_data/distance_lake_glakes.csv', row.names = F)

# find lakes that are within 50 km of two or more lakes
distance.all.lakes[,2:5] <- distance.all.lakes[,2:5]/1000
distance.all.lakes$lakes_within_50km <- c()

for (i in 1:nrow(distance.all.lakes)) {
  distance.all.lakes$lakes_within_50km[i] <-
    length(which(distance.all.lakes[i,2:5] <= 50))
    
}

write.csv(distance.all.lakes, 'cached_data/distance_lake_each_glake.csv', row.names = F)

lake.trends <- left_join(lake.trends, distance.all.lakes) %>%
  left_join(all.trends[,c('site_id', 'jas_laketrend_norm')])

write.csv(lake.trends, 'cached_data/distance_lake_glakes_ID.csv', row.names = F)



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



