# map spatial trends

library(maptools)
library(maps)
library(dplyr)
library(rgdal)
library(ggplot2)

map.prep <- function(stats) {
proj.string <- '+init=epsg:2860' 

# map <- maps::map('state', fill=TRUE, plot = FALSE)
# IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
# map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84")) %>% 
#   spTransform(CRS(proj.string ))
# 
# study.sp <- map.sp[names(map.sp) %in% c('minnesota','wisconsin','michigan'), ]
# other.sp <- map.sp[!names(map.sp) %in% c('minnesota','wisconsin','michigan'), ] 

setwd("~/data")
lakes.sp <- readOGR(dsn = "C:/Users/soliver/Documents/data", layer = "model_lakes") 
names(lakes.sp)[1] = 'id'


# fortify in prep for ggploting
lakes.sp.f <- fortify(lakes.sp, region = 'id')

# add attributes back to df
lakes.df <- merge(lakes.sp.f, lakes.sp, by = 'id', all.x = TRUE)

# merge with stats
col.id <- which(names(stats) %in% 'site_id')
names(stats)[1] = 'id'
lakes.df <- merge(lakes.df, stats, by = 'id', all.x = TRUE)

return(lakes.df)
}
#point
point_sp <- SpatialPoints(data.frame(x=NLDAS.lake$lon,y=NLDAS.lake$lat), proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
  spTransform(CRS(proj.string ))