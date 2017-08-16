# map spatial trends

library(maptools)
library(maps)
library(dplyr)
library(rgdal)

proj.string <- '+init=epsg:2860' 

map <- maps::map('state', fill=TRUE, plot = FALSE)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84")) %>% 
  spTransform(CRS(proj.string ))

study.sp <- map.sp[names(map.sp) %in% c('minnesota','wisconsin','michigan'), ]
other.sp <- map.sp[!names(map.sp) %in% c('minnesota','wisconsin','michigan'), ] 

setwd("~/data")
lakes.sp <- readOGR(dsn = "C:/Users/soliver/Documents/data", layer = "model_lakes") %>% 
  spTransform(CRS(proj.string))

# add calculated statistics to shape characteristics for plotting

plot(lakes.sp)

sp.df <- data.frame(site_id = as.character(lakes.sp$site_id), stringsAsFactors = FALSE) %>% 
  left_join(NLDAS.lake)

#point
point_sp <- SpatialPoints(data.frame(x=NLDAS.lake$lon,y=NLDAS.lake$lat), proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
  spTransform(CRS(proj.string ))