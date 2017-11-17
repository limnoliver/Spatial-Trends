library(ggplot2)
library(mapproj)
library(ggthemes)
library(cowplot)
library(leaflet)
library(RColorBrewer)

# lake trend map
lakes.sp <- readOGR(dsn = "H:/Projects/spatial temp trends data", layer = "model_lakes") 
names(lakes.sp)[1] = 'id'


# fortify in prep for ggploting
lakes.sp.f <- fortify(lakes.sp, region = 'id')

# add attributes back to df
lakes.df <- merge(lakes.sp.f, lakes.sp, by = 'id', all.x = TRUE)


# merge with stats
# read in lake trends
lake.trends <- read.csv('cached_data/lake_trends.csv')
names(lake.trends)[1] = 'id'
map.dat <- merge(lakes.df, lake.trends, by = 'id', all.x = TRUE)

states <- map_data('state')
study.site <- subset(states, region %in% c('minnesota', 'michigan', 'wisconsin'))

fig.w = 10
fig.h = 6.25
res = 200

get.cols <- function(var){
  bins = pretty(quantile(var, probs = c(0.005, 0.995), na.rm = T), 100)
  key.bins = pretty(quantile(var, probs = c(0.0005,0.995), na.rm = T), 5, min.n = 4)
  pal = colorRampPalette(rev(brewer.pal(9, 'RdYlBu')))(length(bins))
  bin.cols = bins[findInterval(var, bins, all.inside = TRUE)]
  return(list(bin.cols, key.bins, pal))
}

plot.trends.cont <- function(map.dat, var, var.legend, season, col.bins) {
  ggplot() +
    geom_polygon(data = study.site, aes(long, lat, group = group),fill = 'gray', color = 'white') +
    geom_polygon(data = map.dat, aes_string('long', 'lat', group = 'group', fill = factor(col.bins[[1]]), color = factor(col.bins[[1]]))) +
    scale_fill_manual(values = col.bins[[3]], name = var.legend, breaks = col.bins[[2]], 
                      labels = col.bins[[2]]) + 
    #scale_fill_brewer(type = 'div', palette = 'Spectral', direction = -1, name = var.legend) +
    #scale_color_brewer(type = 'div', palette = 'Spectral', direction = -1, guide = F) +
    scale_color_manual(values = col.bins[[3]], guide = F) + 
    coord_map('albers', lat0 = 41, lat1 = 50) +
    #facet_wrap(~timescale, scales = 'fixed', nrow = 2, ncol = 2)
    theme_map() +
    theme(legend.position = c(0.5, 0.8), 
          legend.justification = c(0, 0),
          legend.direction = 'horizontal',
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    guides(fill = guide_legend(title.position = 'top', label.position = 'bottom', nrow = 1))
}

p <- plot.trends.cont(map.dat, 'mean_surf_jas_slope', 'JAS Surface Temp \nDecadal Trend', 'JAS', col.bins = get.cols(map.dat$mean_surf_jas_slope))
ggsave("figures/JAS_lake_surface_trend.png", p)
