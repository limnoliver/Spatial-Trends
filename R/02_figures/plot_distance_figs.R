library(dplyr)
# read in lake trends with IDs
lake.trends <- read.csv('cached_data/lake_trends.csv')

# read in air temp trends with IDs
driver.trends.wide <- readr::read_csv('cached_data/driver_trends_wide.csv')

# read in distances
distance <- read.csv('cached_data/distance_lake_glakes_ID.csv', stringsAsFactors = F)
distance <- select(distance, site_id, distance, nearest_lake, distance_km)
distance2 <- read.csv('cached_data/distance_lake_each_glake.csv', stringsAsFactors = F)

# merge lake and driver trends
lake.trends <- lake.trends %>%
  left_join(driver.trends.wide) %>%
  mutate(jas_laketrend_norm = mean_surf_jas_slope/jas_AirTemp_slope) %>%
  left_join(distance) %>%
  left_join(distance2)

# make distance plots

p <- ggplot(lake.trends, aes(x = distance_km, y = mean_surf_jas_slope)) +
  geom_point(alpha = 0.1, shape = 16, size = 2) +
  facet_wrap(~ nearest_lake, ncol = 2) +
  scale_color_manual(values = my.cols) +
  geom_smooth() +
  theme_bw() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16),
        strip.background = element_rect(fill = 'transparent', color = NA), 
        strip.text = element_text(size = 16), panel.grid.minor = element_blank()) +
  labs(x = "Distance to nearest Great Lake (km)", y = "JAS surface temp trend")

# create a data frame for adding text to plot
ann.text <- data.frame(distance_km = 180, jas_laketrend_norm = 0.9, 
                       nearest_lake = factor('Lake Michigan', levels=c('Lake Erie', 
                                                                       'Lake Superior', 
                                                                       'Lake Michigan', 
                                                                       'Lake Huron', 
                                                                       'Lake Ontario')))

lake.trends$nearest_lake2 <- as.character(lake.trends$nearest_lake)
lake.trends$nearest_lake2[lake.trends$lakes_within_50km >1] <- '2+ Lakes'

# get rid of one Lake Erie point that has air temp trend < 0.1
lake.trends <- filter(lake.trends, jas_AirTemp_slope > 0.1)

p2 <- ggplot(lake.trends, aes(x = distance_km, y = jas_laketrend_norm)) +
  geom_point(shape = 16, size = 1.2, aes(color = factor(lakes_within_50km), alpha = factor(lakes_within_50km))) +
  scale_color_manual(values = c('black', 'black', my.cols[1], my.cols[1]), guide = F) +
  scale_alpha_manual(values = c(0.1, 0.1, 0.6, 0.6), guide = F) +
  facet_wrap(~ nearest_lake, ncol = 2, scales = 'free_y') +
  geom_text(data = ann.text, aes(label = "Lakes within 50 km\nof 2+ Great Lakes"), 
            color = my.cols[1], size = 5) +
  #scale_color_manual(values = my.cols) +
  geom_smooth(size = 2, alpha = 0.6) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 14), axis.title = element_text(size = 18),
        strip.background = element_rect(fill = NA, color = NA), 
        strip.text = element_text(size = 18)) +
  labs(x = "Distance to nearest Great Lake (km)", y = "JAS Surface : Air Temp Trend")

# create distance bins
lake.trends$distance_bins <- cut(lake.trends$distance_km, c(0, 50, 100, 150, 600))
#lake.trends$distance_bins[lake.trends$lakes_within_50km > 1] <- 'multiple_within_50km'
#lake.trends$distance_bins <- as.factor(lake.trends$distance_bins)
#levels(all.trends$distance_bins) <- c('0-25 km', '25-50 km', '50-75 km', '75-100 km', '100-125 km', '125-150 km', '150-200 km', '>200 km')
levels(lake.trends$distance_bins) <- c('0-50 km', '50-100 km', '100-150 km', ">150 km")

source('R/02_figures/ggplot_smooth_func.R')
my.cols <- brewer.pal(5, 'Set1')
p3 <- ggplot(all.trends, aes(x = jas_AirTemp_slope, y = mean_surf_jas_slope)) +
  scale_color_manual(values = my.cols) +
  geom_abline(intercept = 0, slope = 1) +
  stat_smooth_func(geom = 'text', method = 'lm', parse = TRUE, ypos = 0.8, xpos = 0.45) +
  geom_smooth(method = 'lm', color = 'red') +
  facet_wrap(~distance_bins, ncol = 2) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  labs(x = "JAS Air Temp Trend", y = "JAS Lake Surface Trend", color = "Nearest Lake")

# create a boxplot of lake temps by distance
add.text <- data.frame(distance_bins = as.factor(levels(lake.trends$distance_bins)), 
                       mean_surf_jas_slope = rep(-0.05, 4), 
                       label = c('n = 2399', 'n = 3303', ' n = 2354', 'n = 2718'))
p4 <- ggplot(lake.trends, aes(x = distance_bins, y = mean_surf_jas_slope)) +
  geom_boxplot() +
  theme_bw() +
  geom_text(data = add.text, aes(x = distance_bins, y = mean_surf_jas_slope, label = label), 
            col = 'red', size = 8) +
  labs(x  = "Distance (km) to Great Lakes", y = 'JAS Surface Temp Trend') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 22), 
        axis.text = element_text(size = 20)) +
  coord_cartesian(ylim = c(-0.1, 1))

p3
ggsave('figures/distance_vs_trends.png', p)
ggsave('figures/distance_vs_trends_normalized.png', p2)
ggsave('figures/air_vs_water_bydistance.png', p3)
ggsave('figures/surfacetemp_distance_boxplot.png', p4)

library(RColorBrewer)
my.cols <- brewer.pal(4, 'Set1')