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

# great scatter plot of JAS RH and JAS surface trends

p <- ggplot(lake.trends, aes(x = jas_RelHum_slope, y = mean_surf_jas_slope)) +
  geom_point(aes(color = distance_km), alpha = 0.5, shape = 16, size = 2) +
  scale_color_gradient2(low = '#081d58', mid = '#41b6c4', high = '#ffffd9', midpoint = 114, 
                        name = "Distance (km) to \nGreat Lakes ") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.title = element_text(size = 18), axis.text = element_text(size = 14)) +
  labs(x = "Relative Humidity Trend (% per decade)", 
       y = "JAS Surface Temp Trend (deg C per decade)")

ggsave('figures/surf_temp_vs_RH.png', p)  
