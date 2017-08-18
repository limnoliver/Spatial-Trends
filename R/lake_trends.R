lake.dat <- readr::read_tsv('NLDAS_thermal_metrics.tsv')

slope = function(x,y){
  coefficients(lm(y~x))[[2]]*10
}

lake.trends <- lake.dat %>%
  select(c(year, site_id, mean_surf_jas)) %>%
  group_by(site_id) %>%
  summarise(mean_surf_jas_slope = slope(year, mean_surf_jas))