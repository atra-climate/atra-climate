

pacman::p_load(Rahat, tidyverse, here, tictoc, janitor, sf, raster, matrixStats)

library(ggthemes)
library(viridis)
library(ggnewscale)
library(patchwork)
library(cowplot)
library(ggtext)

source(here::here("R", "main_setup.R"))

source(here("R", "functions", "bivariate_map_functions.R"))
####

utm_33n <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


#### Load species data
atra_raw_sf <- st_read(var_path_species_data)

atra_thinned_sf <- atra_raw_sf

atra_thinned_sf_utm <- atra_thinned_sf %>% 
  st_transform(crs = utm_33n)
#

dinaric_countries <- here::here("results", "procext_countries.gpkg") %>% 
  st_read() %>% 
  st_transform(crs = utm_33n)



## Process alpine ranges
range_alps <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("European_Alps") %>% 
  st_read() %>% 
  transmute(
    range = "Alps"
  )

range_dinarides <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinaric_") %>% 
  st_read() %>% 
  transmute(
    range = "Dinarides"
  )
#
alpine_ranges <- rbind(range_alps, range_dinarides)



###########################3
# Load commitee averages and continous projections

r_prediction <- folder_path_projections_manual_ensembles %>%
  list.files(full.names = TRUE, pattern = "ensemble") %>%
  head(1) %>% 
  raster() 


####
atra_thinned_sf_all <- atra_thinned_sf %>% 
  dplyr::select(
    subspecies
    ) %>% 
  filter(subspecies == "points_all") %>% 
  mutate(
    subspecies = 1
  )

atra_thinned_sf_atra <- atra_thinned_sf %>% 
  dplyr::select(
    subspecies
  ) %>% 
  filter(subspecies == "points_atra") %>% 
  mutate(
    subspecies = 1
  )

atra_thinned_sf_prenjensis <- atra_thinned_sf %>% 
  dplyr::select(
    subspecies
  ) %>% 
  filter(subspecies == "points_prenjensis") %>% 
  mutate(
    subspecies = 1
  )





r_points_all <- rasterize(atra_thinned_sf_all, r_prediction, field = "subspecies")
r_points_atra <- rasterize(atra_thinned_sf_atra, r_prediction, field = "subspecies")
r_points_prenjensis <- rasterize(atra_thinned_sf_prenjensis, r_prediction, field = "subspecies")

r_points_all[is.na(r_points_all)] <- 0

r_all_alps <- r_points_all %>% 
  crop(range_alps) %>% 
  mask(range_alps)

r_all_dinarides <- r_points_all %>% 
  crop(range_dinarides) %>% 
  mask(range_dinarides)

####

r_all_alps %>% 
  getValues() %>% 
  tabyl(show_na = F)

r_all_dinarides %>% 
  getValues() %>% 
  tabyl(show_na = F)

pts_extracted <- extract(r_points_prenjensis, alpine_ranges, df = TRUE)

pts_extracted %>% tabyl(layer)
