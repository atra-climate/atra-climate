#########################################!#
####  Make ensemble models from individual binary projections
#########################################!#
#### | Project name: Atra climate modeling
#### | Script type: Data processing
#### | What it does: Description
#### | Date created: Mart 30, 2022.
#### | Creator: ---
#### | Contact: ---
#########################################!#

pacman::p_load(Rahat, tidyverse, tictoc, janitor, raster)

library(sf)

# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables
source(here::here("R", "main_setup.R"))
  

####

# Set folders -------------------------------------------------------------


#### This case is used when data is read locally on the laptop
folder_path_projections <- folder_path_projections %>% 
    str_replace_all(here(""), "Projects/atra_climate_model/") %>% 
    milkunize2("archive")

####

sum_vals <- function(x, outname)
{
  tic()
  bin_projections_vals <- getValues(x)
  bin_projections_valsums <- matrixStats::rowSums2(bin_projections_vals)
  bin_projections_valdiv <- bin_projections_valsums / nlayers(x)
  bin_projections_div <- setValues(x[[1]], bin_projections_valdiv)
  writeRaster(bin_projections_div, outname, options = "COMPRESS=LZW")
  toc()
}

#


mean_vals <- function(x, outname)
{
  tic()
  bin_projections_vals <- getValues(x)
  bin_projections_valmeans <- matrixStats::rowMeans2(bin_projections_vals)
  bin_projections_valmeans <- bin_projections_valmeans / 1000
  bin_projections_mn <- setValues(x[[1]], bin_projections_valmeans)
  writeRaster(bin_projections_mn, outname, options = "COMPRESS=LZW")
  toc()
}


# Create stacked binary ensembles ####
## Current climate projections ----------------------------------------------

# Create raster files for consensus models for mapping and calcs

for (my_species in c("points_all", "points_atra", "points_prenjensis"))
{
  bin_projections_stack <- str_glue("{folder_path_projections}/binary/") %>% 
    list.files(full.names = TRUE) %>% 
    str_subset(my_species) %>% 
    str_subset("current") %>% 
    str_subset("Ensemble", negate = TRUE) %>% 
    stack()
  
  
  bin_raster_outname <- str_glue("{folder_path_projections_manual_ensembles}ensemble_current_{my_species}.tif")
  ####
  if (!file.exists(bin_raster_outname))
  {
    sum_vals(bin_projections_stack, bin_raster_outname)
  }
}



# Future climate projections ----------------------------------------------

# Create raster files for consensus models for mapping and calcs

for (my_species in c("points_all", "points_atra", "points_prenjensis"))
{
  for (my_scenario in c("rcp26", "rcp85"))
  {
    
    bin_projections_stack <- str_glue("{folder_path_projections}/binary/") %>% 
      list.files(full.names = TRUE) %>% 
      str_subset(my_species) %>% 
      str_subset(my_scenario) %>% 
      str_subset("Ensemble", negate = TRUE) %>% 
      stack()
    
    bin_raster_outname <- str_glue("{folder_path_projections_manual_ensembles}ensemble_future_{my_species}_{my_scenario}.tif")
    # bin_raster_outname <- here("test", str_glue("consensus_noensemble_future_{my_species}_{my_scenario}.tif"))
    
    ####
    if (!file.exists(bin_raster_outname))
    {
      sum_vals(bin_projections_stack, bin_raster_outname)
    }
  }
}
###############!#


# Intersect changes with polygons
# Intesections ####

range_alps <- here("data", "mnt_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Alps") %>% 
  st_read() %>% 
  mutate(
    range = "Alps"
  )


range_dinarides <- here("data", "mnt_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinarides") %>% 
  st_read() %>% 
  mutate(
    range = "Dinarides"
  )


ranges_mountains <- rbind(
  range_dinarides,
  range_alps
)
####


# Load ensemble models ####

r_ensembles_stack <- folder_path_projections_manual_ensembles %>% 
  list.files(full.names = TRUE, pattern = "ensemble") %>%
  stack()


####
