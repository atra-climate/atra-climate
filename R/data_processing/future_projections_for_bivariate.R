##########################################
####  Make data for bivariate projections
##########################################
#### | Project name: Atra climate
#### | Script type: Data processing
#### | What it does: Description
#### | Date created: juni 20, 2021.
#### | Creator: ---
#### | Contact: ---
##########################################

pacman::p_load(Rahat, tidyverse, tictoc, janitor, raster)
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
###### Loop ########

# Future ------------------------------------------------------------------


for (my_species in c("points_all", "points_atra", "points_prenjensis"))
{
 for (my_scenario in c("rcp26", "rcp85"))
 {
   
   bin_projections_stack <- str_glue("{folder_path_projections}/binary/") %>% 
     list.files(full.names = TRUE) %>% 
     str_subset(my_species) %>% 
     str_subset(my_scenario) %>% 
     stack()
   
   bin_raster_outname <- str_glue("{folder_path_dataviz}/Future_binary_sums_{my_species}_{my_scenario}.tif")
   ####
   if (!file.exists(bin_raster_outname))
   {
     sum_vals(bin_projections_stack, bin_raster_outname)
   }
 }
}

####

####
for (my_species in c("points_all", "points_atra", "points_prenjensis"))
{
  for (my_scenario in c("rcp26", "rcp85"))
  {
    # RCP85
    cont_projections_stack <- str_glue("{folder_path_projections}/future/") %>% 
      list.files(full.names = TRUE) %>% 
      str_subset(my_species) %>%
      str_subset(my_scenario) %>% 
      stack()
    
    cont_r_outname <- str_glue("{folder_path_dataviz}/Future_cont_mean_{my_species}_{my_scenario}.tif")
    
    if (!file.exists(cont_r_outname))
    {
      tic()
      mean_vals(cont_projections_stack, cont_r_outname)
      toc()
    }
  }
}
##############


# Current -----------------------------------------------------------------


for (my_species in c("points_all", "points_atra", "points_prenjensis"))
{
    bin_projections_stack <- str_glue("{folder_path_projections}/binary/") %>% 
      list.files(full.names = TRUE) %>% 
      str_subset(my_species) %>% 
      str_subset("current") %>% 
      stack()
    
    bin_raster_outname <- str_glue("{folder_path_dataviz}/Current_binary_sums_{my_species}.tif")
    ####
    if (!file.exists(bin_raster_outname))
    {
      sum_vals(bin_projections_stack, bin_raster_outname)
    }
}



####
for (my_species in c("points_all", "points_atra", "points_prenjensis"))
{
  # Current period
  cont_projections_stack <- str_glue("{folder_path_projections}/current/") %>% 
    list.files(full.names = TRUE) %>% 
    str_subset(my_species) %>%
    str_subset("current") %>% 
    stack()
  
  cont_r_outname <- str_glue("{folder_path_dataviz}/Current_cont_mean_{my_species}.tif")
  
  if (!file.exists(cont_r_outname))
  {
    tic()
    mean_vals(cont_projections_stack, cont_r_outname)
    toc()
  }
}

##############
