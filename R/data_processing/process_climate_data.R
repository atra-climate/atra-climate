##########################################
####  Get climate predictors
##########################################
#### | Project name: Atra climate
#### | Script type: Data processing
#### | What it does: Description
#### | Creator: ---
#### | Contact: ---
##########################################

# Script setup -----------------------------------------------------------

pacman::p_load(Rahat, raster, tidyverse, sf, tictoc, climatedata)

# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.

source(here::here("R", "main_setup.R"))
  

# Download chelsa variables -----------------------------------------------


# Convert to Celsius for Kelvin. This is needed for past data
# Current data needs to be divided by 10
# From chelsa:
# This means you have to divide by 10 and add -273.15 to get degree Celsius.
# lyrid <- 19
# cls_K <- climate_lgm[[lyrid]]
# cls_K <- (cls_K / 10)# + -273.15 # For precipitation
# # cls_K <- (cls_K / 10) + -273.15
# 
# plot(stack(climate_current[[lyrid]], cls_K)) # For precipitation
# plot(stack(climate_current[[lyrid]] / 10, cls_K))
# plot(stack(climate_current[[lyrid]], climate_lgm[[lyrid]]))
# 
# # 1, 4, 5, 6, 8, 9, 10, 11 needs conversion from K to C
# # 2, 3, 7  is fine, unit is not C (dimensionless) (doesn't need anything)
# # 12, 13, 14, 15, 16, 17, 18, 19 is strange (precipitation), lgm needs to be divided by 10 to get millimeters


# Load data ---------------------------------------------------------------


# Define spatial extent to which to crop variables
dinarides_bbox <- str_glue("{folder_path_data_species}/atra_spatial_extent.gpkg") %>% 
  st_read()

outdir = folder_path_data_climate


indir <- "Projects/atra_model/Data/CHELSA_bioclim_2020" %>% 
  milkunize2("archive")


## Set output directory for input
# 
file_list_outfiles_old <- outdir %>% 
  list.files(full.names = TRUE, recursive = TRUE)

file_list_outfiles_old[1] %>% 
  raster() %>% 
  plot()

## 
for (i in seq_along(file_list_outfiles_old))
{
  
  my_file <- file_list_outfiles_old[i]
  
  
  my_file_df_vars <- my_file %>% 
    str_remove(str_c(outdir, "/")) %>% 
    as_tibble() %>% 
    separate(col = value, into = c("period", "filepath"), sep = "/")
  
  
  }
folder_path_data_raster_chelsaclim <- str_glue("{folder_path_data_raster}/climate_chelsa/")

folder_path_data_raster_chelsaclim <- str_glue("{folder_path_data_raster}/climate_chelsa/")
folder_path_data_raster_chelsaclim <- str_glue("{folder_path_data_raster}/climate_chelsa/")
folder_path_data_raster_chelsaclim <- str_glue("{folder_path_data_raster}/climate_chelsa/")


# Current ------------------------------------------------------------------
for (i in 1:19)
{
  
  layerf <- sprintf("%02d", i)
  
  print(str_glue("Processing {layerf}"))
  tic(str_glue("Processing {layerf}"))
  
  base_folder <- str_glue("{outdir}/current")
  # dir.create(base_folder,
  #            recursive = TRUE,
  #            showWarnings = FALSE)
  # 
  
  outfile <- str_glue("{base_folder}/CHELSA_bio_{layerf}.tif")
  
  if (!file.exists(outfile))
  {
    my_lyr <- str_glue("{indir}/current") %>%
      list.files(recursive = TRUE, full.names = TRUE, pattern = glue::glue("{layerf}.tif")) %>%
      raster()
    
    tic("Cropping")
    my_lyr_cropped <- crop(my_lyr, dinarides_bbox)
    toc()
    
    if (i %in% c(1, 4:6, 8:11))
    {
      cat("Converting to C", "\n")
      my_lyr_croppedK <- my_lyr_cropped / 10
      cat("Saving...", "\n")
      writeRaster(my_lyr_croppedK, outfile,
                  options = "COMPRESS=LZW")
    } else {
      cat("Saving...", "\n")
      writeRaster(my_lyr_cropped, outfile,
                  options = "COMPRESS=LZW")
      
    }
    
  }
  toc()
}


# Future ------------------------------------------------------------------


climate_future_files <- str_glue("{indir}/future") %>%
  list.files()


future_vars_df <- climate_future_files %>% 
  str_remove("CHELSA_bio_mon_") %>% 
  as.data.frame() %>% 
  dplyr::rename(value = 1) %>%
  separate(value, into = c("model", "scenario", "extra", "extra2", "var"), sep = "_", remove = FALSE) %>%
  mutate(
    var = str_remove(var, ".tif"),
    base_path = str_glue("{indir}/future"),
    full_path = str_c(base_path, "/CHELSA_bio_mon_", value)
  )

scenarios_list_future <- unique(future_vars_df$model)


for (ii in seq_along(scenarios_list_future))
{
  
  my_gcm <- scenarios_list_future[ii]  
  
  for (rcp_scenario in c("rcp26", "rcp85"))
  {
    
    scen_id <- str_glue("{rcp_scenario}_{my_gcm}")
    
    scenario_processing_future <- my_gcm
    
    print(str_glue("Running {scen_id}"))
    
    # num_files <- str_glue("{outfolder_path}/future/") %>% 
    #   list.files(pattern = str_glue("{scen_id}")) %>% 
    #   length()
    
    future_raster_scenario <- future_vars_df %>% 
      filter(model == scenario_processing_future) %>% 
      filter(scenario == rcp_scenario) %>% 
      pull(full_path) %>% 
      stack()
    
    names(future_raster_scenario) <- names(future_raster_scenario) %>% 
      # names(past_raster_scenario) %>% 
      str_split("_", simplify = TRUE) %>% 
      as.data.frame() %>% 
      transmute(
        varname = str_c("CHELSA_bio10_", V8, "_", V4, "_", V5)
      ) %>% 
      pull(varname)
    

    for (i in 1:19)
    {
      
      
      
      layerf <- sprintf("%02d", i)
      
      print(str_glue("Processing {layerf} {rcp_scenario}"))
      
      base_folder <- str_glue("{outdir}/future")
      

      outfile <- str_glue("{base_folder}/CHELSA_bio_{layerf}_{rcp_scenario}_{my_gcm}.tif")
      
      if (!file.exists(outfile))
      {
        
        
        scenario_lyr_name <- str_replace_all(my_gcm, "-", ".")
        my_stack <- subset(future_raster_scenario, str_glue("CHELSA_bio10_{i}_{scenario_lyr_name}_{rcp_scenario}"))
        # names(future_raster_scenario)[1]
        tic("Cropping")
        my_median <- crop(my_stack, dinarides_bbox)
        toc()
        
        # tic("Calculating median")
        # my_median <- calc(my_stack_cropped, fun = median)
        # toc()
        
        
        if (i %in% c(1, 4:6, 8:11))
        {
          
          cat("Converting to C", "\n")
          my_median[my_median == -32768] <- NA
          
          
          my_median_K <- my_median / 10 #) + -273.15
          
          
          
          cat("Saving...", "\n")
          writeRaster(my_median_K, outfile,
                      options = "COMPRESS=LZW")
        } else if (i %in% 12:19){
          
          cat("Converting to mm", "\n")
          my_median_mm <- my_median
          
          my_median_mm[my_median_mm == -32768] <- NA
          
          
          cat("Saving...", "\n")
          writeRaster(my_median_mm, outfile,
                      options = "COMPRESS=LZW")
          
        } else if (i %in% c(2, 3, 7)){
          cat("Saving...", "\n")
          my_median[my_median == -32768] <- NA
          
          writeRaster(my_median, outfile,
                      options = "COMPRESS=LZW")
          
        } else {
          print("Exception!")
        }
      }
    }
  }
}


# crop and process --------------------------------------------------------


#### Check climate data

#### Past predictor variables
past_climate_vars_df <- outdir %>% 
  list.files(recursive = TRUE) %>% 
  str_split(pattern = "/", simplify = TRUE) %>% 
  as.data.frame() %>% 
  dplyr::rename() %>% 
  filter(V1 == "past") %>% 
  separate(V2, into = c("type", "bio", "var", "model"), sep = "_", remove = FALSE) %>% 
  transmute(
    period = V1,
    model = model %>% str_remove(".tif"),
    var = var,
    full_path = str_glue("{outdir}/{period}/{V2}")
  ) 


model_ids <- past_climate_vars_df %>% 
  pull(model) %>% unique()

i <- 1

# there are 7
my_past_scenario <- model_ids[i]

my_past_rasterstack <- past_climate_vars_df %>% 
  filter(model == my_past_scenario) %>% 
  pull(full_path) %>% 
  raster::stack()

selected_vars <- var_variables_vif

climate_past_vif <- subset(my_past_rasterstack, selected_vars)



#### Future climate variables

future_climate_vars_df <- outdir %>% 
  list.files(recursive = TRUE) %>% 
  str_split(pattern = "/", simplify = TRUE) %>% 
  as.data.frame() %>% 
  dplyr::rename() %>%
  filter(V1 == "future") %>% 
  separate(V2, into = c("chelsa", "bio", "var", "rcp_scen", "model"), sep = "_", remove = FALSE) %>% 
  transmute(
    period = V1,
    model = model %>% str_remove(".tif"),
    rcp_scen,
    scenario_id = str_glue("{rcp_scen}_{model}"),
    var = sprintf("%02d", as.numeric(var)),
    full_path = str_glue("{outdir}/{period}/{V2}")
  )


scenario_ids <- future_climate_vars_df %>% 
  pull(scenario_id) %>% unique()

i <- 1

my_future_scenario <- scenario_ids[i]

my_future_rasterstack <- future_climate_vars_df %>% 
  filter(scenario_id == my_future_scenario) %>% 
  pull(full_path) %>% 
  raster::stack()

my_future_rasterstack[my_future_rasterstack == -32768] <- NA


selected_vars <- var_variables_vif

climate_future_vif <- subset(my_future_rasterstack, selected_vars)

my_fut_name <- str_replace_all(my_future_scenario, "-", ".")

names(climate_future_vif) <- names(climate_future_vif) %>% 
  str_remove(str_c("_", my_fut_name))

# climate_future_vif[[2]] + 273

# Define spatial extent to which to crop variables
spatial_extent <- "Projects/atra_model/Data/atra_spatial_extent.gpkg" %>% 
  Rahat::milkunize2("archive") %>% 
  st_read()

climate_future_vif_cropped <- crop(climate_future_vif, spatial_extent)


#### Current

current_climate_vars_df <- outdir %>% 
  list.files(recursive = TRUE) %>% 
  str_split(pattern = "/", simplify = TRUE) %>% 
  as.data.frame() %>% 
  dplyr::rename() %>%
  filter(V1 == "current") %>% 
  separate(V2, into = c("type", "bio", "var"), sep = "_", remove = FALSE) %>% 
  transmute(
    period = V1,
    var = var %>% str_remove(".tif"),
    full_path = str_glue("{outdir}/{period}/{V2}")
  )


my_current_rasterstack <- current_climate_vars_df %>% 
  pull(full_path) %>% 
  stack()


climate_current_vif <- subset(my_current_rasterstack, selected_vars)
# 

names(climate_past_vif_cropped) <- names(climate_past_vif_cropped) %>% 
  str_remove(str_glue("{my_past_scenario}_"))

names(climate_future_vif) <- names(climate_future_vif) %>% 
  str_remove("_rcp26_bcc.csm1.1")


