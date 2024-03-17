##########################################
####  Create response variable
##########################################
#### | Project name: Atra climate
#### | Script type: Data processing
#### | What it does: Description
#### | Creator: ---
#### | Contact: ---
##########################################


pacman::p_load(Rahat, tidyverse, tictoc, janitor, blockCV, raster, biomod2, sf, tictoc)

# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))
  

pa_num = 1000

# Get data ----------------------------------------------------------------

# Adjust for modeling -----------------------------------------------------
atra_data_sf <- var_path_species_data %>% 
  st_read()

my_species <- unique(as.character(atra_data_sf$subspecies))

# Create pseudo-absences; do it only once
my_absences <- raster::sampleRandom(var_current_rasterstack[[1]], sp = TRUE,
                                    size = pa_num, na.rm = TRUE) %>% 
  st_as_sf()  %>% 
  mutate(
    PA = 0
  ) %>% 
  dplyr::select(-1)


# species_name = var_taxon_name
for (species_name in my_species)
{
  print(species_name)
  atra_data_sf_clean <- atra_data_sf %>% 
    filter(subspecies == species_name) %>% 
    transmute(
      PA = 1
    ) %>% 
    rename(
      geometry = geom
    )
  
  my_resp_var <- rbind(atra_data_sf_clean, my_absences) %>% 
    mutate(
      PA = as.factor(PA)
    )
  
  my_resp_var_sp <- as(my_resp_var, "Spatial")
  
  my_resp_var_sp_rarified <- rarify_points(my_resp_var_sp, var_current_rasterstack[[1]])
  
  
  # Split data into training and testing
  data_partition <- caret::createDataPartition(y = my_resp_var$PA, p = .75, list = FALSE)
  
  training_data <- my_resp_var[data_partition,]
  testing_data <- my_resp_var[-data_partition,]
  # 
  df <- c()
  df$train <- training_data
  df$test <- testing_data
  
  
  toc()
  
  my_resp_var_sp_rarified %>% 
    st_as_sf() %>% 
    st_write(str_glue("{folder_path_data_species}/response_data_{species_name}.gpkg"), append = FALSE)
  
}



#"#######################################################################################
#"#######################################################################################

tic("Preparing response variable")
response_variable_data <- prepare_model_data(response_vars = atra_data_sf_clean,
                                             explanatory_vars = climate_current_vif,
                                             pa_num = 1000)
toc()


####


atra_thinned_sf <- st_read(var_path_species_data)

# Adjust for modeling -----------------------------------------------------
atra_data_sf_clean <- var_path_species_data %>% 
  st_read() %>% 
  filter(subspecies == "points_all") %>% 
  transmute(
    PA = 1
  )

response_vars = atra_data_sf_clean
explanatory_vars = var_current_rasterstack
pa_num = 1000

# Function skeleton for prepare model data function
prepare_model_data <- function(response_vars, explanatory_vars,
                               pa_num = 1000)
{
  # Check input types
  assertthat::assert_that(
    inherits(explanatory_vars, "Raster"),
    inherits(response_vars, c("sf", "Spatial"))
  )
  
  # Extract values from raster stack
  response_var_extracted <- sf::st_as_sf(raster::extract(x = explanatory_vars, y = response_vars, 
                                                         method = "simple", sp = TRUE, na.rm = TRUE))
  
  
  # Create pseudo-absences
  my_absences <- raster::sampleRandom(explanatory_vars, sp = TRUE,
                                      size = pa_num, na.rm = TRUE) %>% 
    st_as_sf() %>% 
    # as.data.frame() %>% 
    mutate(
      PA = 0
    )
  # Finalize response variable (combine presences and absences)
  response_var_extracted_df <- response_var_extracted %>% 
    # st_set_geometry(NULL) %>% 
    mutate(PA = 1)
  
  my_resp_var <- rbind(response_var_extracted_df, my_absences) %>% 
    # filter(complete.cases(.)) %>% 
    mutate(
      PA = as.factor(PA)
    )
  
  plot(my_resp_var[1])
  tic("SB")
  set.seed(666)
  spatial_blocks <- spatialBlock(speciesData = my_resp_var,
                                 species = "PA",
                                 rasterLayer = var_current_rasterstack,
                                 theRange = distance_range_m, # size of the blocks
                                 k = 5,
                                 selection = "random",
                                 iteration = 100, # find evenly dispersed folds
                                 biomod2Format = T,
                                 xOffset = 0, # shift the blocks horizontally
                                 yOffset = 0)
  toc()
  
  folds_table <- spatial_blocks$biomodTable %>% 
    as.data.frame()
  
  folds_table_filename <- "Projects/atra_refugia/data/folds_table.csv" %>% 
    milkunize2()
  
  str_glue("{folder_path_data_misc}/atra_ecozones.gpkg")
  
  # Split data into training and testing
  data_partition <- caret::createDataPartition(y = my_resp_var$PA, p = .75, list = FALSE)
  
  training_data <- my_resp_var[data_partition,]
  testing_data <- my_resp_var[-data_partition,]
  # 
  df <- c()
  df$train <- training_data
  df$test <- testing_data
  # return(df)
  return(my_resp_var)
  
}