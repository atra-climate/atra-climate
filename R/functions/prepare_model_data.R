##########################################
####  Prepare model data function
##########################################
#### | Project name: Atra climate
#### | Script type: Function
#### | What it does: Prepares data for modeling
##########################################


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
  # response_var_extracted <- sf::st_as_sf(raster::extract(x = explanatory_vars, y = response_vars, 
  #                                                        method = "simple", sp = TRUE, na.rm = TRUE))
  
  
  # Create pseudo-absences; later on mopa functionality should be used for this
  set.seed(666)
  my_absences <- raster::sampleRandom(explanatory_vars, sp = TRUE,
                                      size = pa_num, na.rm = TRUE) %>% 
    st_as_sf() %>% 
    # as.data.frame() %>% 
    mutate(
      PA = 0
    ) %>% 
    dplyr::select(PA, geometry)
  # Finalize response variable (combine presences and absences)
  response_var_df <- response_vars %>% 
    # st_set_geometry(NULL) %>% 
    mutate(PA = 1) %>% 
    rename(geometry= geom)
  
  my_resp_var <- rbind(response_var_df, my_absences) %>% 
    # filter(complete.cases(.)) %>% 
    mutate(
      PA = as.factor(PA)
    )
  
  ####
  my_resp_var_sp <- as(my_resp_var, "Spatial")
  
  my_resp_var_rarified <- rarify_points(my_resp_var_sp, var_current_rasterstack[[1]]) %>% 
    st_as_sf() %>% 
    dplyr::select(PA, contains("geom"))
  # # Split data into training and testing
  # data_partition <- caret::createDataPartition(y = my_resp_var_rarified$PA, p = .75, list = FALSE)
  # # 
  # str(data_partition)
  # training_data <- my_resp_var_rarified[data_partition,]
  # testing_data <- my_resp_var_rarified[-data_partition,]
  # # # 
  # df <- list()
  # df$train <- training_data
  # df$test <- testing_data
  # return(df)
  return(my_resp_var_rarified)
  
}