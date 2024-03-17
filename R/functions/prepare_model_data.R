##########################################
####  Prepare model data function
##########################################
#### | Project name: Atra climate
#### | Script type: Function
#### | What it does: Prepares data for modeling
##########################################


#' Prepare model data
#'
#' @param response_vars Spatial Points.
#' @param explanatory_vars Raster.
#' @param pa_num Numeric. Number of pseudoabsences to create. Default is 1000.
#'
#' @return
#' @export
#'
#' @examples
prepare_model_data <- function(response_vars, explanatory_vars,
                               pa_num = 1000)
{
  # Check input types
  assertthat::assert_that(
    inherits(explanatory_vars, "Raster"),
    inherits(response_vars, c("sf", "Spatial"))
  )
  
  # Create pseudo-absences; later on mopa functionality should be used for this
  set.seed(666)
  my_absences <- raster::sampleRandom(explanatory_vars, sp = TRUE,
                                      size = pa_num, na.rm = TRUE) %>% 
    st_as_sf() %>% 
    mutate(
      PA = 0
    ) %>% 
    dplyr::select(PA, geometry)
  
  # Finalize response variable (combine presences and absences)
  response_var_df <- response_vars %>% 
    mutate(PA = 1) %>% 
    rename(geometry= geom)
  
  my_resp_var <- rbind(response_var_df, my_absences) %>% 
    mutate(
      PA = as.factor(PA)
    )
  
  ####
  my_resp_var_sp <- as(my_resp_var, "Spatial")
  
  my_resp_var_rarified <- rarify_points(my_resp_var_sp, var_current_rasterstack[[1]]) %>% 
    st_as_sf() %>% 
    dplyr::select(PA, contains("geom"))

  return(my_resp_var_rarified)
  
}