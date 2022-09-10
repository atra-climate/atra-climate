##########################################
####  Project biomod models future
##########################################
#### | Project name: Atra modeling
#### | Script type: Data processing
#### | What it does: Description
##########################################

# Script setup ------------------------------------------------------------

pacman::p_load(Rahat, tidyverse, readxl, sf, broom, tictoc, 
               rasterVis, data.table, caret, raster,
               biomod2, gdalR, usdm, here)


# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))
  

#### 
#### Set taxon name
# Available options: points_atra; points_prenjensis; points_all
# Now loaded via main_setup if missing as argument taxon
# var_taxon_name = "points_atra"


# Source variables
## Create wd
setwd(var_biomod_wd)
## Source function
source(here("R", "functions", "prepare_model_data.R"))

outdir <- "Projects/atra_model/Data/CHELSA_bioclim_2020_cleaned" %>% 
  milkunize2("archive")

## Get slurm index
# it comes from main_setup, passed via make, and bash script
if (length(my_args$slurm) > 0)
{
  i = as.numeric(my_args$slurm)
} else {
  i = 4
}
# Load data ---------------------------------------------------------------


# Load future climate data -------------------------------------------------------


#### Future climate variables

future_climate_vars_df <- folder_path_data_climate %>% 
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
    full_path = str_glue("{folder_path_data_climate}/{period}/{V2}")
  )


scenario_ids <- future_climate_vars_df %>% 
  pull(scenario_id) %>% 
  unique()

#### Load future climate


tic("Running entire thing for future")


my_future_scenario <- scenario_ids[i]  

projections_no <- folder_path_projections_future %>% 
  list.files(recursive = TRUE, pattern = my_future_scenario) %>% 
  length()


if (projections_no != 45)
{
  
  climate_future_vif <- future_climate_vars_df %>% 
    filter(scenario_id == my_future_scenario) %>% 
    slice(var_variables_vif) %>% 
    pull(full_path) %>% 
    raster::stack() 


  climate_future_vif <- climate_future_vif
  
  
  my_fut_name <- str_replace_all(my_future_scenario, "-", ".")
  
  names(climate_future_vif) <- names(climate_future_vif) %>% 
    str_remove(str_c("_", my_fut_name))
  
  
  ####
  
  fitted_models_name <- getwd() %>% 
    list.files(
      recursive = TRUE, 
      pattern = str_glue("{var_taxon_name}.models.out")
    )
  
  fitted_models_var <- load(fitted_models_name)
  fitted_models <- get(fitted_models_var)
  
  
  fitted_models_ensemble_name <- getwd() %>% 
    list.files(recursive = TRUE,
               pattern = str_glue("{var_taxon_name}ensemble.models.out")
               )
  
  fitted_models_ensemble_var <- load(fitted_models_ensemble_name)
  fitted_models_ensemble <-   get(fitted_models_ensemble_var)
  
  print(str_glue("Running {my_future_scenario}"))
  
  
  
  tic("Projecting future models")
  model_projection_future <- BIOMOD_Projection(modeling.output = fitted_models,
                                               new.env = climate_future_vif,
                                               compress = "xz",
                                               do.stack = FALSE,
                                               proj.name = str_glue("future_climate_{var_taxon_name}_{my_future_scenario}"),
                                               selected.models = "all",
                                               binary.meth = "TSS",
                                               build.clamping.mask = FALSE)
  toc()
  
  # Projecting future models: 147.748 sec elapsed
  
  prediction_future <- get_predictions(model_projection_future)

  print("Maximum memory used after predicting: ")
  pryr::mem_used()
  
  for (i_pred in 1:nlayers(prediction_future))
  {
    
    r_lyr <- prediction_future[[i_pred]]
    
    run_name <- names(r_lyr) %>% 
      str_remove("Salamandra.atra_AllData_") %>% 
      str_remove(str_c("model.", str_replace(var_taxon_name, "_", "."), "_"))
    
    
    projection_outname <- str_glue("{folder_path_projections_future}/Projection_future_{var_taxon_name}_{run_name}_{my_future_scenario}.tif")
    
    if (!file.exists(projection_outname))
    {
      writeRaster(r_lyr, projection_outname, options = "COMPRESS=LZW")  
    } else {
      print(str_glue("File for {run_name} exists"))
    }
    
  }
  
  
  
  # Ensemble models ---------------------------------------------------------
  
  
  ensemble_model_projection_future <- BIOMOD_EnsembleForecasting(projection.output = model_projection_future,
                                                                 EM.output = fitted_models_ensemble,
                                                                 total.consensus = TRUE,
                                                                 binary.meth = "TSS")
  
  ensemble_prediction_future <- get_predictions(ensemble_model_projection_future)
  
  for (i_pred in 1:nlayers(ensemble_prediction_future))
  {
    
    r_lyr <- ensemble_prediction_future[[i_pred]]
    
    run_name <- names(r_lyr) %>% 
      str_remove("Salamandra.atra_") %>% 
      str_remove("_AllData") %>% 
      str_remove("EMwmeanByTSS_mergedAlgo_")%>% 
      str_remove(str_c("model.", str_replace(var_taxon_name, "_", "."), "_"))
    
    
    projection_outname_ens <- str_glue("{folder_path_projections_future}/Projection_future_{var_taxon_name}_{run_name}_ensemble_{my_future_scenario}.tif")
    
    if (!file.exists(projection_outname_ens))
    {
      writeRaster(r_lyr, projection_outname_ens, options = "COMPRESS=LZW")  
    } else {
      print(str_glue("File for {run_name} exists"))
    }
    
  }
} else {
  print(str_glue("There are 45 files for projection {my_future_scenario}. Skipping..."))
}

toc()


pryr::mem_used()