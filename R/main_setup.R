##########################################
####  Main setup
##########################################
#### | Project name: Atra climate model
#### | Script type: Setup
#### | What it does: Creates setup for the project
##########################################



## Create run name
# Attempt here is to pass variable with make for different runs
# If bash script is used to run this script, argument after --args is used to set 
# run name, otherwise use user input
# User input can be modified via makefile and bash scripts
# eg.   Rscript R/model/mytest2.R -varname test -run $period
# This passes the arguments here, so varname and run are columns in list my_args
# eg.   Rscript R/model/fit_biomod_model.R -varname test -run $period

my_args <- R.utils::commandArgs(trailingOnly = TRUE, asValues = TRUE)

if (length(my_args$run) > 0)
{
  var_run_name = my_args$run
} else {
  var_run_name = "climate_impact_VIF"
}

if (length(my_args$taxon) > 0)
{
  var_taxon_name = my_args$taxon
} else {
  var_taxon_name = "points_atra"
}



# Load packages -----------------------------------------------------------

suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(raster))
# todor::todor_project_addin()

source(here("R", "functions", "rarify_points.R"))
# Set variables -----------------------------------------------------------

var_test_run = FALSE

if (var_test_run)
{
  warning(str_glue("Variable test run is set to {var_test_run}"))
}


#### VIF 
# Define VIF treshold
vif_th <- 2

# VIF threshold will be passed to script below, to define non-redundant variables 
# in var_variables_vif
#### Re-run this script in case variables need to be updated
# here("R", "data_processing", "climate_data_VIF.R")

var_variables_vif = c()



var_path_maxent = here("maxent.jar")
var_model_algorithms = c("GLM", "GBM", "GAM",
                         "RF",
                         "ANN",
                         "MAXENT.Phillips", "CTA",
                         "MARS"
                         )

var_path_species_data = here("data", "species_data", "atra_points_combined.gpkg")

# Create folders ----------------------------------------------------------

# Data paths
folder_path_data <- here("data")
# For output logs
folder_path_logs <- here("logs")
# For bash scripts
folder_path_bash <- here("bash")
# For other stuff, eg. makefile files
folder_path_misc <- here("misc")
folder_path_helpers_make <- here("misc", "makefile_helpers")

#
folder_path_data_misc <- here("data", "misc")
folder_path_data_raster <- here("data", "raster")
folder_path_data_raw <- here("data", "raw")
folder_path_data_species <- here("data", "species_data")
#

# Climate data path

folder_path_data_climate <- here("data", "raster", "CHELSA_bioclim_2020_cleaned")
folder_path_runs <- here("runs")
  

folder_path_dataviz <- str_glue("{folder_path_runs}/{var_run_name}/dataviz")

folder_path_data_raster_quarters <- str_glue("{folder_path_data_raster}/biomod_quarters")

folder_path_data_raster <- here("data", "raster")


#
folder_path_code_explore <- here("R", "explore")
folder_path_code_functions <- here("R", "functions")
folder_path_code_data_processing <- here("R", "data_processing")
folder_path_code_model <- here("R", "model")

folder_path_data_niche_overlap <- here("R", "niche_overlap")
folder_path_results <- here("results")


#### Create folders
dir.create(folder_path_data, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_data_raster, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_data_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_data_misc, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_bash, recursive = TRUE, showWarnings = FALSE)

dir.create(folder_path_logs, recursive = TRUE, showWarnings = FALSE)

dir.create(folder_path_data_climate, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_data_niche_overlap, recursive = TRUE, showWarnings = FALSE)

dir.create(folder_path_misc, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_helpers_make, recursive = TRUE, showWarnings = FALSE)


dir.create(folder_path_data_species, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_runs, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_results, recursive = TRUE, showWarnings = FALSE)

dir.create(folder_path_dataviz, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_data_raster_quarters, recursive = TRUE, showWarnings = FALSE)


##
dir.create(folder_path_code_explore, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_code_functions, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_code_data_processing, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_code_model, recursive = TRUE, showWarnings = FALSE)


#### Set model folder paths ####



## Set folder where output is stored 
# It uses var_run_name from above
folder_path_assessment <- str_glue("{folder_path_runs}/{var_run_name}/assessment")
folder_path_projections <- str_glue("{folder_path_runs}/{var_run_name}/projections")
folder_path_hypervolumes <- str_glue("{folder_path_runs}/{var_run_name}/hypervolumes")
folder_path_hypervolumes_assessment <- str_glue("{folder_path_runs}/{var_run_name}/hypervolume_assessments")
folder_path_calculations = str_glue("{folder_path_projections}/calculations/")

folder_path_hypervolumes <- str_glue("{folder_path_runs}/{var_run_name}/hypervolume_assessments")

folder_path_figures <- str_glue("{folder_path_runs}/{var_run_name}/figures")



# Create level 1 output folders
dir.create(folder_path_assessment, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_calculations, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_projections, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_hypervolumes, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_hypervolumes_assessment, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_figures, recursive = TRUE, showWarnings = FALSE)

# Set files for level 2 folders (level 1 subfolders)
folder_path_projections_current = str_glue("{folder_path_projections}/current/")
folder_path_projections_future = str_glue("{folder_path_projections}/future/")
folder_path_projections_past = str_glue("{folder_path_projections}/past/")
folder_path_projections_binary = str_glue("{folder_path_projections}/binary/")
folder_path_projections_outputs = str_glue("{folder_path_projections}/output_analysis/")
folder_path_projections_summarized = str_glue("{folder_path_projections}/summarized/")
# This folder is for new results - ensembles created from individual projections
folder_path_projections_manual_ensembles = str_glue("{folder_path_projections}/ensemble_manual/")


# Create level 2 output folders
dir.create(folder_path_projections_current, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_projections_future, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_projections_past, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_projections_binary, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_projections_outputs, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_projections_summarized, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_path_projections_manual_ensembles, recursive = TRUE, showWarnings = FALSE)


# Set work directory

var_biomod_wd <- str_glue("{folder_path_runs}/biomod_output")

dir.create(var_biomod_wd, showWarnings = FALSE, recursive = TRUE)
# NOTE This is an issue! This needs to be moved to single modeling script since it needs to be contained.
# It changes environment status.
# 



# Climate data ------------------------------------------------------------


var_current_climate_df = folder_path_data_climate %>% 
  list.files(recursive = TRUE) %>% 
  str_split(pattern = "/", simplify = TRUE) %>% 
  as.data.frame() %>% 
  dplyr::rename() %>%
  filter(V1 == "current") %>% 
  separate(V2, into = c("type", "bio", "var"), sep = "_", remove = FALSE) %>% 
  transmute(
    period = V1,
    var = var %>% str_remove(".tif"),
    full_path = str_glue("{folder_path_data_climate}/{period}/{V2}")
  )

#

var_current_rasterstack = var_current_climate_df %>% 
  slice(var_variables_vif) %>% 
  dplyr::pull(full_path) %>% 
  stack()

if (var_test_run == TRUE)
{
  var_current_rasterstack <- aggregate(var_current_rasterstack, fact = 5)
}


#


# Deleting files ----------------------------------------------------------


if (exists("delete_evaluations"))
{
  if (delete_evaluations == TRUE)
  {
    warning("Deleting all evaluations")
    folder_path_assessment %>% 
      list.files(recursive = TRUE, full.names = TRUE) %>% 
      file.remove()
    
  }
}


if (exists("delete_current"))
{
  if (delete_current == TRUE)
  {
    warning("Deleting current projections")
    folder_path_projections_current %>% 
      list.files(recursive = TRUE, full.names = TRUE) %>% 
      file.remove()
    
  }
}


if (exists("delete_biomod"))
{
  if (delete_current == TRUE)
  {
    warning("Deleting biomod data")
    var_biomod_wd %>% 
      list.files(recursive = TRUE, full.names = TRUE) %>% 
      file.remove()
    
  }
}
