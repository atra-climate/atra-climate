#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=ALL
#SBATCH --time=6:00:00
#SBATCH --mem=32G


##########################################
####  Fit climate models with biomod2
##########################################
#### | Project name: Atra model
#### | Script type: Data processing
#### | What it does: Description
##########################################

# Load packages -----------------------------------------------------------

pacman::p_load(tidyverse, readxl, sf, 

               tictoc, 
               Rahat,
               data.table, caret, raster,
               biomod2, usdm, here)

# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))
  
#### 
print(my_args)
## Create wd
setwd(var_biomod_wd)
## Source function
source(here("R", "functions", "prepare_model_data.R"))
## Source model functions
source(here("R", "functions", "model_functions.R"))
## Set taxon name, used below in clean_model_evaluation

# Load data ---------------------------------------------------------------


# Load climate data -------------------------------------------------------

climate_current_vif <- var_current_rasterstack
#### Load species data ####
atra_raw_sf <- st_read(var_path_species_data)
# 
# Adjust for modeling -----------------------------------------------------
atra_data_sf_clean <- atra_raw_sf %>%
  filter(subspecies == var_taxon_name) %>%
  transmute(
    PA = 1
    )
# 
tic("Preparing response variable")
response_variable_data <- prepare_model_data(response_vars = atra_data_sf_clean,
                                             explanatory_vars = climate_current_vif,
                                             pa_num = 1000)
toc()

respvar_binary <- response_variable_data %>% 
  pull(PA) %>% 
  as.character() %>% 
  as.numeric()

respvar_coordinates <- response_variable_data %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  transmute(x = X, y = Y)

# biomod2 part ------------------------------------------------------------

#### Prepare data for modeling
tic("Preparing model data")
model_data <- BIOMOD_FormatingData(resp.var = respvar_binary,
                                   expl.var = climate_current_vif, # explanatory raster data
                                   resp.xy = respvar_coordinates,
                                   resp.name = str_c("model_", var_taxon_name),
                                   na.rm = TRUE)
toc()
# 
glm_params <- list(type = "polynomial", interaction.level = 0,
                   test = "AIC", family = "binomial")
# GAM
gam_params <- list(Spline = 3)
## MaxEnt
# MaxEnt path comes from main_setup
maxent_params <- list(path_to_maxent.jar = var_path_maxent,
                      memory_allocated = 2048)

#### Still to add more modeling tecniques; check from meeting notes what have we decided upon
biomod_options <- BIOMOD_ModelingOptions(GLM = glm_params, 
                                         GAM = gam_params,
                                         MAXENT.Phillips = maxent_params
)

# Modeling stuff


tic("Training models")
myBiomodModelOut <- BIOMOD_Modeling(data = model_data,
                                    models = var_model_algorithms,
                                    models.options = biomod_options,
                                    VarImport = 1,
                                    DataSplit = 70,
                                    models.eval.meth = c("ROC", "TSS"),
                                    do.full.models=FALSE,
                                    modeling.id = str_c("fitted_", var_taxon_name)
                                    )
toc()


variable_importances <- biomod2::get_variables_importance(myBiomodModelOut)

variable_importances_df <- variable_importances %>% 
  reshape2::melt() %>% 
  transmute(
    variable_name = Var1,
    algorithm = Var2,
    fold = Var3,
    variable_importance = value,
    type = var_taxon_name
  )

varimp_filepath <- str_glue("{folder_path_assessment}/model_variable_importance_{var_taxon_name}.csv")

write_csv(variable_importances_df, varimp_filepath)


####
myBiomodModelEval <- biomod2::get_evaluations(myBiomodModelOut)

my_evals <- clean_model_evaluations(myBiomodModelEval) %>% 
  mutate(
    taxon = var_taxon_name
  )

eval_filepath <- str_glue("{folder_path_assessment}/model_evaluations_{var_taxon_name}.csv")

write_csv(my_evals, eval_filepath)

#### Ensemble models part ####
## Run ensemble models
ensemble_model_out <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,
                                              chosen.models = "all",
                                              em.by = 'PA_dataset+repet',
                                              eval.metric = "TSS", #metric used to scale the ensamble
                                              eval.metric.quality.threshold = 0.7,
                                              prob.mean = FALSE,
                                              committee.averaging = FALSE,
                                              prob.mean.weight = TRUE, #weight by TSS, Luca had T
                                              prob.cv = FALSE,
                                              prob.ci = FALSE,
                                              # prob.ci.alpha = 0.05,
                                              prob.median = FALSE,
                                              prob.mean.weight.decay = "proportional")


####
myBiomodModelEval_ensemble <- biomod2::get_evaluations(ensemble_model_out)

my_evals_ensemble <- clean_model_evaluations(myBiomodModelEval_ensemble, ensemble = TRUE) %>% 
  mutate(
    taxon = var_taxon_name
  )

eval_filepath_ensemble <- str_glue("{folder_path_assessment}/model_evaluations_{var_taxon_name}_ensemble.csv")

write_csv(my_evals_ensemble, eval_filepath_ensemble)

####
pres.only.eval <- BIOMOD_presenceonly(myBiomodModelOut, ensemble_model_out)

evals_presonly <- pres.only.eval$eval %>% 
  transmute(model = Model.name, 
            metric = Eval.metric,
            value = Testing.data,
            taxon = var_taxon_name)


eval_presonly_filepath <- str_glue("{folder_path_assessment}/model_evaluations_presenceonly_{var_taxon_name}.csv")

write_csv(evals_presonly, eval_presonly_filepath)

# Projecting models into different time


####################################
#### Projections entire dataset ####
####################################
# Project models for current climate
tic("Projecting models")
model_projection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                      new.env = climate_current_vif,
                                      compress = 'xz',
                                      do.stack = FALSE,
                                      proj.name = str_c("current_climate_", var_taxon_name),
                                      selected.models = "all",
                                      binary.meth = "TSS",
                                      build.clamping.mask = FALSE)
toc()


prediction_current <- get_predictions(model_projection)

for (i_pred in 1:nlayers(prediction_current))
{
  
  r_lyr <- prediction_current[[i_pred]]
  
  
  run_name <- names(r_lyr) %>% 
    str_remove("Salamandra.atra_AllData_") %>% 
  str_remove(str_c("model.", str_replace(var_taxon_name, "_", "."), "_"))
  
  projection_outname <- str_glue("{folder_path_projections}/current/Projection_current_{var_taxon_name}_{run_name}.tif")
  
  if (!file.exists(projection_outname))
  {
    writeRaster(r_lyr, projection_outname, options = "COMPRESS=LZW")  
  } else {
    print(str_glue("File for {run_name} exists"))
  }
  
}

#### Do ensemble model projections
ensemble_model_projection <- BIOMOD_EnsembleForecasting(projection.output = model_projection,
                                                        EM.output = ensemble_model_out,
                                                        total.consensus = TRUE,
                                                        do.stack = FALSE,
                                                        binary.meth = "TSS")

# 8*5
prediction_current_ensemble <- get_predictions(ensemble_model_projection)

for (i_pred in 1:nlayers(prediction_current_ensemble))
{
  
  r_lyr_ens <- prediction_current_ensemble[[i_pred]]
  
  run_name_ens <- names(r_lyr_ens) %>% 
    str_remove("Salamandra.atra_EMwmeanByTSS_mergedAlgo_") %>% 
    str_remove("_AllData") %>% 
    str_remove(str_c("model.", str_replace(var_taxon_name, "_", "."), "_"))
  
  
  projection_outname_ens <- str_glue("{folder_path_projections}/current/Projection_current_{var_taxon_name}_{run_name_ens}_ensemble.tif")
  
  if (!file.exists(projection_outname_ens))
  {
    writeRaster(r_lyr_ens, projection_outname_ens, options = "COMPRESS=LZW")  
  } else {
    print(str_glue("File for {run_name_ens} exists"))
  }
  
}
#####################################

