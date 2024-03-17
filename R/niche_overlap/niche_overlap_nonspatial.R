##########################################
####  Calculate niche overlap - non spatial
##########################################
#### | Project name: Atra climate
#### | Script type: Data processing
#### | What it does: Description
##########################################

## Useful literature
# Pavlek & Mammola - Niche‐based processes explaining the distributions of closely related subterranean spiders [LINK](https://doi.org/10.1111/jbi.13987)
# Barros et al. 2016 - N-dimensional hypervolumes to study stability of complex ecosystems [LINK](https://doi.org/10.1111/ele.12617)
# Lineage‐level distribution models lead to more realistic climate change predictions for a threatened crayfish https://onlinelibrary.wiley.com/doi/10.1111/ddi.13225

# Load packages -----------------------------------------------------------

pacman::p_load(Rahat, tidyverse, readxl, sf, 
               broom, tictoc, hypervolume,
               data.table,  raster,
               biomod2, here)

tic("ENTIRE THING:")
# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))
  

# Source variables
folder_path_data_niche_overlap
# Set folder where output is stored 


#### Get slurm argument
if (length(my_args$slurm) > 0)
{
  my_seed = as.numeric(my_args$slurm)
} else {
  my_seed = 4
}
####

## Source function
source(here("R", "functions", "prepare_model_data.R"))
## Source model functions
source(here("R", "functions", "model_functions.R"))

####
# 10 is for testing purpose; use 100 000
number_of_samples = 100000

string_species_groupA = "random_set1"
string_species_groupB = "random_set2"

run_type = "nonspatial"

# Load data ---------------------------------------------------------------


# Load climate data -------------------------------------------------------



outdir <- "Projects/atra_model/Data/CHELSA_bioclim_2020_cleaned" %>% 
  milkunize2("archive")

current_climate_vars_df <- folder_path_data_climate %>% 
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

selected_vars <- var_variables_vif

climate_current_vif <- current_climate_vars_df %>% 
  slice(selected_vars) %>%
  pull(full_path) %>% 
  stack()

message("Rasters are aggregated for testing purpose")

# Load files

data_species_all <- st_read(var_path_species_data)

# Adjust for modeling -----------------------------------------------------

## Scale predictors
climate_current_vif_scaled <- raster::scale(climate_current_vif, center = TRUE, scale = TRUE)

# Extract raster values
atra_clim <- raster::extract(climate_current_vif_scaled, data_species_all, sp = TRUE) %>% 
  st_as_sf()

species_data_hv <- atra_clim %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(contains("CHELSA")) %>%
  drop_na() %>% 
  mutate(
    set = str_c("random_set", sample(1:2, n(), replace = TRUE))
  )
# 
species_data_groupA_hv <- species_data_hv %>%
  filter(set == string_species_groupA) %>%
  mutate(group = "group_A") %>% 
  dplyr::select(contains("CHELSA"))

species_data_groupB_hv <- species_data_hv %>%
  filter(set == string_species_groupB) %>%
  mutate(group = "group_B") %>% 
  dplyr::select(contains("CHELSA"))


tic()
set.seed(666)
hv_bandwidth_value <- estimate_bandwidth(dplyr::select(species_data_hv, contains("CHELSA")), method="silverman")
toc()


# # #### read csv evaluations

# These are the filenames where hypervolumes will be stored
hv_groupA_filepath <- str_glue("{folder_path_hypervolumes}/hypervolume_{run_type}_{string_species_groupA}_seed{my_seed}.rds")
hv_groupB_filepath <- str_glue("{folder_path_hypervolumes}/hypervolume_{run_type}_{string_species_groupB}_seed{my_seed}.rds")
hv_comparison_filepath <- str_glue("{folder_path_hypervolumes}/hypervolume_{run_type}_comparison_seed{my_seed}.rds")

# This is the filename where comparison between the two will be stored
hv_subsp_filepath <- str_glue("{folder_path_hypervolumes}/hypervolume_{run_type}_seed{my_seed}.csv")

if (!file.exists(hv_subsp_filepath))
{
  #### Running hypervolume for group A
  tic("Calculating hypervolume for group A")
  set.seed(my_seed)
  test_hv_groupA <- hypervolume_gaussian(species_data_groupA_hv, 
                                         kde.bandwidth = hv_bandwidth_value,
                                         samples.per.point = number_of_samples,
                                         name = string_species_groupA, verbose = TRUE)
  toc()
  write_rds(test_hv_groupA, hv_groupA_filepath)
  
  
  tic("Calculating hypervolume for group B")
  set.seed(666)
  test_hv_groupB <- hypervolume_gaussian(species_data_groupB_hv, 
                                         kde.bandwidth = hv_bandwidth_value,
                                         samples.per.point = number_of_samples,
                                         name= string_species_groupB, verbose = TRUE)
  toc()
  
  write_rds(test_hv_groupB, hv_groupB_filepath)
  
  #
  
  #####
  tic("HV set")
  hv_comparison <- hypervolume_set(test_hv_groupA, 
                                   test_hv_groupB, 
                                   num.points.max = 1000000,
                                   check.memory = FALSE) 
  toc()
  # HV set: 105.231 sec elapsed (w 1m points)
  write_rds(hv_comparison, hv_comparison_filepath)
  
  
  
  hv_stats <- hypervolume_overlap_statistics(hv_comparison)
  
  tic("HV distance")
  set.seed(666)
  hv_dist <- hypervolume_distance(test_hv_groupA,
                                  test_hv_groupB, 
                                  type = "centroid")
  toc()
  
  hv_stats_cleaned <- hv_stats %>%
    reshape2::melt() %>% 
    rownames_to_column() %>% 
    transmute(
      index = rowname,
      value,
      group = run_type,
      set = str_c(string_species_groupA, "-", string_species_groupB)
    ) %>% 
    add_row(
      index = "centroid_distance",
      value = hv_dist, 
      group = run_type,
      set = str_c(string_species_groupA, "-", string_species_groupB)
    )

  write_csv(hv_stats_cleaned, hv_subsp_filepath)
  
}
####


toc()
