##########################################
####  Read model evaluations
##########################################
#### | Project name: Atra model
#### | Script type: Data processing
#### | What it does: Description
##########################################

# Load packages -----------------------------------------------------------

pacman::p_load(tidyverse, 
               tictoc, 
               Rahat,
               here
               )

# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

## Remove stuff created here first
# delete_evaluations = TRUE
# delete_current = TRUE

source(here::here("R", "main_setup.R"))
  

my_files <- folder_path_assessment %>% 
  list.files(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("evaluations_presence") %>% 
  map(read_csv) %>% 
  reduce(rbind)


my_files %>% 
  filter(metric == "TSS") %>% 
  filter(str_detect(model, "EM")) %>% 
  ggplot() +
    aes(x = taxon, y = value, fill = taxon) +
  geom_col() +
  labs(title = "TSS of fitted ensemble models - test run") +
  theme_minimal()
    
#### How many files?
## For past
# 7 scenarios, times 3 taxa, times n models + ensemble
# 7 * 3 * 4
folder_path_projections_past %>%
    list.files(full.names = TRUE, recursive = TRUE)

folder_path_projections_future %>%
  list.files(full.names = TRUE, recursive = TRUE)

# 
# folder_path_projections_past %>%
#   list.files(full.names = TRUE, recursive = TRUE) %>%
#   file.remove()


# Variable importances ----------------------------------------------------



evals_variable_importance <- folder_path_assessment %>% 
  list.files(recursive = TRUE, full.names = TRUE) %>% 
  str_subset("_importance") %>% 
  map(read_csv) %>% 
  reduce(rbind)


# Plot variables importance
evals_variable_importance %>% 
  ggplot() +
  aes(x = variable_name, y = variable_importance, fill = algorithm) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "top"
  ) +
  facet_wrap(~ type)

