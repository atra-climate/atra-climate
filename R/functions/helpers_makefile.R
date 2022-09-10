##########################################
####  Makefile helper scripts
##########################################
#### | Project name: Atra model
#### | Script type: Data processing
#### | What it does: Runs data checks to create files used as dependencies and targets in make
##########################################

# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

 source(here::here("R", "main_setup.R"))
  


#### ####


current_projections_number <- folder_path_projections_current %>% 
  list.files() %>% 
  length()

folder_path_helpers_make %>% 
  list.files(full.names = TRUE, pattern = "num_current_projections_") %>% 
  file.remove()

num_projections_number_current <- stringr::str_glue("{folder_path_helpers_make}/num_current_projections_{current_projections_number}.makedat")

file.create(num_projections_number_current)

message(stringr::str_glue("There are {current_projections_number} current projections"))

#### Past ####

past_projections_number <- folder_path_projections_past %>% 
  list.files() %>% 
  length()

folder_path_helpers_make %>% 
  list.files(full.names = TRUE, pattern = "num_past_projections_") %>% 
  file.remove()

num_projections_number_past <- stringr::str_glue("{folder_path_helpers_make}/num_past_projections_{past_projections_number}.makedat")

file.create(num_projections_number_past)

message(stringr::str_glue("There are {past_projections_number} past projections"))


#### Future ####

future_projections_number <- folder_path_projections_future %>% 
  list.files() %>% 
  length()

folder_path_helpers_make %>% 
  list.files(full.names = TRUE, pattern = "num_future_projections_") %>% 
  file.remove()

num_projections_number_future <- stringr::str_glue("{folder_path_helpers_make}/num_future_projections_{future_projections_number}.makedat")

file.create(num_projections_number_future)

message(stringr::str_glue("There are {future_projections_number} future projections"))

#### Check number of binary projections


binary_projections_number <- folder_path_projections_binary %>% 
  list.files() %>% 
  length()

folder_path_helpers_make %>% 
  list.files(full.names = TRUE, pattern = "num_binary_projections_") %>% 
  file.remove()

num_projections_number_binary <- stringr::str_glue("{folder_path_helpers_make}/num_binary_projections_{binary_projections_number}.makedat")

file.create(num_projections_number_binary)

message(stringr::str_glue("There are {binary_projections_number} binary projections"))

#### Get number of hypervolumes
num_hypervolumes_number_binary <- folder_path_hypervolumes %>%
  list.files(pattern = "csv")

