##########################################
####  Calculate collinear variables
##########################################
#### | Project name: Atra climate
#### | Script type: Data processing
#### | What it does: Description
#### | Date created: february 01, 2021.
#### | Creator: ---
#### | Contact: ---
##########################################

pacman::p_load(Rahat, tidyverse, tictoc, janitor, raster, usdm, sf)

####
# Source variables
source(here::here("R", "main_setup.R"))
  


#### Load species data
species_thinned_sf <- st_read(var_path_species_data) %>% 
  filter(subspecies == "points_all")


# Load climate data -------------------------------------------------------

# Set 
outdir <- folder_path_data_climate

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
    full_path = str_glue("{outdir}/{period}/{V2}")
  )

# current_climate_vars_df

my_current_rasterstack <- current_climate_vars_df %>% 
  pull(full_path) %>% 
  stack()

tic("Extracting values")
species_data_climate <- raster::extract(my_current_rasterstack, species_thinned_sf, method = "simple",
                                        df = TRUE)
toc()
####
species_data_df <- species_data_climate %>%
  pivot_longer(contains("CHELSA")) 

species_data_df_fullnames_raw <- species_data_df %>% 
  mutate(
    layer_names = name %>% 
      str_remove("CHELSA_"),
    layer_names_full = case_when(
      layer_names == "bio_01" ~  "Annual Mean Temperature",
      layer_names == "bio_02" ~  "Mean Diurnal Range",
      layer_names == "bio_03" ~  "Isothermality", 
      layer_names == "bio_04" ~  "Temperature Seasonality",
      layer_names == "bio_05" ~  "Max Temperature of Warmest Month", 
      layer_names == "bio_06" ~  "Min Temperature of Coldest Month",
      layer_names == "bio_07" ~  "Temperature Annual Range",
      layer_names == "bio_08" ~  "Mean Temperature of Wettest Quarter", 
      layer_names == "bio_09" ~  "Mean Temperature of Driest Quarter",
      layer_names == "bio_10" ~  "Mean Temperature of Warmest Quarter",
      layer_names == "bio_11" ~  "Mean Temperature of Coldest Quarter",
      layer_names == "bio_12" ~  "Annual Precipitation",
      layer_names == "bio_13" ~  "Precipitation of Wettest Month",
      layer_names == "bio_14" ~  "Precipitation of Driest Month", 
      layer_names == "bio_15" ~  "Precipitation Seasonality",
      layer_names == "bio_16" ~  "Precipitation of Wettest Quarter",
      layer_names == "bio_17" ~  "Precipitation of Driest Quarter",
      layer_names == "bio_18" ~  "Precipitation of Warmest Quarter",
      layer_names == "bio_19" ~  "Precipitation of Coldest Quarter"
    ) %>% 
      str_replace_all(" ", "_")
  )

species_data_df_fullnames <- species_data_df_fullnames_raw %>% 
  transmute(
    ID, layer_names_full, value
  )
# Define layer names
df_layer_names <- species_data_df_fullnames_raw %>% 
  dplyr::select(name, layer_names, layer_names_full) %>% 
  distinct(name, .keep_all = TRUE)

species_data_df_fullnames_wquarter <- species_data_df_fullnames %>% 
  pivot_wider(
    names_from = layer_names_full, values_from = value
  ) %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

species_data_df_fullnames_noquarter <- species_data_df_fullnames %>% 
  filter(!str_detect(layer_names_full, "Quarter")) %>% 
  pivot_wider(
    names_from = layer_names_full, values_from = value
  ) %>% 
  dplyr::select(-ID) %>% 
  as.matrix()


species_data_df_fullnames_somequarter <- species_data_df_fullnames %>% 
  filter(layer_names_full %notin% c("Mean_Temperature_of_Wettest_Quarter",
                                    "Mean_Temperature_of_Driest_Quarter",
                                    "Precipitation_of_Wettest_Quarter",
                                    "Precipitation_of_Driest_Quarter")
  ) %>% 
  pivot_wider(
    names_from = layer_names_full, values_from = value
  ) %>% 
  dplyr::select(-ID) %>% 
  as.matrix()

# We selected five variables, namely, bio 4 (Temperature seasonality), bio 6
# (minimum temperature of the coldest month), bio 7 (temperature annual range),
# bio 13 (precipitation of the wettest month), bio 15 (precipitation
# seasonality)
my_colinear_vars_noquarter <- vifstep(species_data_df_fullnames_noquarter, th = vif_th)
my_colinear_vars_noquarter

my_colinear_vars_wquarter <- vifstep(species_data_df_fullnames_wquarter, th = vif_th)
my_colinear_vars_wquarter

my_colinear_vars_somequarter <- vifstep(species_data_df_fullnames_somequarter, th = vif_th)
my_colinear_vars_somequarter

####
#### No quarters
# ---------- VIFs of the remained variables -------- 
#   Variables      VIF
# 1                    Isothermality 1.361695 x
# 2 Max_Temperature_of_Warmest_Month 1.241835
# 3         Temperature_Annual_Range 1.233779 x
# 4   Precipitation_of_Wettest_Month 1.106609
# 5        Precipitation_Seasonality 1.221162 x


####
my_current_rasterstack_renamed <- my_current_rasterstack

names(my_current_rasterstack_renamed) <- df_layer_names$layer_names_full


####
exclude(my_current_rasterstack_renamed, my_colinear_vars_wquarter) %>% 
  names() %>% 
  as_tibble() %>% 
  left_join(
    df_layer_names, 
    by = c("value" = "layer_names_full")
  )

exclude(my_current_rasterstack_renamed, my_colinear_vars_noquarter) %>% 
  names() %>% 
  as_tibble() %>% 
  left_join(
    df_layer_names, 
    by = c("value" = "layer_names_full")
  ) 

var_variables_vif <- exclude(my_current_rasterstack_renamed, my_colinear_vars_somequarter) %>% 
  names() %>% 
  as_tibble() %>% 
  left_join(
    df_layer_names, 
    by = c("value" = "layer_names_full")
  ) %>% 
  pull(layer_names) %>% 
  parse_number()

