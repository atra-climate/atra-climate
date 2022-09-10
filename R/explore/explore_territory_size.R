# Explore some data on territory sizes



pacman::p_load(Rahat, tidyverse, here, tictoc, janitor, sf, raster, matrixStats)

library(ggthemes)
library(viridis)
library(ggnewscale)
library(patchwork)
library(ggtext)



# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))
  

df_area_files <- here("results", str_glue("df_area_files_{var_run_name}.csv")) %>% 
  read_csv()



df_area_files %>% 
  group_by(
    taxon, scenario, range
  ) %>% 
  summarize(
    mean_area = mean(area),
    median_area = median(area),
    sd = sd(area),
    sem = sd / sqrt(n())
  ) %>% 
  ungroup()

df_area_files %>% 
  filter(scenario == "current") %>% 
  group_by(
    taxon, range, model
  ) %>% 
  summarize(
    mean_area = mean(area),
    median_area = median(area),
    sd = sd(area),
    sem = sd / sqrt(n())
  ) %>% 
  ungroup() %>% 
  mutate(
    id = case_when(
      taxon == "points_all" & range == "Dinarides" ~ "Species model (Dinarides)",
      taxon == "points_all" & range == "Alps" ~ "Species model (Alps)",
      taxon == "points_atra" & range == "Alps" ~ "Atra model",
      taxon == "points_prenjensis" & range == "Dinarides" ~ "Prenjensis model",
      TRUE ~str_c(taxon, " ", range)
    )) %>% 
  ggplot() +
  # aes(x = taxon, y = mean_area, fill = range) +
  aes(x = id, y = mean_area, fill = id) +
  # geom_col(position = "dodge") +
  geom_col() +
  # geom_errorbar(
  #   position = "dodge",
  #   width = 0.5,
  #   aes(ymin = lower_sem, ymax = upper_sem)) +
  theme_minimal() +
  facet_wrap(~ model) +
  scale_y_continuous(labels = scales::unit_format(unit = "km²")) +
  ggthemes::scale_fill_tableau() +
  labs(
    y = NULL, x = NULL
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 14), 
    panel.border = element_rect(fill = NA, color = "grey70"),
    panel.grid.major.x = element_blank()
  )



########
utm_33n <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Process alpine ranges
range_alps <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("European_Alps") %>% 
  st_read() %>% 
  transmute(
    range = "Alps"
  ) %>% 
st_transform(crs = utm_33n) %>% 
  mutate(
    area = st_area(.) %>% units::set_units("km2")
  )


range_dinarides <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinaric_") %>% 
  st_read() %>% 
  transmute(
    range = "Dinarides"
  ) %>% 
st_transform(crs = utm_33n) %>% 
  mutate(
    area = st_area(.) %>% units::set_units("km2")
  )


df_area_files %>% 
  mutate(
    total_area = ifelse(range == "Alps", range_alps$area, range_dinarides$area),
    occupancy_percent = (area / total_area) * 100
  ) %>% 
  group_by(
    taxon, scenario, range
  ) %>% 
  summarize(
    mean_area = mean(occupancy_percent),
    median_area = median(occupancy_percent),
    sd = sd(occupancy_percent),
    sem = sd / sqrt(n())
  ) %>% 
  ungroup() %>% 
  ggplot() +
  # aes(x = taxon, y = mean_area, fill = range) +
  aes(x = taxon, y = mean_area, fill = scenario) +
  geom_col(position = "dodge") +
  # geom_col() +
  # geom_errorbar(
  #   position = "dodge",
  #   width = 0.5,
  #   aes(ymin = lower_sem, ymax = upper_sem)) +
  theme_minimal() +
  facet_wrap(~ range) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggthemes::scale_fill_tableau() +
  labs(
    y = NULL, x = NULL
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 14), 
    panel.border = element_rect(fill = NA, color = "grey70"),
    panel.grid.major.x = element_blank()
  )


#

df_area_files %>% 
  mutate(
    total_area = ifelse(range == "Alps", range_alps$area, range_dinarides$area),
    occupancy_percent = (area / total_area) * 100
  ) %>% 
  filter(scenario == "current") %>% 
  arrange(desc(occupancy_percent))




r_ensembles_stack <- folder_path_projections_manual_ensembles %>%
  list.files(full.names = TRUE, pattern = "ensemble") %>%
  stack()

r_ensembles_stack_utm <- r_ensembles_stack%>% 
  projectRaster(crs = utm_33n, method = "ngb")

#############
range_alps_wgs <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("European_Alps") %>% 
  st_read() %>% 
  transmute(
    range = "Alps"
  )

range_alps_utm <- range_alps_wgs %>% 
  st_transform(
    crs = utm_33n
  )


range_dinarides_wgs <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinaric_") %>% 
  st_read() %>% 
  transmute(
    range = "Dinarides"
  )

range_dinarides_utm <- range_dinarides_wgs %>% 
  st_transform(
    crs = utm_33n
  )

range_combined <- rbind(
  range_dinarides_wgs,
  range_alps_wgs
)

range_combined_utm <- rbind(
  range_dinarides_utm,
  range_alps_utm
)

r_dinarides <- mask(r_ensembles_stack, range_dinarides_wgs)
r_alps <- mask(r_ensembles_stack, range_alps_wgs)

r_combined <- mask(r_ensembles_stack, range_combined)




# Calculate territory as dataframe ----------------------------------------

r_dinarides_vals <- r_dinarides %>% 
  getValues()

r_alps_vals <- r_alps %>% 
  getValues()

r_combined_vals <- r_combined %>% 
  getValues()

values_dinarides_df <- r_dinarides_vals %>%
  as_tibble() %>% 
  map(~tabyl(.x, show_na = FALSE)) %>% 
  bind_rows(.id = "id") %>% 
  mutate(
    scenario = case_when(
      str_detect(id, "current") ~ "current",
      str_detect(id, "rcp26") ~ "rcp26",
      str_detect(id, "rcp85") ~ "rcp85",
      TRUE ~ "OTHER"),
    taxon = case_when(
      str_detect(id, "points_all") ~ "Species level",
      str_detect(id, "points_atra") ~ "Subspecies atra",
      str_detect(id, "points_prenjensis") ~ "Subspecies prenjensis",
    ),
    range = "Dinarides"
  ) %>% 
  transmute(
    taxon, scenario, range, value = .x, n, percent = percent * 100
  )
  

values_alps_df <- r_alps_vals %>%
  as_tibble() %>% 
  map(~tabyl(.x, show_na = FALSE)) %>% 
  bind_rows(.id = "id") %>% 
  mutate(
    scenario = case_when(
      str_detect(id, "current") ~ "current",
      str_detect(id, "rcp26") ~ "rcp26",
      str_detect(id, "rcp85") ~ "rcp85",
      TRUE ~ "OTHER"),
    taxon = case_when(
      str_detect(id, "points_all") ~ "Species level",
      str_detect(id, "points_atra") ~ "Subspecies atra",
      str_detect(id, "points_prenjensis") ~ "Subspecies prenjensis",
    ),
    range = "Alps"
  ) %>% 
  transmute(
    taxon, scenario, range, value = .x, n, percent = percent * 100
  )

values_combined_df <- r_combined_vals %>%
  as_tibble() %>% 
  map(~tabyl(.x, show_na = FALSE)) %>% 
  bind_rows(.id = "id") %>% 
  mutate(
    scenario = case_when(
      str_detect(id, "current") ~ "current",
      str_detect(id, "rcp26") ~ "rcp26",
      str_detect(id, "rcp85") ~ "rcp85",
      TRUE ~ "OTHER"),
    taxon = case_when(
      str_detect(id, "points_all") ~ "Species level",
      str_detect(id, "points_atra") ~ "Subspecies atra",
      str_detect(id, "points_prenjensis") ~ "Subspecies prenjensis",
    ),
    range = "Combined"
  ) %>% 
  transmute(
    taxon, scenario, range, value = .x, n, percent = percent * 100
  )

####
values_combined_df %>% 
  filter(scenario == "current") %>% 
  mutate(
    percent_rcl = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
    ),
    percent_rcl2 = case_when(
      value == 0 ~ "zero",
      # value == 1 ~ "all",
      value >= 0.5  ~ "more than 05",
      value < 0.5  ~ "less than 05",
    )
    
  ) %>% 
  group_by(taxon, percent_rcl2) %>% 
  summarize(
    sum = sum(percent)
  )

values_combined_df %>% 
  filter(scenario == "current") %>% 
  ggplot() +
  aes(x = taxon, y = percent, fill = as.factor(value)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "top"
  )

values_combined_df %>%
  filter(scenario == "current") %>% 
  mutate(
    percent_rcl = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
    ),
    percent_rcl2 = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5  ~ "more than 05",
      value < 0.5  ~ "less than 05",
    )
    
  ) %>% 
  group_by(taxon, scenario, percent_rcl2) %>% 
  summarize(
    sum = sum(percent),
    max = max(value)
  ) %>%
  ungroup()
 

# Calculate territory as area ---------------------------------------------
library(stars)

r_combined_utm <- mask(r_ensembles_stack_utm, range_combined_utm)
r_alps_utm <- mask(r_ensembles_stack_utm, range_alps_utm)
r_dinarides_utm <- mask(r_ensembles_stack_utm, range_dinarides_utm)

## Percentages combined
sf_combined <- r_combined_utm %>% 
  st_as_stars() %>% 
  st_as_sf() %>% 
  mutate(
    cell_area = st_area(.) %>% 
      units::set_units("km2")
  )

names(sf_combined)[1:9] <- names(r_combined)

sf_combined %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      # value == 1 ~ "all",
      value >= 0.5 ~ "more than 05",
      value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
      percent = total / sum(total) * 100
  ) %>% 
  ungroup() 

## Percentages alps
sf_alps <- r_alps_utm %>% 
  st_as_stars() %>% 
  st_as_sf() %>% 
  mutate(
    cell_area = st_area(.) %>% 
      units::set_units("km2")
  )

names(sf_alps)[1:9] <- names(r_alps)

sf_alps %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
    filter(str_detect(name, "atra")) %>% 
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      # value == 1 ~ "all",
      value >= 0.5 ~ "more than 05",
      value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = (total / sum(total)) * 100
  ) 

## Percentages dinarides
sf_dinarides <- r_dinarides_utm %>% 
  st_as_stars() %>% 
  st_as_sf() %>% 
  mutate(
    cell_area = st_area(.) %>% 
      units::set_units("km2")
  )

names(sf_dinarides)[1:9] <- names(r_dinarides)

sf_dinarides %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      # value == 1 ~ "all",
      value >= 0.5 ~ "more than 05",
      value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = (total / sum(total)) * 100
  ) 




# Make tables -------------------------------------------------------------

## Combined
sf_combined %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      # value >= 0.5 ~ "more than 05",
      # value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = round(total / sum(total) * 100, 1)
  ) %>% 
  ungroup() %>% 
  filter(values_reclassified2 != "OTHER") %>% 
  dplyr::select(-total) %>% 
  # filter(str_detect(name, "26")) %>% 
  View()

#####
sf_combined %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      # value == 1 ~ "all",
      value >= 0.5 ~ "more than 05",
      # value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = round(total / sum(total) * 100, 1)
  ) %>% 
  ungroup() %>% 
  filter(values_reclassified2 != "OTHER") %>% 
  dplyr::select(-total) %>% 
  # filter(str_detect(name, "85")) %>% 
  View()

#### Dinarides
sf_dinarides %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      # value >= 0.5 ~ "more than 05",
      # value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = round(total / sum(total) * 100, 1)
  ) %>% 
  ungroup() %>% 
  filter(values_reclassified2 != "OTHER") %>% 
  dplyr::select(-total) %>% 
  # filter(str_detect(name, "85")) %>% 
  View()


sf_dinarides %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      # value == 1 ~ "all",
      value >= 0.5 ~ "more than 05",
      # value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = round(total / sum(total) * 100, 1)
  ) %>% 
  ungroup() %>% 
  filter(values_reclassified2 != "OTHER") %>% 
  dplyr::select(-total) %>% 
  # filter(str_detect(name, "85")) %>% 
  View()

#### Alps
sf_alps %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      # value >= 0.5 ~ "more than 05",
      # value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = round(total / sum(total) * 100, 1)
  ) %>% 
  ungroup() %>% 
  filter(values_reclassified2 != "OTHER") %>% 
  dplyr::select(-total) %>% 
  View()


sf_alps %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  mutate(
    values_reclassified = case_when(
      value == 0 ~ "zero",
      value == 1 ~ "all",
      value >= 0.5 & value != 1 ~ "more than 05",
      value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    ),
    values_reclassified2 = case_when(
      value == 0 ~ "zero",
      # value == 1 ~ "all",
      value >= 0.5 ~ "more than 05",
      # value < 0.5  ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified2, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = round(total / sum(total) * 100, 1)
  ) %>% 
  ungroup() %>% 
  filter(values_reclassified2 != "OTHER") %>% 
  dplyr::select(-total) %>% 
  View()



# Make tables using 5%, 50%, 95% ------------------------------------------


# Make tables -------------------------------------------------------------

## Combined
sf_combined %>% 
  st_set_geometry(NULL) %>% 
  pivot_longer(
    contains("ensemble")
  ) %>% 
  
  mutate(
    values_reclassified = case_when(
      value <= 0.1 ~ "10%",
      value >= 0.9 ~ "90%",
      
      # value >= 0.05 & value < 0.5 ~ "more than 05",
      # value >= 0.5 & value != 1 ~ "more than 05",
      # value < 0.5 & value != 0 ~ "less than 05",
      TRUE ~ "OTHER"
    )
  ) %>% 
  group_by(
    values_reclassified, name
  ) %>% 
  summarize(
    total = sum(cell_area)
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(
    percent = round(total / sum(total) * 100, 1)
  ) %>% 
  ungroup() %>% 
  filter(values_reclassified != "OTHER") %>% 
  dplyr::select(-total) %>% 
  pivot_wider(
    names_from = values_reclassified,
    values_from = percent
  )
################## 