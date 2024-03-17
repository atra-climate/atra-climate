library(biomod2)
library(tidyverse)


source(here::here("R", "main_setup.R"))

setwd(str_glue("{folder_path_runs}/biomod_output_2023"))


models_list <- list.files(getwd(), recursive = TRUE, pattern = "models.out", full.names = TRUE) %>% 
  str_subset("ensemble", negate = TRUE)


# Load models for prenjensis

my_mod_prenjensis <- models_list[3]

bm_out_prenjensis <- load(my_mod_prenjensis)

get(bm_out_prenjensis)


glm_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "GLM")
gbm_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "GBM")
gam_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "GAM")
rf_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "RF")


ann_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "ANN")
maxent_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "MAXENT.Phillips")
cta_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "CTA")
mars_prenjensis <- BIOMOD_LoadModels(model.points.prenjensis.fitted_points_prenjensis.models.out, models = "MARS")



model_dat_prenjensis <- biomod2::response.plot2(c(glm_prenjensis, gbm_prenjensis, gam_prenjensis, rf_prenjensis,
                                                  ann_prenjensis, maxent_prenjensis, cta_prenjensis, mars_prenjensis), 
                                                Data = get_formal_data(model.points.prenjensis.fitted_points_prenjensis.models.out,'expl.var'),
                                                show.variables = get_formal_data(model.points.prenjensis.fitted_points_prenjensis.models.out,'expl.var.names'),
                                                plot = FALSE
)


model_dat_clean_prenjensis <- model_dat_prenjensis %>%
  ## transform the pred.name to extract model, cv run and data info
  mutate(
    species = pred.name %>% strsplit('_') %>% sapply(function(x) x[1]) %>% str_remove("model.") %>% str_replace("[.]", "_"),
    pa.dat = pred.name %>% strsplit('_') %>% sapply(function(x) x[2]),
    cv.rep = pred.name %>% strsplit('_') %>% sapply(function(x) x[3]),
    model = pred.name %>% strsplit('_') %>% sapply(function(x) x[4])
  )

#
model_dat_clean_prenjensis %>% 
  dplyr::select(-pa.dat, -cv.rep) %>%
  write_csv(here::here("data", "response_curves", "response_data_raw_points_prenjensis.csv"))

####

# Load models for atra subspecies

my_mod_atra <- models_list[2]

bm_out_atra <- load(my_mod_atra)

get(bm_out_atra)


glm_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "GLM")
gbm_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "GBM")
gam_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "GAM")
rf_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "RF")


ann_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "ANN")
maxent_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "MAXENT.Phillips")
cta_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "CTA")
mars_atra <- BIOMOD_LoadModels(model.points.atra.fitted_points_atra.models.out, models = "MARS")



model_dat_atra <- biomod2::response.plot2(c(glm_atra, gbm_atra, gam_atra, rf_atra,
                                            ann_atra, maxent_atra, cta_atra, mars_atra), 
                                          Data = get_formal_data(model.points.atra.fitted_points_atra.models.out,'expl.var'),
                                          show.variables = get_formal_data(model.points.atra.fitted_points_atra.models.out,'expl.var.names'),
                                          plot = FALSE
)


model_dat_clean_atra <- model_dat_atra %>%
  ## transform the pred.name to extract model, cv run and data info
  mutate(
    species = pred.name %>% strsplit('_') %>% sapply(function(x) x[1]) %>% str_remove("model.") %>% str_replace("[.]", "_"),
    pa.dat = pred.name %>% strsplit('_') %>% sapply(function(x) x[2]),
    cv.rep = pred.name %>% strsplit('_') %>% sapply(function(x) x[3]),
    model = pred.name %>% strsplit('_') %>% sapply(function(x) x[4])
  )

#
model_dat_clean_atra %>% 
  dplyr::select(-pa.dat, -cv.rep) %>% 
  write_csv(here::here("data", "response_curves", "response_data_raw_points_atra.csv"))

####

# Load models for species-level


my_mod_all <- models_list[1]

bm_out_all <- load(my_mod_all)

get(bm_out_all)


glm_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "GLM")
gbm_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "GBM")
gam_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "GAM")
rf_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "RF")


ann_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "ANN")
maxent_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "MAXENT.Phillips")
cta_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "CTA")
mars_all <- BIOMOD_LoadModels(model.points.all.fitted_points_all.models.out, models = "MARS")



model_dat_all <- biomod2::response.plot2(c(glm_all, gbm_all, gam_all, rf_all,
                                           ann_all, maxent_all, cta_all, mars_all), 
                                         Data = get_formal_data(model.points.all.fitted_points_all.models.out,'expl.var'),
                                         show.variables = get_formal_data(model.points.all.fitted_points_all.models.out,'expl.var.names'),
                                         plot = FALSE
)


model_dat_clean_all <- model_dat_all %>%
  ## transform the pred.name to extract model, cv run and data info
  mutate(
    species = pred.name %>% strsplit('_') %>% sapply(function(x) x[1]) %>% str_remove("model.") %>% str_replace("[.]", "_"),
    pa.dat = pred.name %>% strsplit('_') %>% sapply(function(x) x[2]),
    cv.rep = pred.name %>% strsplit('_') %>% sapply(function(x) x[3]),
    model = pred.name %>% strsplit('_') %>% sapply(function(x) x[4])
  )

#
model_dat_clean_all %>% 
  dplyr::select(-pa.dat, -cv.rep) %>% 
  write_csv(here::here("data", "response_curves", "response_data_raw_points_all.csv"))


model_dat_clean_merged <- rbind(model_dat_clean_all,
                                model_dat_clean_atra,
                                model_dat_clean_prenjensis)

model_dat_clean_merged %>% 
  dplyr::select(-pa.dat, -cv.rep) %>% 
  write_csv(here::here("data", "response_curves", "response_data_merged_data_cleaned.csv"))



model_dat_clean_merged <- "Projects/atra_climate_model/data/response_curves/response_data_merged_data_cleaned.csv" %>% 
  read_csv() %>% 
  mutate(
    species = case_when(
      species == "points_all" ~ "Salamandra atra",
      species == "points_atra" ~ "Salamandra atra atra",
      species == "points_prenjensis" ~ "Salamandra atra prenjensis",
    ) %>% 
      str_wrap(15),
    expl.name = case_when(
      expl.name == "CHELSA_bio_04" ~ "temperature seasonality", 
      expl.name == "CHELSA_bio_06" ~ "minimum temperature of the coldest month", 
      expl.name == "CHELSA_bio_07" ~ "temperature annual range", 
      expl.name == "CHELSA_bio_13" ~ "precipitation of the wettest month",  
      expl.name == "CHELSA_bio_15" ~ "precipitation seasonality"
    ) %>% 
      str_to_title() %>% 
      str_wrap(20)
  )


####
p_combined <- model_dat_clean_merged %>% 
  group_by(species, expl.name, expl.val) %>% 
  summarize(
    average = mean(pred.val),
    median = median(pred.val),
    n = n(),
    standard_deviation = sd(pred.val),
    standard_error = standard_deviation/sqrt(n)
  ) %>%
  ungroup() %>% 
  ggplot(
    aes(
      x = expl.val,
      y = average,
      ymin = average - standard_error, 
      ymax = average + standard_error,
      colour = expl.name,
      group = 1
    )
  ) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.25, color = "transparent", fill = "grey60") + 
  facet_grid(species ~ expl.name, scales = 'free_x') +
  labs(
    x = "Variable value",
    y = "Predicted suitability",
    colour = "model type"
  ) + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_color_brewer(type = 'qual', palette = 4) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 12, face = "italic", lineheight = 0.75),
    strip.text.x = element_text(size = 10, lineheight = 0.75),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

p_combined

ggsave(here::here("results", "response_curves.png"), p_combined, 
       dpi = 300, 
       width = 8.5, height = 5.5)
