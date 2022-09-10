
##########################################
#### Developing visualization for figure 2
##########################################
#### | Project name: My research project
#### | Script type: Data processing
#### | What it does: Description

##########################################

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



# SEM is calculated by taking the standard deviation and dividing it by the square root of the sample size.
#### Plot figure 2

df_combined_areas <- df_area_files %>% 
  group_by(
    taxon, scenario
  ) %>% 
  summarize(
    mean_area = mean(area),
    median_area = median(area),
    sd = sd(area),
    sem = sd / sqrt(n())
  ) %>% 
  ungroup() %>% 
  mutate(
    upper_sem = mean_area + sem,
    lower_sem = mean_area - sem,
    range = "Entire range"
  ) %>% 
  transmute(
    taxon, range, scenario, mean_area, lower_sem, upper_sem
  )

p_figure3 <-
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
  ungroup() %>% 
  mutate(
    upper_sem = mean_area + sem,
    lower_sem = mean_area - sem
  ) %>% 
  filter(
    # (taxon == "points_atra" & range == "Alps") |
    #   (taxon == "points_prenjensis" & range == "Dinarides") |
    #   (taxon == "points_all" & (range == "Dinarides" | range == "Alps"))
    
    
  ) %>% 
  mutate(
    id = case_when(
      taxon == "points_all" & range == "Dinarides" ~ "Species model (Dinarides)",
      taxon == "points_all" & range == "Alps" ~ "Species model (Alps)",
      taxon == "points_atra" & range == "Alps" ~ "Atra model",
      taxon == "points_prenjensis" & range == "Dinarides" ~ "Prenjensis model",
      TRUE ~str_c(taxon, " ", range)
    ),
    id = str_wrap(id, width = 18)
  ) %>%
     transmute(
      taxon, range, scenario, mean_area, lower_sem, upper_sem
    ) %>% 
    rbind(df_combined_areas) %>% 
    mutate(
      
      scenario2 = case_when(
        scenario == "current" ~ "Current climate",
        scenario == "rcp26" ~ "Future climate (RCP2.6)",
        scenario == "rcp85" ~ "Future climate (RCP8.5)"
      ),
      taxon = case_when(
        taxon == "points_all" ~ "Species model",
        taxon == "points_atra" ~ "Subspecies model *atra*",
        taxon == "points_prenjensis" ~ "Subspecies model *prenjensis*",
      )
    ) %>%
    #### Added filter to remove results where taxon is extrapolated spatially
    filter(!(range == "Alps" & taxon == "Subspecies model *prenjensis*")) %>% 
    filter(!(range == "Dinarides" & taxon == "Subspecies model *atra*")) %>% 
    
  ggplot() +
  # aes(x = taxon, y = mean_area, fill = range) +
  # aes(x = id, y = mean_area, fill = id) +
  aes(x = taxon, y = mean_area, fill = scenario2) +
  geom_col(position = "dodge") +
  # geom_col() +
  geom_errorbar(
    position = position_dodge2(padding = 0.5),
    # width = 0.5,
    aes(ymin = lower_sem, ymax = upper_sem)) +
  theme_minimal() +
  facet_wrap(~ range, scales = "free_x") +
  scale_y_continuous(labels = scales::unit_format(unit = "km²")) +
  # ggthemes::scale_fill_tableau() +
  scale_fill_manual(values = c("#2CA02C", "#1F77B4", "#FF7F0E")) +
  labs(
    y = NULL, x = "Study unit", fill = NULL
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 24),
    strip.text = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_markdown(size = 14), 
    axis.title.x = element_text(size = 24, color = "grey10"),
    panel.border = element_rect(fill = NA, color = "grey70"),
    panel.grid.major.x = element_blank()
  )


# p_figure3
## Set filename for the output filemap
fig3_suitable_climate <- str_glue("{folder_path_figures}/Fig3_results_suitable_climate_bars_{var_run_name}_{Rahat::today()}.png")

## Final file version for submission stored in 600 dpi's


ggsave(
  # here("results", "figure_territory_size.png"),
       fig3_suitable_climate,
       p_figure3,
       dpi = 200, height = 9, width = 20)

####

p_figure3a <-
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
  ungroup() %>% 
  mutate(
    upper_sem = mean_area + sem,
    lower_sem = mean_area - sem
  ) %>% 
  # filter(
  #   (taxon == "points_atra" & range == "Alps") |
  #     (taxon == "points_prenjensis" & range == "Dinarides") |
  #     (taxon == "points_all" & (range == "Dinarides" | range == "Alps"))
  #   
  #   
  # ) %>% 
  mutate(
    # id = str_c(taxon, " ", range),
    
    taxon = case_when(
      taxon == "points_all" ~ "Species model",
      taxon == "points_atra" ~ "Atra model",
      taxon == "points_prenjensis" ~ "Prenjensis model",
    ),
    scenario2 = case_when(
      scenario == "current" ~ "Current climate",
      scenario == "rcp26" ~ "Future climate (RCP2.6)",
      scenario == "rcp85" ~ "Future climate (RCP8.5)"
    ),
    # id = str_wrap(id, width = 18)
  ) %>%
  ggplot() +
  # aes(x = taxon, y = mean_area, fill = range) +
  aes(x = taxon, y = mean_area, fill = taxon) +
  # geom_col(position = "dodge") +
  geom_col() +
  geom_errorbar(
    position = "dodge",
    width = 0.5,
    aes(ymin = lower_sem, ymax = upper_sem)) +
  theme_minimal() +
  # coord_flip() +
  facet_wrap(range ~ scenario2, nrow = 1) +
  scale_y_continuous(labels = scales::unit_format(unit = "km²")) +
  ggthemes::scale_fill_tableau() +
  labs(
    y = NULL, x = NULL
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 16), 
    panel.border = element_rect(fill = NA, color = "grey70"),
    panel.grid.major.x = element_blank()
  )

p_figure3a
#######################



# Make plots for changes in area size

####

for (my_taxon in pull(distinct(df_area_files, taxon)))
{
  for (my_model in pull(distinct(df_area_files, model)))
  {
    
    str_glue("{here('results')}/area_changes_{var_run_name}") %>% dir.create(showWarnings = FALSE)
    
    vec_outname <- str_glue("{here('results')}/area_changes_{var_run_name}/{my_taxon}_{my_model}.csv")
    
    if( !file.exists(vec_outname))
    {
      
      my_current_area_alps_val <- df_area_files %>% 
        filter(taxon == my_taxon, 
               model == my_model, 
               scenario == "current", 
               range == "Alps") %>% 
        pull(area)
      
      my_current_area_dinarides_val <- df_area_files %>% 
        filter(taxon == my_taxon, 
               model == my_model, 
               scenario == "current", 
               range == "Dinarides") %>% 
        pull(area)
      
      my_df_alps <- df_area_files %>% 
        filter(taxon == my_taxon, 
               model == my_model, 
               scenario %in% c("rcp26", "rcp85"),
               range == "Alps") %>% 
        mutate(
          current_size = my_current_area_alps_val,
          area_change = area - current_size
        )
      
      my_df_dinarides <- df_area_files %>% 
        filter(taxon == my_taxon, 
               model == my_model, 
               scenario %in% c("rcp26", "rcp85"),
               range == "Dinarides") %>% 
        mutate(
          current_size = my_current_area_dinarides_val,
          area_change = area - current_size
        )
      
      
      
      rbind(
        my_df_dinarides,
        my_df_alps
      ) %>% 
        write_csv(
          vec_outname
        )
    }
  }
}

##


var_run_name = "climate_impact_VIF"

df_area_changes <- here("results", str_glue("area_changes_{var_run_name}")) %>% 
  list.files(full.names = TRUE) %>% 
  map(data.table::fread) %>% 
  reduce(rbind)

####

df_area_changes %>% 
  mutate(
    percent_change = (area_change / current_size) * 100
  ) %>% 
  
  # df_area_changes %>% 
  ggplot() +
  aes(x = taxon, y = percent_change, fill = range) +
  geom_boxplot() +
  facet_wrap(~ scenario)

df_area_changes %>% 
  mutate(
    percent_change = (area_change / current_size) * 100
  ) %>% 
  group_by(
    taxon, range, scenario
  ) %>% 
  summarize(
    mean = mean(percent_change),
    median = median(percent_change),
    sd = sd(percent_change)
  ) %>% 
  ungroup() %>% 
  ggplot() +
  aes(
    x = taxon, y = mean, fill = scenario) +
  geom_col(position = "dodge") +
  theme_minimal() +
  # coord_flip() +
  facet_wrap(~ range)


# Updated figure

utm_33n <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Process alpine ranges
val_area_alps <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("European_Alps") %>% 
  st_read() %>% 
  transmute(
    range = "Alps"
  ) %>% 
  st_transform(crs = utm_33n) %>% 
  mutate(
    area = units::set_units(st_area(.), "km2")
  ) %>% 
  st_set_geometry(NULL) %>% 
  pull(area) %>% 
  as.numeric()


val_area_dinarides <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinaric_") %>% 
  st_read() %>% 
  transmute(
    range = "Dinarides"
  ) %>% 
  st_transform(crs = utm_33n) %>% 
  mutate(
    area = units::set_units(st_area(.), "km2")
  ) %>% 
  st_set_geometry(NULL) %>% 
  pull(area) %>% 
  as.numeric()


df_alpz <- df_area_changes %>% 
  mutate(
    percent_change = (area_change / current_size) * 100 
  ) %>% 
  filter(range == "Alps") %>%
  group_by(
    taxon, range, scenario
  ) %>% 
  summarize(
    mean = mean(percent_change),
    median = median(percent_change),
    sd = sd(percent_change)
  ) %>% 
  ungroup() %>%
  mutate(
    value_relative_post_summary = (val_area_alps / val_area_dinarides) * mean
  ) 

df_dinaridez <- df_area_changes %>% 
  mutate(
    percent_change = (area_change / current_size) * 100 
  ) %>% 
  filter(range == "Dinarides") %>%
  group_by(
    taxon, range, scenario
  ) %>% 
  summarize(
    mean = mean(percent_change),
    median = median(percent_change),
    sd = sd(percent_change)
    # value_relative_pre_summary = (val_area_dinarides / val_area_alps) * mean
    
  ) %>% 
  ungroup() %>%
  mutate(
    value_relative_post_summary = (val_area_dinarides / val_area_alps) * mean
  ) 

fig3_normalized <- rbind(
  df_alpz,
  df_dinaridez
)  %>% 
  ggplot() +
  aes(
    x = taxon, y = value_relative_post_summary, fill = scenario) +
  geom_col(position = "dodge") +
  # geom_errorbar(
  #   position = position_dodge2(width = 0.5, padding = 1.5),
  #   aes(ymin = lower_sem, ymax = upper_sem)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  # scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-100, 100)) +
  ggthemes::scale_fill_tableau() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 32, family = "Helvetica"),
    strip.text = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    # legend.title = element_text(size = 28, family = "Helvetica-Narrow"),
    legend.title = element_blank(),
    legend.text = element_text(size = 18, family = "Helvetica-Narrow"),
    axis.text.x = element_markdown(size = 12),
    axis.title.x = element_text(size = 18),
    axis.text.y = element_markdown(size = 14),
    axis.title.y = element_text(size = 18)
  ) +
  labs(y = "Values normalized per mountain range") +
  facet_wrap(~ range)





## Set filename for the output filemap
fig3_normalized_bars_outname <- str_glue("{folder_path_figures}/Fig3_results_barchart_normalized_{var_run_name}_{Rahat::today()}.png")

## Final file version for submission stored in 600 dpi's

ggsave(fig3_normalized_bars_outname, fig3_normalized,
       dpi = 300, height = 9, width = 14)  

  
######

# Final version -----------------------------------------------------------

df_combined_ranges <- df_area_changes %>% 
  mutate(
    percent_change = (area_change / current_size) * 100 
  ) %>% 
  group_by(
    taxon, scenario
  ) %>% 
  summarize(
    mean = mean(percent_change),
    median = median(percent_change),
    sd = sd(percent_change),
    sem = sd / sqrt(n())
  ) %>% 
  ungroup() %>%
  # filter(range == "Dinarides") %>% 
  mutate(
    upper_sem = mean + sem,
    lower_sem = mean - sem,
    taxon = case_when(
      taxon == "points_all" ~ "Species model",
      taxon == "points_atra" ~ "Subspecies model *atra*",
      taxon == "points_prenjensis" ~ "Subspecies model *prenjensis*",
    ),
    range = "Entire range",
    scenario = ifelse(scenario == "rcp26", "RCP2.6", "RCP8.5")
  ) %>% 
  transmute(
    taxon, range, scenario, mean, lower_sem, upper_sem
  )


p_fig3_bars <-
  df_area_changes %>% 
  mutate(
    percent_change = (area_change / current_size) * 100 
  ) %>% 
  group_by(
    taxon, range, scenario
  ) %>% 
  summarize(
    mean = mean(percent_change),
    median = median(percent_change),
    sd = sd(percent_change),
    sem = sd / sqrt(n())
  ) %>% 
  ungroup() %>%
  # filter(range == "Dinarides") %>% 
  mutate(
    upper_sem = mean + sem,
    lower_sem = mean - sem,
    value_relative = (val_area_dinarides / val_area_alps) * mean,
    taxon = case_when(
    taxon == "points_all" ~ "Species model",
    taxon == "points_atra" ~ "Subspecies model *atra*",
    taxon == "points_prenjensis" ~ "Subspecies model *prenjensis*",
    ),
    scenario = ifelse(scenario == "rcp26", "RCP2.6", "RCP8.5")
  ) %>% 
  #### Added filter to remove results where taxon is extrapolated spatially
  filter(!(range == "Alps" & taxon == "Subspecies model *prenjensis*")) %>% 
  filter(!(range == "Dinarides" & taxon == "Subspecies model *atra*")) %>% 
    transmute(
      taxon, range, scenario, mean, lower_sem, upper_sem
    ) %>%
    rbind(df_combined_ranges) %>% 
  ggplot() +
  aes(
    x = taxon, y = mean, fill = scenario) +
  geom_col(position = "dodge") +
  geom_errorbar(
    position = position_dodge2(width = 0.5, padding = 1.5),
    aes(ymin = lower_sem, ymax = upper_sem)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-100, 100)) +
  # coord_flip() +
  labs(
    # title = "Relative change in the extent of suitable climate",
    # subtitle = "Values are relative to the size of the alpine territory",
    fill = "Future climate scenario",
    y = "Change in the extent of suitable climate relative to current climate (%)",
    x = "Study unit"
       ) +
  facet_wrap(~ range, scales = "free_x") +
  ggthemes::scale_fill_tableau() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 32, family = "Helvetica"),
    strip.text = element_text(size = 24),
    panel.grid.major.x = element_blank(),
    # legend.title = element_text(size = 28, family = "Helvetica-Narrow"),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18, family = "Helvetica-Narrow"),
    axis.text.x = element_markdown(size = 14),
    axis.title.x = element_text(size = 18),
    axis.text.y = element_markdown(size = 16),
    axis.title.y = element_text(size = 18)
  )


## Set filename for the output filemap
fig3_bars_outname <- str_glue("{folder_path_figures}/Fig3_results_barchart_pct_{var_run_name}_{Rahat::today()}.png")

## Final file version for submission stored in 600 dpi's

ggsave(fig3_bars_outname, p_fig3_bars,
       dpi = 300, height = 10, width = 18)  


####
