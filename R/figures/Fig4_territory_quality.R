pacman::p_load(Rahat, tidyverse, tictoc, janitor, sf, raster)
library(terra)
library(ggtext)
library(patchwork)
library(cowplot)
library(here)

source(here::here("R", "main_setup.R"))



####


range_alps <- here("data", "mnt_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("European_Alps") %>% 
  st_read() %>% 
  transmute(
    range = "Alps"
  )

range_dinarides <- here("data", "mnt_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinaric_") %>% 
  st_read() %>% 
  transmute(
    range = "Dinarides"
  )


ranges_mountains <- rbind(
  range_dinarides,
  range_alps
)


utm_33n <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

area_dinarides <- ranges_mountains %>% 
  st_transform(crs = utm_33n) %>% 
  mutate(
    area = st_area(.) %>% units::set_units("km2")
  ) %>% 
  st_set_geometry(NULL) %>%
  filter(range == "Dinarides") %>% 
  pull(area) %>% 
  as.numeric()

area_alps <- ranges_mountains %>% 
  st_transform(crs = utm_33n) %>% 
  mutate(
    area = st_area(.) %>% units::set_units("km2")
  ) %>% 
  st_set_geometry(NULL) %>% 
  filter(range == "Alps") %>% 
  pull(area) %>% 
  as.numeric()

area_combined <- ranges_mountains %>% 
  st_transform(crs = utm_33n) %>% 
  mutate(
    area = st_area(.) %>% units::set_units("km2")
  ) %>% 
  st_set_geometry(NULL) %>% 
  pull(area) %>% 
  as.numeric() %>% 
  sum()


my_i <- crossing(
  taxon = c("points_all", "points_atra", "points_prenjensis"),
  scenario = c("current", "rcp26", "rcp85")
)

output_folder <- str_glue("{folder_path_projections}/territory_quality_2023_test")
dir.create(output_folder, showWarnings = FALSE)



calculate_territory <- function(species, scenario, classes = 10)
{
  r <- "/vol/milkunarc/mcengic/Projects/atra_climate_model/runs/climate_impact_VIF/projections/consensus_manual/" %>% 
    list.files(full.names = TRUE, pattern = "tif") %>% 
    str_subset(species) %>% 
    str_subset(scenario) %>% 
    raster()
  
  r_values <- getValues(r)
  
  raster_values_rcl <- r_values %>% 
    as_tibble() %>% 
    mutate(
      value_rcl_10_inc = case_when(value <= 0.1 ~ "0-10%",
                                   value > 0.1 & value <= 0.2 ~ "10-20%",
                                   value > 0.2 & value <= 0.3 ~ "20-30%",
                                   value > 0.3 & value <= 0.4 ~ "30-40%",
                                   value > 0.4 & value <= 0.5 ~ "40-50%",
                                   value > 0.5 & value <= 0.6 ~ "50-60%",
                                   value > 0.6 & value <= 0.7 ~ "60-70%",
                                   value > 0.7 & value <= 0.8 ~ "70-80%",
                                   value > 0.8 & value <= 0.9 ~ "80-90%",
                                   value > 0.9 & value <= 1 ~ "90-100%"
      ),
      value_rcl_10_inc_num = case_when(
        value == 0.06 ~ 0,
        value > 0.06 & value <= 0.1 ~ 1,
        value > 0.1 & value <= 0.2 ~ 2,
        value > 0.2 & value <= 0.3 ~ 3,
        value > 0.3 & value <= 0.4 ~ 4,
        value > 0.4 & value <= 0.5 ~ 5,
        value > 0.5 & value <= 0.6 ~ 6,
        value > 0.6 & value <= 0.7 ~ 7,
        value > 0.7 & value <= 0.8 ~ 8,
        value > 0.8 & value <= 0.9 ~ 9,
        value > 0.9 & value <= 1 ~ 10
      ),
      value_rcl_20_inc = case_when(value <= 0.2 ~ "0-20%",
                                   value > 0.2 & value <= 0.4 ~ "20-40%",
                                   value > 0.4 & value <= 0.6 ~ "40-60%",
                                   value > 0.6 & value <= 0.8 ~ "60-80%",
                                   value > 0.8 & value <= 1 ~ "80-100%"
      ),
      value_rcl_20_inc_num = case_when(
        
        value <= 0.06 ~ 0,
        value > 0.06 & value <= 0.2 ~ 2,
        value > 0.2 & value <= 0.4 ~ 4,
        value > 0.4 & value <= 0.6 ~ 6,
        value > 0.6 & value <= 0.8 ~ 8,
        value > 0.8 & value <= 1 ~ 10
      )
    )
  
  if (classes == 10)
  {
    raster_rcl <- rast(setValues(r, raster_values_rcl$value_rcl_10_inc_num))
  }
  if (classes == 5)
  {
    raster_rcl <- rast(setValues(r, raster_values_rcl$value_rcl_20_inc_num))
  }
  
  
  
  r_alps <- terra::crop(raster_rcl, range_alps, mask = TRUE)
  r_dinarides <- terra::crop(raster_rcl, range_dinarides, mask = TRUE)
  r_combined <- terra::crop(raster_rcl, ranges_mountains, mask = TRUE)
  
  p_alps <- r_alps %>% 
     as.polygons(dissolve = TRUE, na.rm = TRUE, trun = TRUE, values = TRUE) %>% 
    st_as_sf() %>%
    st_transform(crs = utm_33n) %>%
    mutate(
      area = units::set_units(st_area(.), "km2"),
      range = "Alps",
      taxon = species,
      scenario = scenario
    )
  
  p_dinarides <- r_dinarides %>% 
    as.polygons(dissolve = TRUE, na.rm = TRUE, trun = TRUE, values = TRUE) %>% 
    st_as_sf() %>%
    st_transform(crs = utm_33n) %>%
    mutate(
      area = units::set_units(st_area(.), "km2"),
      range = "Dinarides",
      taxon = species,
      scenario = scenario
    )
  
  p_combined <- r_combined %>% 
     as.polygons(dissolve = TRUE, na.rm = TRUE, trun = TRUE, values = TRUE) %>% 
    st_as_sf() %>%
    st_transform(crs = utm_33n) %>%
    mutate(
      area = units::set_units(st_area(.), "km2"),
      range = "Combined",
      taxon = species,
      scenario = scenario
    )
  
  if (classes == 10)
  {
    st_write(p_alps, here("results", "territory_quality_gpkg", str_glue("polygon_alps_{species}_{scenario}_10inc.gpkg")), append = FALSE)
    st_write(p_dinarides, here("results", "territory_quality_gpkg", str_glue("polygon_dinarides_{species}_{scenario}_10inc.gpkg")), append = FALSE)
    st_write(p_combined, here("results", "territory_quality_gpkg", str_glue("polygon_combined_{species}_{scenario}_10inc.gpkg")), append = FALSE)
  }
  if (classes == 5)
  {
    st_write(p_alps, here("results", "territory_quality_gpkg", str_glue("polygon_alps_{species}_{scenario}_20inc.gpkg")), append = FALSE)
    st_write(p_dinarides, here("results", "territory_quality_gpkg", str_glue("polygon_dinarides_{species}_{scenario}_20inc.gpkg")), append = FALSE)
    st_write(p_combined, here("results", "territory_quality_gpkg", str_glue("polygon_combined_{species}_{scenario}_20inc.gpkg")), append = FALSE)
  }
}

for (i in 1:nrow(my_i))
{
  
  print(i)
  species = my_i %>% 
    slice(i) %>% 
    pull(taxon)
  
  scenario = my_i %>% 
    slice(i) %>% 
    pull(scenario)
  
  tic("10 increments")
  calculate_territory(species = species, scenario = scenario, classes = 10)
  toc()
 
}


loadd <- function(d)
{
  d <- st_read(d) %>% 
    st_set_geometry(NULL)
  d
}



####
n_classes = "10inc"

  data_combined <- here("results", "territory_quality_gpkg") %>% 
    # Local path
    list.files(full.names = TRUE, pattern = n_classes) %>% 
    map(loadd) %>% 
    reduce(bind_rows)
  
  write_csv(data_combined, here::here(str_glue("territory_quality_{today()}_{n_classes}.csv")))




####

data_combined <- str_glue("/home/mirza/milkunK/Projects/atra_climate_model/territory_quality_{today()}_{n_classes}.csv") %>% 
  read_csv()

data_combined <- str_glue("/home/mirza/milkunK/Projects/atra_climate_model/territory_quality_21_08_2023_20inc.csv") %>% 
  read_csv()


data_combined_cleaned <- data_combined %>% 
  transmute(
    range,
    taxon = case_when(
      taxon == "points_all" ~ "Species model",
      taxon == "points_atra" ~ "Subspecies model<br>*atra*",
      taxon == "points_prenjensis" ~ "Subspecies model<br>*prenjensis*",
    ),
    scenario,
    category = sum,
    area,
    total_area = case_when(
      range == "Alps" ~ area_alps,
      range == "Dinarides" ~ area_dinarides,
      range == "Combined" ~ area_combined,
    )
  ) %>% 
  mutate(
    range = ifelse(range == "Combined", "Combined extent", range)
  ) %>% 
  group_by(range, taxon, scenario) %>% 
  filter(category != 0) %>% 
  mutate(
    area_percent = round((area / total_area), 4) * 100
  ) %>% 
  ungroup()



#### Plot
data_combined_cleaned %>% 
  mutate(
    category = as.factor(category) %>% 
      fct_relevel(c("2", "4", "6", "8", "10") %>% rev()),
    range = fct_relevel(range, c("Combined extent", "Alps", "Dinarides")),
    scenario = case_when(
      scenario == "current" ~ "Current climate",
      scenario == "rcp26" ~ "Future climate (RCP2.6)",
      scenario == "rcp85" ~ "Future climate (RCP8.5)"
    )
  ) %>%
  #### Added filter to remove results where taxon is extrapolated spatially
  filter(!(range == "Alps" & taxon == "Subspecies model *prenjensis*")) %>% 
  filter(!(range == "Dinarides" & taxon == "Subspecies model *atra*")) %>% 
  ggplot() +
  aes(
    x = scenario, y = area_percent, fill = category
  ) +
  geom_col(
    color = "grey50"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  # scale_fill_viridis_d(direction = -1) +
  scale_fill_manual(values = rev(c("#ffffcc", "#c2e699", "#78c679","#31a354","#006837"))) +
  # facet_wrap(~ taxon) 
  labs(
    y = "% of total mountain range area",
    x = "Study unit"
  ) +
  facet_wrap(vars(range, taxon), nrow = 1, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 18, family = "Helvetica-Narrow"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_markdown(size = 14),
    axis.title.x = element_text(size = 18),
    axis.text.y = element_markdown(size = 16),
    axis.title.y = element_text(size = 18),
    strip.text = element_markdown(size = 16)
  )


data_combined_plotting <- data_combined_cleaned %>% 
  mutate(
    
    category = as.factor(category) %>% 
      fct_relevel(
        c("2", "4", "6", "8", "10") %>%

          rev()),
    range = fct_relevel(range, c("Combined extent", "Alps", "Dinarides")),
    scenario = case_when(
      scenario == "current" ~ "Current",
      scenario == "rcp26" ~ "Future<br>(RCP2.6)",
      scenario == "rcp85" ~ "Future<br>(RCP8.5)"
    )
  ) %>%
  #### Added filter to remove results where taxon is extrapolated spatially
  filter(!(range == "Alps" & taxon == "Subspecies model<br>*prenjensis*")) %>% 
  filter(!(range == "Dinarides" & taxon == "Subspecies model<br>*atra*")) 


brewer_10_color_palette <- c("white" ,'#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')
labels_10_colors <- c("1-10%", "11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%")


brewer_5_color_palette <- c('#ffffe5', '#d9f0a3', '#78c679', '#238443','#004529')
labels_5_colors <- c("1-20%", "21-40%", "41-60%", "61-80%", "81-100%")


p1 <- data_combined_plotting %>% 
  filter(range == "Combined extent") %>% 
  ggplot() +
  aes(
    x = scenario, y = area_percent, fill = category
  ) +
  geom_col(
    color = "grey50", width = 0.75
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), breaks = seq(0, 100, 10)) +
  # scale_fill_viridis_d(direction = -1) +
  scale_fill_manual(values = rev(brewer_5_color_palette)) +
  # facet_wrap(~ taxon) 
  labs(
    title = "a) Combined extent (Alps + Dinarides)",
    y = "% of study area",
    x = NULL
  ) +
  facet_wrap(~ taxon, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 18, family = "Helvetica-Narrow"),
    plot.background = element_rect(color = "transparent"),
    plot.title = element_text(size = 24, family = "Helvetica", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_markdown(angle = 35, hjust = 1, vjust = 1.4, size = 14),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.text.y = element_markdown(size = 18, family = "Helvetica"),
    axis.title.y = element_text(size = 18, family = "Helvetica"),
    strip.text = element_markdown(size = 18, family = "Helvetica")
  )
# p1

p2 <- data_combined_plotting %>% 
  filter(range == "Alps") %>%
  ggplot() +
  aes(
    x = scenario, y = area_percent, fill = category
  ) +
  geom_col(
    color = "grey50", width = 0.5
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), breaks = seq(0, 100, 10)) +
  # scale_fill_viridis_d(direction = -1) +
  scale_fill_manual(values = rev(brewer_5_color_palette),
                    labels = rev(labels_5_colors),
                    guide = guide_legend(nrow = 3)
  ) +
  # facet_wrap(~ taxon) 
  labs(
    title = "b) Alps",
    y = NULL,
    x = NULL,
    fill = "Consensus score:"
  ) +
  facet_wrap(~ taxon, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 18, family = "Helvetica-Narrow"),
    plot.background = element_rect(color = "transparent"),
    plot.title = element_text(size = 24, family = "Helvetica", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_markdown(angle = 35, hjust = 1, vjust = 1.4, size = 14),
    axis.title.x = element_text(size = 18),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 18),
    strip.text = element_markdown(size = 18, family = "Helvetica")
  )

#

p3 <- data_combined_plotting %>% 
  filter(range == "Dinarides") %>% 
  ggplot() +
  aes(
    x = scenario, y = area_percent, fill = category
  ) +
  geom_col(
    color = "grey50", width = 0.5
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), breaks = seq(0, 100, 10)) +
  # scale_fill_viridis_d(direction = -1) +
  # scale_fill_manual(values = rev(c("#ffffcc", "#c2e699", "#78c679","#31a354","#006837"))) +
  scale_fill_manual(values = rev(brewer_5_color_palette)) +
  # facet_wrap(~ taxon) 
  labs(
    title = "c) Dinarides",
    y = NULL,
    x = NULL
  ) +
  facet_wrap(~ taxon, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 18, family = "Helvetica-Narrow"),
    plot.background = element_rect(color = "transparent"),
    plot.title = element_text(size = 24, family = "Helvetica", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_markdown(angle = 35, hjust = 1, vjust = 1.4, size = 14),
    axis.title.x = element_text(size = 18),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 18),
    strip.text = element_markdown(size = 18, family = "Helvetica")
  )


my_legend_plot <- p2 +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 22, margin = margin(r = 17.5), family = "Helvetica-Narrow"),
    legend.title = element_text(size = 22, margin = margin(r = 5), family = "Helvetica")
  ) +
  scale_fill_manual(values = brewer_5_color_palette,
                    labels = labels_5_colors,
                    guide = guide_legend(nrow = 1)
  )


my_legend <- get_legend(my_legend_plot)
# ggdraw(my_legend)

p_combined <- ggdraw(my_legend) / (p1 | p2 | p3) +
  plot_layout(heights = c(1, 4)) #+

# 
ggsave(here("results", "figure4_territory_quality.png"),
       p_combined,
       dpi = 300, height = 11, width = 22)    

