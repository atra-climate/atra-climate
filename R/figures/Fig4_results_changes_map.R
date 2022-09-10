# Figure 4 – Map showing the spatial distribution of categories, only for 2 subspecies


pacman::p_load(Rahat, tidyverse, here, tictoc, janitor, sf, raster, matrixStats)

library(ggthemes)
library(viridis)
library(ggnewscale)
library(patchwork)



# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))
  

utm_33n <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

atra_raw_sf <- st_read(var_path_species_data)

atra_thinned_sf <- atra_raw_sf %>% 
  st_transform(crs = utm_33n)
#
# Calculate differences ####
r_differences_stack <- folder_path_projections_manual_ensembles %>% 
  list.files(full.names = TRUE, pattern = "changes") %>%
  stack()

# Map differences

DEM_large <- here::here("results", "DEM_aggregated.tif") %>% 
  raster()

dinaric_countries <- here::here("results", "procext_countries.gpkg") %>% 
  st_read() %>% 
  st_transform(crs = utm_33n)

####
dem_gg2 <- DEM_large %>%
  projectRaster(crs = utm_33n) %>%
  raster_to_gg()


## Process alpine ranges
range_alps <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("European_Alps") %>% 
  st_read() %>% 
  transmute(
    range = "Alps"
  )


range_dinarides <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinaric_") %>% 
  st_read() %>% 
  transmute(
    range = "Dinarides"
  )

#
alpine_ranges <- rbind(range_alps, range_dinarides)

make_changes_map_data <- function(taxon, scenario, mask = FALSE)
{
  my_names <- r_differences_stack %>% 
    names() %>% 
    str_subset(taxon) %>% 
    str_subset(scenario)
  
  layer_future <- r_differences_stack %>% 
    subset(my_names)
  
  layer_current <- folder_path_projections_manual_ensembles %>% 
    list.files(full.names = TRUE, pattern = "ensemble_current") %>% 
    str_subset(taxon) %>% 
    raster()
  
  my_stack <- stack(
    layer_current,
    layer_future
  )
  
  df_combined_vals <- getValues(my_stack)
  
  my_valz <- df_combined_vals %>% 
    as.data.frame() %>% 
    rename(
      current = 1,
      future_changes = 2
    ) %>% 
    mutate(
      # value = case_when(
      #   current == 0 & future_changes == 0 ~ NA_real_,
      #   TRUE ~ future_changes
      # )
      value = case_when(
        current == 0 & future_changes == 0 ~ 0, #"to remove",
        current >= 0.5 & future_changes < 0 ~ 1, #"Suitable, decreasing",
        current >= 0.5 & future_changes > 0 ~ 2, #"Suitable, increasing",
        current >= 0.5 & future_changes == 0 ~ 3, #"Suitable, stable",
        current < 0.5 & future_changes < 0 ~ 4, #"Unsuitable, decreasing",
        current < 0.5 & future_changes > 0 ~ 5, #"Unsuitable, increasing",
        (current < 0.5 & current != 0) & future_changes == 0 ~ 6 #"Unsuitable, stable"
      ),
      value = ifelse(value == 0, NA_real_, value)
      # current == 0 & future_changes == 0 ~ "to remove",
      # current >= 0.5 & future_changes < 0 ~ "Suitable, decreasing",
      # current >= 0.5 & future_changes > 0 ~ "Suitable, increasing",
      # current >= 0.5 & future_changes == 0 ~ "Suitable, stable",
      # current < 0.5 & future_changes < 0 ~ "Unsuitable, decreasing",
      # current < 0.5 & future_changes > 0 ~ "Unsuitable, increasing",
      # (current < 0.5 & current != 0) & future_changes == 0 ~ "Unsuitable, stable"
    ) %>% 
    pull(value)
  
  
  layer_new <- setValues(layer_current, my_valz)
  
  if (mask) 
  {
    layer_new <- mask(layer_new, alpine_ranges)
  }
  
  my_df_values_new <- layer_new %>% 
    projectRaster(crs = utm_33n, method = "ngb") %>%
    raster_to_gg() %>% 
    rename(
      value = 1
    )
  
  return(my_df_values_new)
  
}

####

make_changes_map <- function(data)
{
  
  pp <- ggplot() +
    geom_tile(data = dem_gg2, aes(x = x, y = y, fill = DEM_aggregated), alpha = 0.5, show.legend = FALSE) +
    scale_fill_gradient(low = "#f0f0f0", high = "#525252") + # Grey
    #####################
  new_scale_fill() +
    geom_tile(
      data = data,
      aes(x = x, y = y, fill = as.factor(value)),
      show.legend = T) +
    ggthemes::scale_fill_tableau() +
    
    #############33
    # new_scale_fill() +
    # geom_tile(
    #   data = my_df_values3,
    #   alpha = 0.1,
    #   aes(x = x, y = y, fill = as.factor(value)),
    #   show.legend = T) +
    # ggthemes::scale_fill_tableau() +
    ##############
  geom_sf(data = dinaric_countries, fill = NA, color = "grey60", size = 0.5) +
    theme(
      plot.tag = element_text(size = 22),
      legend.position = "none",
      legend.text = element_text(size = 18, family = "Helvetica", color = "grey10"),
      legend.title = element_blank(),
      # legend.background = element_blank(),
      # legend.key = element_blank(),
      axis.ticks = element_line(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 14, 
                               # family = "mono",
                               color = "grey40"),
      # panel.grid.major = element_line(color = "grey50", linetype = 2, size = 0.9),
      panel.grid.major = element_line(colour = "grey60", linetype = "dotted", size = 0.75),
      panel.background = element_rect(fill = "transparent")
    )
  
  return(pp)
  
}


my_map_c <- make_changes_map_data(taxon = "points_atra", scenario = "rcp26", mask = TRUE) %>% 
  make_changes_map() + 
  labs(title = "a) Subspecies model *atra* - RCP2.6") +
  theme(
    plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
    plot.title = element_markdown(size = 22)     
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank()
  ) +
  coord_sf(
    label_graticule = "NW",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))

#####################3
my_map_d <- make_changes_map_data(taxon = "points_atra", scenario = "rcp85", mask = TRUE) %>% 
  make_changes_map() +
  labs(title = "b) Subspecies model *atra* - RCP8.5") +
  theme(
    plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
    plot.title = element_markdown(size = 22)  
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank()
  ) +
  coord_sf(
    label_graticule = "NE",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))



####
my_map_e <- make_changes_map_data(taxon = "points_prenjensis", scenario = "rcp26", mask = TRUE) %>% 
  make_changes_map() + 
  labs(title = "c) Subspecies model *prenjensis* - RCP2.6") +
  theme(
    plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
    plot.title = element_markdown(size = 22)  
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank()
  ) +
  coord_sf(
    label_graticule = "SW",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.03, 
             st_bbox(atra_thinned_sf)[3] * 1.03),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.01,
             st_bbox(atra_thinned_sf)[4] * 1.01))


my_map_f <- make_changes_map_data(taxon = "points_prenjensis", scenario = "rcp85", mask = TRUE) %>% 
  make_changes_map() + 
  labs(title = "d) Subspecies model *prenjensis* - RCP8.5") +
  theme(
    plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
    plot.title = element_markdown(size = 22)  
    # axis.text.y = element_blank()
  ) +
  coord_sf(
    label_graticule = "SE",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.03, 
             st_bbox(atra_thinned_sf)[3] * 1.03),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.01,
             st_bbox(atra_thinned_sf)[4] * 1.01))


##################################################
##################################################
#### map with current stuff ####


vecP <- c(1, 2, 3, 4, 5, 6)
labP <- c("Suitable, decreasing", "Suitable, increasing", "Suitable, stable",
          "Unsuitable, decreasing", "Unsuitable, increasing", "Unsuitable, stable")

my_plot_legend <- cbind(labP, vecP) %>% 
  as.data.frame() %>% 
  ggplot() +
  aes(x = vecP, fill = labP) +
  geom_bar()  +
  ggthemes::scale_fill_tableau() +
  theme(
    legend.position = "bottom",
    legend.spacing.x = grid::unit(3, "pt"),
    legend.margin = margin(l = 0.4, unit='cm'),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26, family = "Helvetica", vjust = -1, color = "grey10")
  )+
  labs(fill = NULL, color = NULL)


##

my_legend <- cowplot::get_legend(my_plot_legend)
my_map_combined <- 
  (my_map_c | my_map_d) /
  (my_map_e | my_map_f) /
  my_legend +
  theme(
    plot.tag = element_text(size = 20),
    plot.title = element_text(size = 32)
) +
  patchwork::plot_layout(
    heights = c(4, 4, 0.8)
    )


## Set filename for the output filemap
fig4_map_outname <- str_glue("{folder_path_figures}/Fig4_changes_map_{var_run_name}_{Rahat::today()}_{format(Sys.time(), paste0('%H-%M-%S'))}.png")

## Final file version for submission stored in 600 dpi's

tic("making map")
ggsave(fig4_map_outname, my_map_combined,
       dpi = 200, height = 15, width = 16)  
toc()
