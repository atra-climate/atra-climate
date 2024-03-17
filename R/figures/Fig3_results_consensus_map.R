# Figure 3 – map with model consensus 
# with all 3 study units, species-level in the bottom row


pacman::p_load(Rahat, tidyverse, here, tictoc, janitor, sf, raster, matrixStats)

library(ggthemes)
library(viridis)
library(ggnewscale)
library(patchwork)
library(cowplot)
library(ggtext)
library(ggspatial)

# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))


source(here("R", "functions", "map_functions.R"))
####

# Should raster be masked to alpine ranges

is_masked = TRUE

if (is_masked)
{
  mask_filename = "_masked"
} else {
  mask_filename = ""
}

utm_33n <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Make map ----------------------------------------------------------------

# Load data ---------------------------------------------------------------

DEM_large <- here::here("results", "DEM_aggregated.tif") %>% 
  raster()
# Make map ----------------------------------------------------------------

#### Load species data
atra_raw_sf <- st_read(var_path_species_data)

atra_thinned_sf <- atra_raw_sf %>% 
  st_transform(crs = utm_33n)
#            

dinaric_countries <- here::here("results", "procext_countries.gpkg") %>% 
  st_read() %>% 
  st_transform(crs = utm_33n)


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


####
dem_gg2 <- DEM_large %>%
  projectRaster(crs = utm_33n) %>%
  raster_to_gg()

###########################3
# Load commitee averages and continous projections

r_ensembles_stack <- folder_path_projections_manual_ensembles %>%
   list.files(full.names = TRUE, pattern = "ensemble") %>%
  stack()



convert_for_plot <- function(x, name = "", mask = FALSE)
{
  
  if(mask)
  {
    masked = "_masked"
  } else {
    masked = ""
  }
  
  outname <- here("tmp", str_glue("{name}_{var_run_name}{masked}.csv"))

  if (!file.exists(outname))
  {
    tic(name)
    
    x <- x %>%
      raster::subset(name) 
  
      if (mask)
      {
        x <- raster::mask(x, alpine_ranges)
      }
      
      
      x <- x %>% 
      projectRaster(crs = utm_33n) %>%
      raster_to_gg() %>%
      rename(
        value = 1
      ) %>%
      mutate(
        name = name
      ) %>%
      separate(
        name, into = c("A", "period", "B", "taxon", "scenario"), sep = "_"
      ) %>%
      mutate(
        scenario = ifelse(period == "current", "current", scenario)
      ) %>%
      dplyr::select(
        -A, -B, -period
      )
    
    write_csv(x, outname)
    
    toc()  
  } else {
    print("Loading existing file")
    x <- data.table::fread(outname)
    
  }
  
  return(x)
  
}

#### Make map stacks
tic("stacking maps")

my_map_data_sp26 <- convert_for_plot(r_ensembles_stack, "ensemble_future_points_all_rcp26", mask = is_masked)
my_map_data_sp85 <- convert_for_plot(r_ensembles_stack, "ensemble_future_points_all_rcp85", mask = is_masked)
my_map_data_south26 <- convert_for_plot(r_ensembles_stack, "ensemble_future_points_prenjensis_rcp26", mask = is_masked)
my_map_data_south85 <- convert_for_plot(r_ensembles_stack, "ensemble_future_points_prenjensis_rcp85", mask = is_masked)
my_map_data_north26 <- convert_for_plot(r_ensembles_stack, "ensemble_future_points_atra_rcp26", mask = is_masked)
my_map_data_north85 <- convert_for_plot(r_ensembles_stack, "ensemble_future_points_atra_rcp85", mask = is_masked)
#Current
my_map_data_spcurrent <- convert_for_plot(r_ensembles_stack, "ensemble_current_points_all", mask = is_masked)
my_map_data_northcurrent <- convert_for_plot(r_ensembles_stack, "ensemble_current_points_atra", mask = is_masked)
my_map_data_southcurrent <- convert_for_plot(r_ensembles_stack, "ensemble_current_points_prenjensis", mask = is_masked)

toc()

#

make_ensemble_map <- function(data)
{
  p_mapp <- ggplot() +
    geom_tile(data = dem_gg2, aes(x = x, y = y, fill = DEM_aggregated), alpha = 0.5, show.legend = FALSE) +
    scale_fill_gradient(low = "#f0f0f0", high = "#525252") + # Grey
    new_scale_fill() +
    geom_tile(
      data = data,
      aes(x = x, y = y, fill = value),
      show.legend = TRUE) +
   
    scale_fill_gradient2(
     
      low = "#f7fcb9",
      mid = "#addd8e",
      high = "#31a354",
      midpoint = 0.5,
      limits = c(0, 1)
    ) + 
    geom_sf(data = dinaric_countries, fill = NA, color = "grey60", size = 0.5) +
    
    theme(
      plot.tag = element_text(size = 22),
      plot.title = element_text(size = 14.5),
      legend.position = "none",
      legend.text = element_text(size = 20, family = "Helvetica", color = "grey10"),
      # legend.title = element_blank(),
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

  
  return(p_mapp)
}

# Make maps ---------------------------------------------------------------
# Atra subspecies

my_map_northcurrent <- my_map_data_northcurrent %>% 
  filter(value != 0) %>% 
  make_ensemble_map() +
  labs(title = "a) *Salamandra atra atra* -  current climate") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "NW",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))  +
  annotation_north_arrow(which_north = "true", style = north_arrow_minimal(fill = "grey40", line_col = "grey40"), location = "tl", pad_x = unit(0.1, "cm"))



my_map_north26 <- my_map_data_north26 %>% 
  filter(value != 0) %>% 
  make_ensemble_map() +
  labs(title = "b) *Salamandra atra atra* - RCP2.6") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "N",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))


my_map_north85 <- my_map_data_north85 %>% 
  filter(value != 0) %>% 
  make_ensemble_map()  +
  labs(title = "c) *Salamandra atra atra* - RCP8.5") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "NE",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015)) 

##################################################
# prenjensis subspecies

my_map_southcurrent <- my_map_data_southcurrent %>% 
  filter(value != 0) %>% 
  make_ensemble_map() +
  labs(title = "d) *Salamandra atra prenjensis* - current climate") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "W",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))


my_map_south26 <- my_map_data_south26 %>% 
  filter(value != 0) %>% 
  make_ensemble_map()  +
  labs(title = "e) *Salamandra atra prenjensis* - RCP2.6") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))

my_map_south85 <- my_map_data_south85 %>% 
  filter(value != 0) %>% 
  make_ensemble_map() +
  labs(title = "f) *Salamandra atra prenjensis* - RCP8.5") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "E",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))

###############################
# Species level model

my_map_spcurrent <- my_map_data_spcurrent %>% 
  filter(value != 0) %>% 
  make_ensemble_map() +
  labs(title = "g) Species model - current climate") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "SW",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))


my_map_sp26 <- my_map_data_sp26 %>% 
  filter(value != 0) %>% 
  make_ensemble_map() +
  labs(title = "h) Species model - RCP2.6") +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "S",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))

my_map_sp85 <- my_map_data_sp85 %>% 
  filter(value != 0) %>% 
  make_ensemble_map() +
  labs(title = "i) Species model - RCP8.5") +
  theme(
    plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
    plot.title = element_markdown()
  ) +
  coord_sf(
    label_graticule = "SE",
    xlim = c(st_bbox(atra_thinned_sf)[1] * 1.2,
             st_bbox(atra_thinned_sf)[3] * 1.1),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.02,
             st_bbox(atra_thinned_sf)[4] * 1.015))  +
  annotation_scale(text_cex = 1.25, 
                   text_col = "grey40",
                   line_col = "grey40",
                   location = "br",
                   style = "ticks",
                   height = unit(0.3, "cm"),
                   pad_x = unit(0.5, "cm"),
                   pad_y = unit(0.3, "cm"),
                   line_width = 1.5)

##################################################
##################################################
#### map with current stuff ####


my_plot_legend <- my_map_spcurrent +
  guides(
    limits = c(0, 1),
    fill = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "grey30",
      barwidth = grid::unit(1000, "pt"),
      barheight = grid::unit(17.5, "pt"),
      ticks.linewidth = 1,
      label.theme = element_text(
        size = 16,
        hjust = 0.5,
        vjust = 0.5,
        color = "grey30",
        family = "Helvetica"
      )
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.spacing.x = grid::unit(3, "pt"),
    legend.margin = margin(l = 0.4, unit='cm'),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.title = element_text(size = 26, family = "Helvetica", vjust = -1, color = "grey10")
  )+
  labs(fill = NULL, color = NULL) 

my_legend <- cowplot::get_legend(my_plot_legend)
# ggdraw(my_legend)

my_agg_map_new <-   (my_map_northcurrent | my_map_north26 | my_map_north85) /
  (my_map_southcurrent | my_map_south26 | my_map_south85) /
  (my_map_spcurrent | my_map_sp26 | my_map_sp85)  +
  my_legend +
  theme(
    plot.tag = element_text(size = 24),
    plot.title = element_text(size = 32)
  ) +
  plot_layout(heights = c(4, 4, 4, 1))



## Set filename for the output filemap
fig2_map_outname <- str_glue("{folder_path_figures}/Fig2_results_consensus_map{mask_filename}_{Rahat::today()}_{format(Sys.time(), paste0('%H-%M-%S'))}.png")

## Final file version for submission stored in 600 dpi's
tic()
ggsave(fig2_map_outname, my_agg_map_new,
       dpi = 600, height = 14, width = 16)  
toc()