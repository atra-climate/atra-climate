##########################################
####  Create species map for Methods section
##########################################
#### | Project name: Salamander modeling
#### | Script type: Figure outputs
#### | What it does: Loads data and creates map
#### | Creator: -----
#### | Contact: -----
##########################################



# Script setup ------------------------------------------------------------

pacman::p_load(Rahat, tidyverse, janitor, raster, sf, ggspatial, mapedit, ggnewscale,
               here, cowplot, ggthemes, mapview, patchwork, ggtext, hrbrthemes)


# Load setup script. Fork is for type of computer, in case here::here() cannot
# find the path on the local folder. This is the case on the computing cluster,
# where here doesn't work well.
# Source variables

source(here::here("R", "main_setup.R"))
  


# Load and process data ---------------------------------------------------

## Define projection for plotting
utm_33n <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Process alpine ranges
range_alps <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("European_Alps") %>% 
  st_read() %>% 
  transmute(
    range = "Alps"
  ) %>% 
  st_transform(crs = utm_33n)


range_dinarides <- here("data", "alpine_ranges") %>% 
  list.files(full.names = TRUE, pattern = "shp$") %>% 
  str_subset("Dinaric_") %>% 
  st_read() %>% 
  transmute(
    range = "Dinarides"
  ) %>% 
  st_transform(crs = utm_33n)

#
alpine_ranges <- rbind(range_alps, range_dinarides)

## Species data
atra_raw_sf <- st_read(var_path_species_data)

atra_thinned_sf <- atra_raw_sf %>% 
  st_transform(crs = utm_33n) %>% 
  mutate(
    subspecies2 = case_when(
      subspecies == "points_all" ~ "*Salamandra atra ssp.*",
      subspecies == "points_atra" ~ "*Salamandra atra atra*",
      subspecies == "points_prenjensis" ~ "*Salamandra atra prenjensis*",
    ) %>% 
      as.factor() %>% 
      fct_relevel(c("*Salamandra atra ssp.*", "*Salamandra atra atra*", "*Salamandra atra prenjensis*"))
  ) 



##  DEM and countries
DEM_large <- here::here("results", "DEM_aggregated.tif") %>% 
  raster()

## Convert DEM to a ggplot-friendly object
dem_gg2 <- DEM_large %>%
  projectRaster(crs = utm_33n) %>%
  raster_to_gg()

dinaric_countries <- here::here("results", "procext_countries.gpkg") %>% 
  st_read() %>% 
  st_transform(crs = utm_33n)

# Create map --------------------------------------------------------------



# Subset species data for a better map

atra_points_subspecies <- atra_thinned_sf %>% 
  filter(subspecies != "points_all")

atra_points_species <- atra_thinned_sf %>% 
  filter(subspecies == "points_all")

p_subsp <- ggplot() +
  # Plot elevation underneath
  geom_tile(data = dem_gg2, aes(x = x, y = y, fill = DEM_aggregated), alpha = 0.5, show.legend = FALSE) +
  # Plot country borders
  scale_fill_gradient(low = "#f0f0f0", high = "#525252") + # Grey
  geom_sf(data = dinaric_countries, fill = NA, color = "grey50", alpha = 0) +
  new_scale_fill() +
  geom_sf(data = alpine_ranges, 
          aes(fill = range),
          # fill = "#1b9e77", 
          # color = "#1b9e77",
          # show.legend = T,
          alpha = 0.4, size = 0.5) +
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  ########
  new_scale_fill() +
  # geom_sf(data = range_dinarides, fill = "#1b9e77", color = "#1b9e77", alpha = 0.4, size = 0.5) +
  # geom_sf(data = range_alps, fill = "#d95f02", color = "#d95f02", alpha = 0.4, size = 0.5) +
  geom_sf(data = atra_points_species, 
          aes(fill = subspecies2),
          alpha = 0.9,
          # fill = NA,
          # show.legend = TRUE,
          color = "grey10", shape = 21, size = 1.5, stroke = 0.75) +
  # scale_fill_manual(values = c("#984ea3", "#EFC000FF", "blue")) +
  geom_sf(data = atra_points_subspecies, 
          aes(fill = subspecies2),
          alpha = 0.95,
          # show.legend = TRUE,
          color = "grey10", shape = 21, size = 1.5, stroke = 0.75) +
  scale_fill_manual(values = c("#984ea3", "#EFC000FF", "grey98")) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.text = element_markdown(size = 18, family = "Helvetica", color = "grey10"),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.ticks = element_line(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 14, family = "mono", color = "grey40"),
    panel.grid.major = element_line(colour = "grey60", linetype = "dotted", size = 0.75),
    panel.background = element_rect(fill = "transparent")
  ) +
  annotation_scale(text_cex = 1.25, 
                   text_col = "grey20",
                   line_col = "grey20",
                   location = "br",
                   style = "ticks",
                   height = unit(0.3, "cm"),
                   pad_x = unit(0.5, "cm"),
                   pad_y = unit(0.3, "cm"),
                   line_width = 2) +
  coord_sf(
    xlim = c(st_bbox(atra_thinned_sf)[1] * 2.3,
             st_bbox(atra_thinned_sf)[3] * 1.05),
    ylim = c(st_bbox(atra_thinned_sf)[2] / 1.03,
             st_bbox(atra_thinned_sf)[4] * 1.012))

# p_subsp
## Set filename for the output filemap
map_outname <- str_glue("{folder_path_figures}/methods_species_map_{Rahat::today()}_{format(Sys.time(), paste0('%H-%M-%S'))}.png")

## Final file version for submission stored in 600 dpi's
ggsave(map_outname, p_subsp,
       dpi = 300, height = 7, width = 12)  

