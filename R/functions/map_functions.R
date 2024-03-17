
make_map_stack <- function(stackk)
{
  my_stack <- stackk %>%
    projectRaster(crs = utm_33n) %>%
    raster_to_gg() %>%
    rename(
      committee = 1,
      suitability = 2
    )

  my_pdata <- my_stack %>%
    mutate(
      hr = suitability,
      pr = committee,
      hry = case_when(
        hr <= 0.33 ~ 1,
        hr > 0.33 & hr <= 0.67 ~ 2,
        hr > 0.66 ~ 3
      ),
      prx = case_when(
        pr <= 0.33 ~ 1,
        pr > 0.33 & pr <= 0.67 ~ 2,
        pr > 0.66 ~ 3
        )
      # hry = ifelse(hr<h.v[1],1, ifelse(hr<h.v[2],2,3)),
      # prx = ifelse(pr<p.v[1],1, ifelse(pr<p.v[2],2,3))
    ) %>%
    filter(suitability > 0.01)

  return(my_pdata)
}

####

make_map_stack2 <- function(stackk)
{
  my_stack <- stackk %>%
    projectRaster(crs = utm_33n) %>%
    raster_to_gg() %>%
    rename(
      committee = 1,
      suitability = 2
    )

  my_pdata <- my_stack %>%
    mutate(
      hr = suitability,
      pr = committee,
      hry = case_when(
        hr <= 0.33 ~ 1,
        hr > 0.33 & hr <= 0.67 ~ 2,
        hr > 0.66 ~ 3
      ),
      prx = case_when(
        pr <= 0.33 ~ 1,
        pr > 0.33 & pr <= 0.67 ~ 2,
        pr > 0.66 ~ 3
      ),
      combined = str_c(hry, prx)

      # hry = ifelse(hr<h.v[1],1, ifelse(hr<h.v[2],2,3)),
      # prx = ifelse(pr<p.v[1],1, ifelse(pr<p.v[2],2,3))
    ) %>%
    filter(suitability > 0.01) %>%
    full_join(cell_labels_d, by = c("combined" = "number_code")) %>%
    # dim()
    # filter(is.na(hr)) %>%
    mutate(
      hry = ifelse(is.na(hry), as.numeric(str_sub(combined, 1, 1)), hry),
      prx = ifelse(is.na(prx), as.numeric(str_sub(combined, 2, 2)), prx),
    ) %>%
    fill(x, y)

  return(my_pdata)
}
# Map function ------------------------------------------------------------

make_bivar_map <- function(data)
{
  p_mapp <- ggplot() +
    geom_tile(data = dem_gg2, aes(x = x, y = y, fill = DEM_aggregated), alpha = 0.5, show.legend = FALSE) +
    scale_fill_gradient(low = "#f0f0f0", high = "#525252") + # Grey
    new_scale_fill() +
    geom_tile(data = data, aes(x = x, y = y, fill = atan(hry/prx),
                               alpha = prx + hry), show.legend = FALSE) +
    # alpha = prx),
    # show.legend = FALSE) +
    # geom_tile(data = data, aes(x = x, y = y, fill = atan(hry/prx)), show.legend = FALSE) +
    scale_fill_viridis() +
    geom_sf(data = dinaric_countries, fill = NA, color = "grey60", size = 0.5) +
    # geom_sf(data = division_line, linetype = "dashed", size = 0.7, color = "grey30") +
    # scale_fill_gradient(low = "grey50", high = "grey") +
    # scale_fill_manual(values = c("grey50", "grey")) +
    # scale_fill_manual(values = c("#984ea3", "#EFC000FF")) +
    # theme_ipsum_rc() +
    # theme_map() +
    # guides(fill = guide_legend(override.aes = list(size = 5))) +
    theme(
      plot.tag = element_text(size = 22),
      legend.position = "top",
      legend.text = element_text(size = 20, family = "Helvetica", color = "grey10"),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
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
  # annotation_scale(text_cex = 1.5, location = "br",
  #                  style = "ticks",
  #                  height = unit(0.3, "cm"),
  #                  pad_x = unit(0.5, "cm"),
  #                  pad_y = unit(0.3, "cm"),
  #                  line_width = 2) +

  return(p_mapp)
}

#### Additional function for ensemble maps

make_ensemble_map <- function(data)
{
  p_mapp <- ggplot() +
    geom_tile(data = dem_gg2, aes(x = x, y = y, fill = DEM_aggregated), alpha = 0.5, show.legend = FALSE) +
    scale_fill_gradient(low = "#f0f0f0", high = "#525252") + # Grey
    new_scale_fill() +
    geom_tile(
      data = data,
      aes(x = x, y = y, fill = value),
      show.legend = FALSE) +
    # alpha = prx),
    # show.legend = FALSE) +
    # geom_tile(data = data, aes(x = x, y = y, fill = atan(hry/prx)), show.legend = FALSE) +
    scale_fill_distiller(type = "seq", palette = "BuGn", direction = 1, limits = c(0, 1)) +
    # scale_fill_viridis() +
    geom_sf(data = dinaric_countries, fill = NA, color = "grey60", size = 0.5) +
    # geom_sf(data = division_line, linetype = "dashed", size = 0.7, color = "grey30") +
    # scale_fill_gradient(low = "grey50", high = "grey") +
    # scale_fill_manual(values = c("grey50", "grey")) +
    # scale_fill_manual(values = c("#984ea3", "#EFC000FF")) +
    # theme_ipsum_rc() +
    # theme_map() +
    # guides(fill = guide_legend(override.aes = list(size = 5))) +
    theme(
      plot.tag = element_text(size = 22),
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
  # annotation_scale(text_cex = 1.5, location = "br",
  #                  style = "ticks",
  #                  height = unit(0.3, "cm"),
  #                  pad_x = unit(0.5, "cm"),
  #                  pad_y = unit(0.3, "cm"),
  #                  line_width = 2) +
  
  return(p_mapp)
}


make_changes_map <- function(data)
{
  p_mapp <- ggplot() +
    geom_tile(data = dem_gg2, aes(x = x, y = y, fill = DEM_aggregated), alpha = 0.5, show.legend = FALSE) +
    scale_fill_gradient(low = "#f0f0f0", high = "#525252") + # Grey
    new_scale_fill() +
    geom_tile(
      data = data,
      aes(x = x, y = y, fill = value),
      show.legend = FALSE) +
    # alpha = prx),
    # show.legend = FALSE) +
    # geom_tile(data = data, aes(x = x, y = y, fill = atan(hry/prx)), show.legend = FALSE) +
    scale_fill_viridis() +
    geom_sf(data = dinaric_countries, fill = NA, color = "grey60", size = 0.5) +
    # geom_sf(data = division_line, linetype = "dashed", size = 0.7, color = "grey30") +
    # scale_fill_gradient(low = "grey50", high = "grey") +
    # scale_fill_manual(values = c("grey50", "grey")) +
    # scale_fill_manual(values = c("#984ea3", "#EFC000FF")) +
    # theme_ipsum_rc() +
    # theme_map() +
    # guides(fill = guide_legend(override.aes = list(size = 5))) +
    theme(
      plot.tag = element_text(size = 22),
      legend.position = "top",
      legend.text = element_text(size = 20, family = "Helvetica", color = "grey10"),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      axis.ticks = element_line(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 14, 
                               # family = "mono",
                               color = "grey40"),
      # panel.grid.major = element_line(color = "grey50", linetype = 2, size = 0.9),
      panel.grid.major = element_line(colour = "grey60", linetype = "dotted", size = 0.75),
      panel.background = element_rect(fill = "transparent")
    ) +
    coord_sf(
      label_graticule = "N",
      xlim = c(st_bbox(atra_thinned_sf)[1] * 1.03, 
               st_bbox(atra_thinned_sf)[3] * 1.03),
      ylim = c(st_bbox(atra_thinned_sf)[2] / 1.01,
               st_bbox(atra_thinned_sf)[4] * 1.01))
  
  # annotation_scale(text_cex = 1.5, location = "br",
  #                  style = "ticks",
  #                  height = unit(0.3, "cm"),
  #                  pad_x = unit(0.5, "cm"),
  #                  pad_y = unit(0.3, "cm"),
  #                  line_width = 2) +
  
  return(p_mapp)
}
