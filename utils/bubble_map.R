create_bubble_map <- function(results,
                              polygons,
                              palette,
                              title    = "",
                              subtitle = "",
                              caption  = "") {
  
  polygons_sf <- polygons %>%
    inner_join(results, by = "borough_code")
  
  unmatched_geo <- setdiff(results$borough_code, polygons_sf$borough_code)
  if (length(unmatched_geo) > 0)
    warning(sprintf("No geometry for %d unit(s): %s",
                    length(unmatched_geo), paste(unmatched_geo, collapse = ", ")))
  
  bubbles_sf <- polygons_sf %>% st_centroid()
  max_margin <- max(bubbles_sf$abs_vote_margin, na.rm = TRUE)
  
  missing_parties <- setdiff(unique(bubbles_sf$winner_short), names(palette))
  if (length(missing_parties) > 0)
    message("Winners with no palette entry (will render grey): ",
            paste(missing_parties, collapse = "; "))
  
  ggplot() +
    geom_sf(
      data = polygons_sf,
      fill = "#FAFAFA", color = "#DCDCDC", linewidth = 0.4
    ) +
    geom_sf_interactive(
      data = bubbles_sf,
      aes(
        size    = abs_vote_margin,
        fill    = winner_short,
        alpha   = pct_margin,
        tooltip = tooltip_text,
        data_id = borough_code
      ),
      shape = 21, color = "#FFFFFF", stroke = 0.4
    ) +
    scale_size_area(
      max_size = 14,
      limits   = c(0, max_margin),
      guide    = "none"
    ) +
    scale_fill_manual(
      values   = palette,
      name     = "Vítěz",
      na.value = "grey60"
    ) +
    scale_alpha_continuous(
      range = c(0.40, 0.95),
      guide = "none"
    ) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_minimal(base_family = "sans") +
    theme(
      panel.grid      = element_blank(),
      axis.text       = element_blank(),
      axis.title      = element_blank(),
      plot.title      = element_text(face = "bold", size = 13),
      legend.title    = element_text(face = "bold", size = 11),
      legend.text     = element_text(size = 10),
      legend.box      = "vertical",
      legend.position = "right"
    ) +
    coord_sf(datum = NA) ->
    map_gg
  
  girafe(
    ggobj   = map_gg,
    options = list(
      opts_tooltip(css = "background-color:none; border:none; box-shadow:none;"),
      opts_hover(css   = "stroke:#111111; stroke-width:1.5px; cursor:pointer;"),
      opts_sizing(rescale = TRUE)
    ),
    width_svg  = 7.5,
    height_svg = 5.5
  )
}