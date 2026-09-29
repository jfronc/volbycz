# ==============================================================================
# 1. ENVIRONMENT INITIALIZATION & ONE-TIME GLOBAL LOOKUPS
# ==============================================================================
xfun::pkg_attach(c("tidyverse", "magrittr", "xml2", "RCzechia", "sf", "scales", "ggiraph"))

message("Caching administrative shapes from RÚIAN...")
all_municipalities_polygons <- obce_polygony() %>%
  select(obec_code = KOD_OBEC, geometry) %>%
  mutate(obec_code = as.character(obec_code)) %>%
  st_transform(crs = 5514)

all_quarters_polygons <- casti() %>%
  select(obec_code = KOD, geometry) %>%
  mutate(obec_code = as.character(obec_code)) %>%
  st_transform(crs = 5514)

ns <- c(default = "http://www.volby.cz/senat/")
date <- "20201002"
global_results_url <- paste0("https://volby.gov.cz/appdata/senat/", date, "/odata/vysledky.xml")

xml_global <- tryCatch({
  read_xml(global_results_url)
}, error = function(e) {
  stop("CRITICAL: Failed to download global results registry. Verify connectivity.")
})

# ==============================================================================
# 2. CORE ENGINE: PARAMETERIZED SELECTION & VISUALIZATION FUNCTION
# ==============================================================================
create_independent_senate_maps <- function(target_so_id, 
                                           municipality_cache = all_municipalities_polygons,
                                           quarter_cache = all_quarters_polygons,
                                           date) {
  
  xml_district_url <- paste0("https://volby.gov.cz/appdata/senat/", date, "/odata/obvody/vysledky_obce_obvod_", target_so_id, ".xml")
  
  xml_district <- tryCatch({
    read_xml(xml_district_url)
  }, error = function(e) {
    stop(sprintf("CRITICAL: Failed to download XML for Senate District %s.", target_so_id))
  })
  
  # A. Map Candidate Names via Metadata Registry (Deterministic Sequence)
  district_node <- xml_find_first(xml_global, paste0(".//default:OBVOD[@CISLO='", target_so_id, "']"), ns)
  if (length(district_node) == 0 || is.na(district_node)) {
    stop(sprintf("Validation Error: Senate District %s not found in metadata.", target_so_id))
  }
  
  candidate_registry <- district_node %>%
    xml_find_all("default:KANDIDAT", ns) %>%
    map(\(cand_node) {
      tibble(
        candidate_id   = as.numeric(xml_attr(cand_node, "PORADOVE_CISLO")),
        candidate_name = paste(xml_attr(cand_node, "JMENO"), xml_attr(cand_node, "PRIJMENI"))
      )
    }) %>%
    list_rbind() %>%
    arrange(candidate_id)
  
  # B. Parse District Units Natively into Long Format (Extracting Both Rounds)
  obec_nodes <- xml_find_all(xml_district, ".//default:OBEC", ns)
  
  election_long <- obec_nodes %>%
    map(\(obec_node) {
      obec_code <- as.character(xml_attr(obec_node, "CIS_OBEC"))
      obec_name <- xml_attr(obec_node, "NAZ_OBEC")
      
      ucast_nodes <- xml_find_all(obec_node, ".//default:UCAST", ns)
      turnout_map <- ucast_nodes %>% 
        map(\(u) {
          tibble(
            kolo = as.numeric(xml_attr(u, "KOLO")),
            total_turnout = as.numeric(xml_attr(u, "PLATNE_HLASY"))
          )
        }) %>% 
        list_rbind()
      
      hlasy_nodes <- xml_find_all(obec_node, ".//default:HLASY", ns)
      if (length(hlasy_nodes) == 0) return(NULL)
      
      hlasy_nodes %>%
        map(\(h) {
          tibble(
            candidate_id = as.numeric(xml_attr(h, "PORADOVE_CISLO")),
            votes_r1     = as.numeric(xml_attr(h, "HLASY_1KOLO")),
            votes_r2     = as.numeric(xml_attr(h, "HLASY_2KOLO"))
          )
        }) %>%
        list_rbind() %>%
        pivot_longer(
          cols = c(votes_r1, votes_r2), 
          names_to = "round_label", 
          values_to = "votes"
        ) %>%
        mutate(kolo = if_else(round_label == "votes_r1", 1, 2)) %>%
        filter(!is.na(votes), votes > 0) %>%
        inner_join(turnout_map, by = "kolo") %>%
        mutate(obec_code = obec_code, obec_name = obec_name)
    }) %>%
    keep(~ !is.null(.x)) %>%
    list_rbind()
  
  election_long %<>% left_join(candidate_registry, by = "candidate_id")
  
  # C. Compute Top 3 for Tooltips Fallback Context
  top3_tooltips <- election_long %>%
    mutate(pct = votes / total_turnout) %>%
    group_by(obec_code, kolo) %>%
    arrange(desc(votes), .by_group = TRUE) %>%
    slice_head(n = 3) %>%
    summarise(
      rank_html = paste0(
        "<div style='margin-top: 5px; border-top: 1px solid #EEEEEE; padding-top: 5px;'>",
        paste0("<span style='font-size: 11px;'>", row_number(), ". ", candidate_name, 
               ": <strong>", round(pct * 100, 1), "%</strong></span>", collapse = "<br/>"),
        "</div>"
      ),
      .groups = "drop"
    )
  
  # D. Analytical Processing Matrix: Calculate Margins Per Round Natively
  processed_rounds <- election_long %>%
    group_by(obec_code, obec_name, kolo) %>%
    arrange(desc(votes), .by_group = TRUE) %>%
    summarise(
      # CRITICAL FIX: Ensure total_turnout is reduced to a vector of length 1
      abs_vote_margin = if_else(n() > 1, first(votes) - nth(votes, 2), first(votes)),
      pct_margin      = abs_vote_margin / first(total_turnout), 
      leader_name     = first(candidate_name),
      total_turnout   = first(total_turnout),
      .groups = "drop"
    ) %>%
    left_join(top3_tooltips, by = c("obec_code", "kolo")) %>%
    mutate(
      tooltip_text = paste0(
        "<div style='font-family:sans-serif; padding:10px; background-color:#FFFFFF; border:1px solid #CCCCCC; border-radius:4px;'>",
        "<strong>", obec_name, "</strong> (Kolo ", kolo, ")<br/>",
        "Leader: <strong>", leader_name, "</strong><br/>",
        "Lead Margin: <strong>", format(abs_vote_margin, big.mark = " "), " votes</strong> (", round(pct_margin * 100, 1), "%)",
        rank_html,
        "</div>"
      )
    )
  
  # E. RECONCILE GEOMETRIES (City Quarters vs. Standalone Towns)
  unique_codes <- unique(processed_rounds$obec_code)
  district_polygons_sf <- quarter_cache %>% filter(obec_code %in% unique_codes)
  missing_codes        <- setdiff(unique_codes, district_polygons_sf$obec_code)
  
  if (length(missing_codes) > 0) {
    standard_segments <- municipality_cache %>% filter(obec_code %in% missing_codes)
    district_polygons_sf %<>% bind_rows(standard_segments)
  }
  
  district_bubbles_sf <- district_polygons_sf %>%
    st_centroid() %>% 
    inner_join(processed_rounds, by = "obec_code")
  
  all_candidates    <- unique(district_bubbles_sf$leader_name)
  candidate_palette <- set_names(scales::hue_pal()(length(all_candidates)), all_candidates)
  
  # F. INDEPENDENT RENDER GENERATOR CLOSURE
  build_independent_layer <- function(target_kolo) {
    round_bubbles <- district_bubbles_sf %>% filter(kolo == target_kolo)
    
    local_max_margin <- max(round_bubbles$abs_vote_margin, na.rm = TRUE)
    local_breaks     <- round(seq(0, local_max_margin, length.out = 4))
    
    map_gg <- ggplot() +
      geom_sf(data = district_polygons_sf, fill = "#FAFAFA", color = "#E0E0E0", linewidth = 0.35) +
      geom_sf_interactive(
        data = round_bubbles,
        aes(size = abs_vote_margin, fill = leader_name, alpha = pct_margin, tooltip = tooltip_text, data_id = obec_code),
        shape = 21, color = "#FFFFFF", stroke = 0.4
      ) +
      scale_size_area(
        max_size = 14,
        limits   = c(0, local_max_margin), 
        breaks   = local_breaks,
        labels = NULL,
        name   = NULL
      ) +
      scale_fill_manual(values = candidate_palette, name = "Leading Candidate") +
      scale_alpha_continuous(range = c(0.40, 0.95),
                             labels = NULL,
                             name   = NULL) +
      labs(
        title    = paste("Senate Election 2024 — District", target_so_id),
        subtitle = paste("Round", target_kolo, "— Local independent bubble scale boundaries applied"),
        caption  = "Source: ČSÚ (volby.cz) | Geometries via RCzechia (casti fallback layer)"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10),
        legend.box = "vertical", legend.position = "right"
      ) +
      coord_sf(datum = NA)
    
    girafe(
      ggobj = map_gg,
      options = list(
        opts_tooltip(css = "background-color:none; border:none; box-shadow:none;"),
        opts_hover(css = "stroke:#111111; stroke-width:1.5px; cursor:pointer;"),
        opts_sizing(rescale = TRUE)
      ),
      width_svg = 7.5, height_svg = 5.5
    )
  }
  
  return(list(
    round_1 = build_independent_layer(1),
    round_2 = build_independent_layer(2)
  ))
}

district_maps <- create_independent_senate_maps(target_so_id = 45, date = date)

# Render each round with its own standalone sizing limits
district_maps$round_1
district_maps$round_2
