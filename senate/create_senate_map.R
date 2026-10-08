xfun::pkg_attach("tidyverse", "magrittr", "sf", "scales", "ggiraph")

create_senate_map <- function(target_so_id,
                              municipality_cache = all_municipalities_polygons,
                              quarter_cache      = all_quarters_polygons,
                              xml_global,
                              date) {

  xml_district <- tryCatch(
    read_xml(paste0(
      "https://volby.gov.cz/appdata/senat/", date,
      "/odata/obvody/vysledky_obce_obvod_", target_so_id, ".xml"
    )),
    error = function(e)
      stop(sprintf("CRITICAL: Failed to download XML for Senate District %s.", target_so_id))
  )
  
  # A. Candidate registry (deterministic order)
  district_node <- xml_find_first(
    xml_global,
    paste0(".//default:OBVOD[@CISLO='", target_so_id, "']"), ns
  )
  if (length(district_node) == 0 || is.na(district_node))
    stop(sprintf("Validation Error: Senate District %s not found in metadata.", target_so_id))
  
  candidate_registry <- district_node %>%
    xml_find_all("default:KANDIDAT", ns) %>%
    map(\(n) tibble(
      candidate_id   = as.numeric(xml_attr(n, "PORADOVE_CISLO")),
      candidate_name = paste(xml_attr(n, "JMENO"), xml_attr(n, "PRIJMENI"))
    )) %>%
    list_rbind() %>%
    arrange(candidate_id)
  
  # B. Parse both rounds into long format
  obec_nodes <- xml_find_all(xml_district, ".//default:OBEC", ns)
  
  election_long <- obec_nodes %>%
    map(\(obec_node) {
      obec_code <- as.character(xml_attr(obec_node, "CIS_OBEC"))
      obec_name <- xml_attr(obec_node, "NAZ_OBEC")
      
      turnout_map <- xml_find_all(obec_node, ".//default:UCAST", ns) %>%
        map(\(u) tibble(
          kolo          = as.numeric(xml_attr(u, "KOLO")),
          total_turnout = as.numeric(xml_attr(u, "PLATNE_HLASY"))
        )) %>%
        list_rbind()
      
      hlasy_nodes <- xml_find_all(obec_node, ".//default:HLASY", ns)
      if (length(hlasy_nodes) == 0) return(NULL)
      
      hlasy_nodes %>%
        map(\(h) tibble(
          candidate_id = as.numeric(xml_attr(h, "PORADOVE_CISLO")),
          votes_r1     = as.numeric(xml_attr(h, "HLASY_1KOLO")),
          votes_r2     = as.numeric(xml_attr(h, "HLASY_2KOLO"))
        )) %>%
        list_rbind() %>%
        pivot_longer(c(votes_r1, votes_r2), names_to = "round_label", values_to = "votes") %>%
        mutate(kolo = if_else(round_label == "votes_r1", 1L, 2L)) %>%
        filter(!is.na(votes), votes > 0) %>%
        inner_join(turnout_map, by = "kolo") %>%
        mutate(borough_code = obec_code, obec_name = obec_name)
    }) %>%
    keep(~ !is.null(.x)) %>%
    list_rbind() %>%
    left_join(candidate_registry, by = "candidate_id")
  
  # C. Per-district candidate palette — built here so tooltip and map share it
  all_candidates    <- unique(election_long$candidate_name)
  candidate_palette <- set_names(scales::hue_pal()(length(all_candidates)), all_candidates)
  
  # D. Top-3 tooltip table (NYT style, matching KV map)
  top3_tooltips <- election_long %>%
    group_by(borough_code, kolo) %>%
    arrange(desc(votes), .by_group = TRUE) %>%
    mutate(
      pct            = votes / total_turnout,
      ribbon_colour  = coalesce(candidate_palette[candidate_name], "#AAAAAA")
    ) %>%
    slice_head(n = 3) %>%
    summarise(
      rank_html = paste0(
        "<table style='border-collapse:collapse; width:230px; margin-top:6px;",
        " border-top:2px solid #E2E2E2; font-family:Georgia,serif;'>",
        "<tr style='font-size:10px; color:#999999;'>",
        "<td style='padding:2px 6px 2px 0; width:100%;'>Kandidát</td>",
        "<td style='padding:2px 4px; text-align:right;'>Hlasy</td>",
        "<td style='padding:2px 0 2px 4px; text-align:right;'>%</td>",
        "</tr>",
        paste0(
          "<tr style='font-size:12px;'>",
          "<td style='padding:3px 6px 3px 0;'>",
          "<span style='display:inline-block; width:4px; height:14px;",
          " background-color:", ribbon_colour,
          "; margin-right:5px; vertical-align:middle; border-radius:1px;'></span>",
          candidate_name, "</td>",
          "<td style='padding:3px 4px; text-align:right; color:#444444;'>",
          format(votes, big.mark = "\u00a0"), "</td>",
          "<td style='padding:3px 0 3px 4px; text-align:right; font-weight:bold;",
          " white-space:nowrap;'>",
          round(pct * 100, 1), " %</td>",
          "</tr>",
          collapse = ""
        ),
        "</table>"
      ),
      .groups = "drop"
    )
  
  # D. Margins per round
  processed_rounds <- election_long %>%
    group_by(borough_code, obec_name, kolo) %>%
    arrange(desc(votes), .by_group = TRUE) %>%
    summarise(
      abs_vote_margin = if_else(n() > 1, first(votes) - nth(votes, 2), first(votes)),
      pct_margin      = abs_vote_margin / first(total_turnout),
      winner_short    = first(candidate_name),   # candidate name as the fill key
      total_turnout   = first(total_turnout),
      .groups = "drop"
    ) %>%
    left_join(top3_tooltips, by = c("borough_code", "kolo")) %>%
    mutate(
      tooltip_text = paste0(
        "<div style='font-family:Georgia,serif; padding:10px 12px; background-color:#FFFFFF;",
        " border:1px solid #CCCCCC; border-radius:3px;",
        " box-shadow:0 2px 6px rgba(0,0,0,0.12); min-width:230px;'>",
        "<div style='font-size:13px; font-weight:bold; color:#111111; margin-bottom:1px;'>",
        obec_name, " (Kolo ", kolo, ")</div>",
        "<div style='font-size:10px; color:#999999; margin-bottom:2px;'>",
        format(total_turnout, big.mark = "\u00a0"), " platných hlasů</div>",
        rank_html,
        "</div>"
      )
    )
  
  # E. Geometry reconciliation: quarters first, municipalities as fallback
  unique_codes         <- unique(processed_rounds$borough_code)
  district_polygons_sf <- quarter_cache %>% filter(borough_code %in% unique_codes)
  missing_codes        <- setdiff(unique_codes, district_polygons_sf$borough_code)
  
  if (length(missing_codes) > 0)
    district_polygons_sf %<>% bind_rows(
      municipality_cache %>% filter(borough_code %in% missing_codes)
    )
  
  unresolved <- setdiff(unique_codes, district_polygons_sf$borough_code)
  if (length(unresolved) > 0)
    warning(sprintf("%d code(s) matched no geometry: %s",
                    length(unresolved), paste(unresolved, collapse = ", ")))
  
  # G. Render each round via shared create_bubble_map()
  build_round <- function(target_kolo) {
    round_results <- processed_rounds %>% filter(kolo == target_kolo)
    
    create_bubble_map(
      results  = round_results,
      polygons = district_polygons_sf,
      palette  = candidate_palette,
      title    = sprintf("Senátní obvod č. %s", target_so_id),
      subtitle = sprintf("%d. kolo (velikost bubliny = absolutní náskok; sytost = relativní náskok)", target_kolo),
      caption  = "Zdroj: github.com/jfronc | Data: \u010cS\u00da (volby.cz) | Geometrie: RCzechia"
    )
  }
  
  list(
    r1 = build_round(1),
    r2 = build_round(2)
  )
}
