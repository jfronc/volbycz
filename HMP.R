# ==============================================================================
# 1. ENVIRONMENT INITIALIZATION & GEOGRAPHIC CACHE
# ==============================================================================
xfun::pkg_attach(c("tidyverse", "magrittr", "RCzechia", "sf", "scales", "ggiraph", "readxl"))
source("utils/parties.R")

message("Loading and caching Prague city borough boundaries...")
all_prague_boroughs <- casti() %>%
  filter(NAZ_OBEC == "Praha") %>%
  select(borough_code = KOD, borough_name = NAZEV, geometry) %>%
  mutate(borough_code = as.character(borough_code)) %>%
  st_transform(crs = 5514)

# ==============================================================================
# 2. DOWNLOAD AND EXTRACTION PIPELINE
# ==============================================================================
download.file(
  url      = "https://volby.gov.cz/opendata/kv2022/KV2022_data_20260328_xlsx.zip",
  destfile = zip_tmp <- tempfile(fileext = ".zip"),
  mode     = "wb",
  quiet    = TRUE
)

kvhl <- unzip(zip_tmp, files = "kvhl.xlsx", exdir = tempdir()) |> read_excel()
kvt3 <- unzip(zip_tmp, files = "kvt3.xlsx", exdir = tempdir()) |> read_excel()

kvhl %<>%
  filter(KRAJ == 1100 & TYPZASTUP == 1) %>%
  select(OBEC, POR_STR_HL, POC_HLASU) %>%
  summarise(
    POC_HLASU2 = sum(POC_HLASU, na.rm = TRUE),
    .by = c(OBEC, POR_STR_HL)
  )

# ==============================================================================
# 3. PARTY REGISTRY
# ==============================================================================
# TYPZASTUP == 1 is city-wide; KODZASTUP for Praha city-wide assembly = 554782
# POR_STR_HL ballot order is scoped to one KODZASTUP, so filtering to 554782
# gives the correct party-name lookup for all borough rows in kvhl
kvros <- readr::read_csv2(
  file           = "https://volby.gov.cz/opendata/kv2022/csv/kvros.csv",
  locale         = locale(encoding = "CP1250"),
  show_col_types = FALSE
)

party_registry <- kvros %>%
  filter(KODZASTUP == 554782, POR_STR_HL > 0) %>%   # numeric filter before coercion
  select(POR_STR_HL, NAZEVCELK, ZKRATKAO8) %>%
  mutate(
    POR_STR_HL = as.character(POR_STR_HL),
    ZKRATKAO8 = replace_values(ZKRATKAO8, from = parties_rename$from, to = parties_rename$to)
    )
  

# ==============================================================================
# 4. JOIN PARTY NAMES
# ==============================================================================
kvhl %<>%
  mutate(POR_STR_HL = as.character(POR_STR_HL)) %>%
  left_join(party_registry, by = "POR_STR_HL")

# ==============================================================================
# 5. TURNOUT PER BOROUGH
# ==============================================================================
borough_turnout <- kvt3 %>%
  filter(KRAJ == 1100, TYPZASTUP == 1) %>%
  summarise(
    total_valid_votes = sum(PL_HL_CELK, na.rm = TRUE),
    .by = OBEC
  ) %>%
  mutate(OBEC = as.character(OBEC))

# ==============================================================================
# 6. WINNER & MARGIN PER BOROUGH
# ==============================================================================
borough_results <- kvhl %>%
  mutate(OBEC = as.character(OBEC)) %>%
  group_by(OBEC) %>%
  arrange(desc(POC_HLASU2), .by_group = TRUE) %>%
  summarise(
    winner_party    = first(NAZEVCELK),
    winner_short    = first(ZKRATKAO8),
    winner_votes    = first(POC_HLASU2),
    runner_up_votes = if_else(n() > 1, nth(POC_HLASU2, 2), 0),
    abs_vote_margin = winner_votes - runner_up_votes,
    .groups = "drop"
  ) %>%
  left_join(borough_turnout, by = "OBEC") %>%
  left_join(
    all_prague_boroughs %>%
      st_drop_geometry() %>%
      select(borough_code, borough_name),
    by = c("OBEC" = "borough_code")
  ) %>%
  rename(borough_code = OBEC) %>%
  mutate(pct_margin = abs_vote_margin / total_valid_votes)

# Top-3 parties per borough for tooltip detail
# pct denominator = full borough total (computed before slice)
top3_tooltips <- kvhl %>%
  mutate(OBEC = as.character(OBEC)) %>%
  group_by(OBEC) %>%
  arrange(desc(POC_HLASU2), .by_group = TRUE) %>%
  mutate(pct = POC_HLASU2 / sum(POC_HLASU2)) %>%
  slice_head(n = 3) %>%
  mutate(
    party_colour = coalesce(
      parties_palette[ZKRATKAO8],
      "#AAAAAA"                           # fallback grey for unmapped parties
    )
  ) %>%
  summarise(
    rank_html = paste0(
      "<table style='border-collapse:collapse; width:220px; margin-top:6px;",
      " border-top:2px solid #E2E2E2; padding-top:2px; font-family:Georgia,serif;'>",
      "<tr style='font-size:10px; color:#999999;'>",
      "<td style='padding:2px 6px 2px 0; width:100%;'>Strana</td>",
      "<td style='padding:2px 4px; text-align:right;'>Hlasy</td>",
      "<td style='padding:2px 0 2px 4px; text-align:right;'>%</td>",
      "</tr>",
      paste0(
        "<tr style='font-size:12px;'>",
        "<td style='padding:3px 6px 3px 0;'>",
        "<span style='display:inline-block; width:4px; height:14px;",
        " background-color:", party_colour,
        "; margin-right:5px; vertical-align:middle; border-radius:1px;'></span>",
        ZKRATKAO8,
        "</td>",
        "<td style='padding:3px 4px; text-align:right; color:#444444;'>",
        format(POC_HLASU2, big.mark = "\u00a0"),
        "</td>",
        "<td style='padding:3px 0 3px 4px; text-align:right; font-weight:bold; white-space:nowrap'>",
        round(pct * 100, 1), " %",
        "</td>",
        "</tr>",
        collapse = ""
      ),
      "</table>"
    ),
    .groups = "drop"
  ) %>%
  rename(borough_code = OBEC)

borough_results %<>%
  left_join(top3_tooltips, by = "borough_code") %>%
  mutate(
    tooltip_text = paste0(
      "<div style='font-family:Georgia,serif; padding:10px 12px; background-color:#FFFFFF;",
      " border:1px solid #CCCCCC; border-radius:3px;",
      " box-shadow:0 2px 6px rgba(0,0,0,0.12); min-width:210px;'>",
      "<div style='font-size:13px; font-weight:bold; color:#111111; margin-bottom:1px;'>",
      borough_name,
      "</div>",
      "<div style='font-size:10px; color:#999999; margin-bottom:2px;'>",
      format(total_valid_votes, big.mark = "\u00a0"), " platných hlasů",
      "</div>",
      rank_html,
      "</div>"
    )
  )

# Sanity checks
message(sprintf(
  "%d boroughs in results; %d matched to geometry; total valid votes: %s",
  nrow(borough_results),
  sum(borough_results$borough_code %in% all_prague_boroughs$borough_code),
  format(sum(borough_results$total_valid_votes, na.rm = TRUE), big.mark = " ")
))
if (any(is.na(borough_results$winner_party)))
  warning("NA winner_party — party_registry join failed for some boroughs; check POR_STR_HL values")

# ==============================================================================
# 7. MAP
# ==============================================================================
create_prague_map <- function(results, borough_cache = all_prague_boroughs) {
  
  polygons_sf <- borough_cache %>%
    inner_join(results, by = "borough_code")
  
  unmatched_geo <- setdiff(results$borough_code, polygons_sf$borough_code)
  if (length(unmatched_geo) > 0)
    warning(sprintf("No geometry for %d borough(s): %s",
                    length(unmatched_geo), paste(unmatched_geo, collapse = ", ")))
  
  bubbles_sf  <- polygons_sf %>% st_centroid()
  max_margin  <- max(bubbles_sf$abs_vote_margin, na.rm = TRUE)
  size_breaks <- unique(round(seq(0, max_margin, length.out = 4)))
  

  missing_parties <- setdiff(unique(bubbles_sf$winner_short), names(parties_palette))
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
      breaks   = size_breaks,
      labels   = NULL,
      name   = NULL
    ) +
    scale_fill_manual(
      values   = parties_palette,
      name     = "Vítěz",
      na.value = "grey60"
    ) +  # fill mapped to winner_short (ZKRATKAO8)
    scale_alpha_continuous(
      range  = c(0.40, 0.95),
      labels = NULL,
      name   = NULL
    ) +
    labs(
      title    = "Volby do zastupitelstva HMP 2022",
      subtitle = "Vítězové dle MČ. Velikost bubliny = absolutní náskok; sytost = relativní náskok",
      caption  = "Source: \u010cS\u00da (volby.cz) opendata | Geometry: RCzechia::casti()"
    ) +
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

prague_map <- create_prague_map(borough_results)
prague_map
