# ==============================================================================
# 1. ENVIRONMENT INITIALIZATION & ONE-TIME GLOBAL LOOKUPS
# ==============================================================================
xfun::pkg_attach("tidyverse", "magrittr", "xml2", "RCzechia", "sf", "scales", "ggiraph")
source("utils/bubble_map.R")
source("senate/create_senate_map.R")

message("Caching administrative shapes from RÚIAN...")
all_municipalities_polygons <- obce_polygony() %>%
  select(borough_code = KOD_OBEC, geometry) %>%
  mutate(borough_code = as.character(borough_code)) %>%
  st_transform(crs = 5514)

all_quarters_polygons <- casti() %>%
  select(borough_code = KOD, geometry) %>%
  mutate(borough_code = as.character(borough_code)) %>%
  st_transform(crs = 5514)

ns   <- c(default = "http://www.volby.cz/senat/")
date <- "20261009"

xml_global <- tryCatch(
  read_xml(paste0("https://volby.gov.cz/appdata/senat/", date, "/odata/vysledky.xml")),
  error = function(e) stop("CRITICAL: Failed to download global results registry.")
)

so_id = 69
district_data = parse_senate_district(target_so_id = so_id, date, xml_global, keep_uncounted = TRUE)
district_maps = create_senate_map_live(target_so_id = so_id,
                              district_data,
                              all_municipalities_polygons,
                              all_quarters_polygons)
  
# district_maps$r1
district_maps$r2
# 
# class_c <- seq(3, 81, by = 3)
# 
# library(htmlwidgets)
# 
# for (i in class_c) {
#   maps <- create_senate_map(target_so_id = i, date = date)
#   
#   iwalk(maps, \(widget, round) {
#     saveWidget(
#       widget = widget,
#       file = glue::glue("docs/senate/2020/{i}_{round}.html"),
#       selfcontained = FALSE,
#       libdir = "lib"
#     )
#   })
# }
