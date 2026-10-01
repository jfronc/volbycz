# V jakých obcích senátního obvodu je nejvíc zapsaných voličů?

xfun::pkg_attach("tidyverse", "magrittr", "xml2")

get_xml <- function (so) {
  base_url <- "https://www.volby.cz/appdata/senat/20181005/odata/obvody/vysledky_obce_obvod_"
  xml <- paste0(base_url, so, ".xml") %>% read_xml()
  return(xml)
}

ns <- c(default = "http://www.volby.cz/senat/")

town_vote <- function (so) {
  res <- get_xml(so = so)
  
  obce <- xml_find_all(res, ".//default:OBEC", ns) %>%
    map( ~{
      list(
        NAZ_OBEC = xml_attr(.x, "NAZ_OBEC"),
        ZAPSANI_VOLICI = xml_find_first(.x, ".//default:UCAST", ns) %>% xml_attr("ZAPSANI_VOLICI") %>% as.numeric()
      )
    }) %>% bind_rows()
  
  obce %>% arrange(desc(ZAPSANI_VOLICI)) %>% return()
}

# town_vote(81)
