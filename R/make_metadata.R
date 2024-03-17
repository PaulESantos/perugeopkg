#' Generate metadata for downloading geographic data of Peru
#'
#' This function reads gpkg files contained in a folder and generates the necessary metadata
#' for downloading geographic information data of Peru, including departments, provinces, and districts.
#'
#' @param file The path to the gpkg file.
#' @return A tibble with metadata specifying the location of the gpkg files and other relevant information.
#'
#' @export
create_metadata <- function(file) {
  dff <- sf::read_sf(file)
  dfnames <- names(dff)

  # Get column names "departamento" and "provincia" if they exist
  if ("departamento" %in% dfnames & "provincia" %in% dfnames) {
    if(nrow(dff) < 1874) {
      dep <- unique(dff$departamento)

      if(length(unique(dff$provincia)) > 1) {
        prov <- NA_character_
      } else if(length(unique(dff$provincia)) == 1) {
        prov <- unique(dff$provincia)
      }
    } else if(nrow(dff) > 1800) {
      dep <- NA_character_
      prov <- NA_character_
    }
  } else if ("departamento" %in% dfnames & !"provincia" %in% dfnames) {
    dep <- unique(dff$departamento)
    prov <- NA_character_
  } else if (!"departamento" %in% dfnames & "provincia" %in% dfnames) {
    dep <- NA_character_
    if(length(unique(dff$provincia)) > 1) {
      prov <- NA_character_
    } else if(length(unique(dff$provincia)) == 1) {
      prov <- unique(dff$provincia)
    }
  } else if (!("departamento" %in% dfnames) & !("provincia" %in% dfnames)) {
    dep <- NA_character_
    prov <- NA_character_
  }

  # Data level
  if(grepl("dep_", file)) {
    level <- "department"
  } else if(grepl("prov_", file)){
    level <- "province"
  } else if(grepl("nan_", file)){
    level <- "national"
  }

  # Data type
  if(grepl("simplified", file)) {
    tipo <- "simplified"
  } else {
    tipo <- "complete"
  }

  # URL
  url <- "https://github.com/PaulESantos/perugeopkg/raw/master/"

  tibble::tibble(dep_name = dep,
                 prov_name = prov,
                 type = tipo,
                 level = level,
                 download_path = paste0(url, file))
}


# library(tidyverse)
# library(sf)
# files <- list.files("geo", recursive = TRUE, full.names = TRUE)
# create_metadata(files[12])
# metadata <- purrr::map_dfr(files,
#                        ~create_metadata(.))
# metadata <- readxl::read_excel("metadata_geoperu.xlsx")
# metadata

