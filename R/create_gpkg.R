library(tidyverse)
library(geoperu)
library(sf)

# -------------------------------------------------------------------------
# data <- sf::st_read("https://github.com/PaulESantos/perugeopkg/raw/master/geo/dep/lima.gpkg")
#
#
# departamentos <- data |>
#   dplyr::group_by(departamento) |>
#   dplyr::summarise(geometry = sf::st_union(geometry))
#
# dep_prov <- data |>
#  dplyr::group_by(departamento, provincia) |>
#   dplyr::summarise(geometry = sf::st_union(geometry))
# data
# nested_prov <- data |>
#   dplyr::mutate(tag = paste0(departamento, "_", provincia) |>
#            stringr::str_to_lower() |>
#            stringr::str_replace_all(" ", "_")) |>
#   dplyr::relocate(tag) |>
#   dplyr::group_split(tag)
#
#
# nested_prov[[100]]
# # Exporta los archivos gpkg de los para cada uno de las provincias
# # incluyendo los distritos
# write_gpkg_prov_dist <- function(df, folder = NULL) {
#   tag <- unique(df$tag)
#   # Verificar si se proporciona un directorio y crearlo si no existe
#   if (!is.null(folder)) {
#     if (!dir.exists(folder)) {
#       dir.create(folder, recursive = TRUE)
#     }
#   }
#   # Construir el nombre completo del archivo
#   file_path <- file.path(folder, "/", paste0(tag, ".gpkg"))
#   # Escribir el archivo GeoPackage
#   sf::write_sf(df, file_path)
# }
# nested_prov |>
#   purrr::map(~write_gpkg_prov_dist(.,
#                             folder = "geo/prov"))
#
# #' Exportar los datos de cada una de las provincias mostrando solo la
# #' informacion de la provincia omitiendo los distritos
# #'
# write_gpkg_prov <- function(df, folder = NULL) {
#   tag <- unique(df$tag)
#   # Verificar si se proporciona un directorio y crearlo si no existe
#   if (!is.null(folder)) {
#     if (!dir.exists(folder)) {
#       dir.create(folder, recursive = TRUE)
#     }
#   }
#   # Construir el nombre completo del archivo
#   file_path <- file.path(folder, "/", paste0(tag, "_simplified.gpkg"))
#
#   #crear el archivo simplificado a nivel de provincia
#   df2 <- df |>
#     dplyr::group_by(provincia) |>
#     dplyr::summarise(geometry = sf::st_union(geometry),
#               .groups = "drop"
#     )
#
#   # Escribir el archivo GeoPackage
#   sf::write_sf(df2, file_path)
# }
# nested_prov |>
#   purrr::map(~write_gpkg_prov(.,
#                               folder = "geo/prov"))
# #' Exportar los datos a nivel de departamento incluyendo las provincias
# #'
# nested_dep <- data |>
#   dplyr::group_split(departamento)
# nested_dep
#
# write_gpkg_dep_prov <- function(df, folder = NULL) {
#   tag <- unique(df$departamento)
#   tag <-  gsub("\\s+", "_", tolower(tag))
#   # Verificar si se proporciona un directorio y crearlo si no existe
#   if (!is.null(folder)) {
#     if (!dir.exists(folder)) {
#       dir.create(folder, recursive = TRUE)
#     }
#   }
#   # Construir el nombre completo del archivo
#   file_path <- file.path(folder, "/", paste0(tag, ".gpkg"))
#   df2 <- df |>
#     dplyr::group_by(provincia) |>
#     dplyr::summarise(geometry = sf::st_union(geometry))
#
#   # Escribir el archivo GeoPackage
#   sf::write_sf(df, file_path)
# }
# nested_dep[[3]]
# nested_dep |>
#   purrr::map(~write_gpkg_dep_prov(.,
#                            folder = "geo/dep"))
#
#
# #' Exportar los datos a nivel de departamento
# #'
# nested_dep <- data |>
#   dplyr::group_split(departamento)
# nested_dep
#
# write_gpkg_dep <- function(df, folder = NULL) {
#   tag <- unique(df$departamento)
#   tag <-  gsub("\\s+", "_", tolower(tag))
#   # Verificar si se proporciona un directorio y crearlo si no existe
#   if (!is.null(folder)) {
#     if (!dir.exists(folder)) {
#       dir.create(folder, recursive = TRUE)
#     }
#   }
#   # Construir el nombre completo del archivo
#   file_path <- file.path(folder, "/", paste0(tag, "_simplified.gpkg"))
#   df2 <- df |>
#     dplyr::group_by(departamento) |>
#     dplyr::summarise(geometry = sf::st_union(geometry))
#
#   # Escribir el archivo GeoPackage
#   sf::write_sf(df2, file_path)
# }
# nested_dep[[3]]
# nested_dep |>
#   purrr::map(~write_gpkg_dep(.,
#                       folder = "geo/dep"))
#
# # Exportar shape del pais con y sin departamentos
# data |>
#   sf::write_sf("geo/nacional/peru.gpkg")
#
# data |>
#   dplyr::mutate(country = "Peru") |>
#   dplyr::group_by(country) |>
#   dplyr::summarise(geometry = sf::st_union(geometry)) |>
#   sf::write_sf("geo/nacional/peru_simplified.gpkg")#
