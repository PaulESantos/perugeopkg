#' Generar metadatos para descargar datos geográficos del Perú
#'
#' Esta función lee los archivos gpkg contenidos en una carpeta y genera la metadata necesaria
#' para descargar los datos de la información geográfica del Perú, incluyendo departamentos, provincias y distritos.
#'
#' @param directory La ruta del directorio que contiene los archivos gpkg.
#' @return Un dataframe de metadatos que especifica la ubicación de los archivos gpkg y otra información relevante.
#'
#' @export
#'
generate_metadata <- function(directory) {
  # Obtener lista de archivos en el directorio especificado
  files <- list.files(directory, recursive = TRUE, full.names = TRUE)

  # Crear la metadata de los departamentos
  departamentos <- grep("/dep", files, value = TRUE)
  make_metadata_dep <- function(file) {
    dff <- sf::read_sf(file)
    dep <- unique(dff$departamento)
    if(grepl("simplified", file)) {
      tipo <- "simplified"
    } else {
      tipo <- "complete"
    }
    url <- "https://github.com/PaulESantos/perugeopkg/raw/master/"
    tibble::tibble(dep_name = dep,
                   type = tipo,
                   download_path  = paste0(url, file))
  }
  dep_metadata <- purrr::map_dfr(departamentos, ~make_metadata_dep(.))

  # Crear la metadata de las provincias
  provincias <- grep("/prov", files, value = TRUE)
  make_metadata_prov <- function(file) {
    dff <- sf::read_sf(file)
    dep <- unique(dff$departamento)
    prov <- unique(dff$provincia)
    if(grepl("simplified", file)) {
      tipo <- "simplified"
    } else {
      tipo <- "complete"
    }
    url <- "https://github.com/PaulESantos/perugeopkg/raw/master/"
    tibble::tibble(dep_name = dep,
                   prov_name = prov,
                   type = tipo,
                   download_path  = paste0(url, file))
  }
  prov_metadata <- purrr::map_dfr(provincias, ~make_metadata_prov(.)) |>
    tidyr::fill(dep_name, .direction = "down")

  # Crear la metadata para los shapes nacionales
  nacional <- tibble::tibble(
    layer = c("PERU", "PERU"),
    type = c("complete", "simplified"),
    download_path = c("https://github.com/PaulESantos/perugeopkg/raw/master/nacional/peru.gpkg",
                      "https://github.com/PaulESantos/perugeopkg/raw/master/nacional/peru_simplified.gpkg")
  )

  # Combinar los archivos de metadata
  metadata <- dplyr::bind_rows(dep_metadata, prov_metadata, nacional) |>
    dplyr::relocate(prov_name, .after = "dep_name") |>
    dplyr::mutate(prov_name = dplyr::if_else(is.na(prov_name), "nill", prov_name)) |>
    dplyr::mutate(layer = dplyr::case_when(
      !is.na(dep_name) ~ dep_name,
      !is.na(prov_name) ~ prov_name,
      TRUE ~ layer
    )) |>
    dplyr::relocate(layer)

  return(metadata)
}
