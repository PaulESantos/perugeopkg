#' library(tidyverse)
#' library(sf)
#'
#' files <- list.files("anp",
#'                     pattern = "\\.shp$",
#'                     recursive = TRUE,
#'                     full.names = TRUE)
#' files
#' lista <- data.table::fread("anp/ANP-ACR-ACP-ZR-dataset_v20230920.csv",
#'                            encoding = "UTF-8")
#'
#' anp_def <- sf::read_sf(files[1]) |>
#'   dplyr::mutate(anp_nombre = stringr::str_remove_all(anp_nomb,
#'                                      paste0(c(
#'                                        "^Sistema de Islas, Islotes y Puntas Guaneras - ",
#'                                        "^de la ",
#'                                        "^del ",
#'                                        "^la ",
#'                                        "^de "
#'                                      ),
#'                                      collapse = "|"))) |>
#'   dplyr::select(anp_cate, anp_nombre, anp_sect, anp_ubpo)
#'
#'
#' by_nombre <- anp_def |>
#'   group_split(anp_cate, anp_nombre)
#'
#' length(by_nombre)
#'
#' contar_nombre <- function(df){
#'   nombre <- df$anp_nombre
#'   numero <- length(nombre)
#'   return(numero)
#' }
#'
#' map(by_nombre,
#'     ~contar_nombre(.)) |>
#'   unlist() |>
#'   as.vector()
#'
#'
#'
#' create_gpkg <- function(df, folder){
#'   # categoria
#'   categoria <- df$anp_cate |> unique()
#'   if(categoria == "Parque Nacional"){
#'     cate <- "pn"
#'   }
#'   else if(categoria == "Bosque de Protección"){
#'     cate <- "bp"
#'   }
#'   else if(categoria == "Reserva Nacional"){
#'     cate <- "rn"
#'   }
#'   else if(categoria == "Reserva Comunal"){
#'     cate <- "rc"
#'   }
#'   else if(categoria == "Coto de Caza"){
#'     cate <- "cc"
#'   }
#'   else if(categoria == "Reserva Paisajistica"){
#'     cate <- "rp"
#'   }
#'   else if(categoria == "Santuario Histórico"){
#'     cate <- "sh"
#'   }
#'   else if(categoria == "Refugio de Vida Silvestre"){
#'     cate <- "rvs"
#'   }
#'   else if(categoria == "Santuario Nacional"){
#'     cate <- "sn"
#'   }
#'
#'   nombre <- df$anp_nombre |> unique()
#'   file_name <- tolower(gsub("\\s+", "_", nombre))
#'   df |>
#'     sf::st_write(paste0("anp/", folder,"/", cate, "_",file_name, ".gpkg"))
#' }
#'
#' by_nombre |>
#'   map(~create_gpkg(., folder = "gpkg"))
#'
#' anp_def
#' # areas de conservacion regional ------------------------------------------
acr_def <- sf::read_sf( "anp/AreasdeConservacionRegional.shp") |>
  dplyr::select(acr_nomb,  acr_sect, acr_balec, acr_ubpo) |>
  dplyr::mutate(acr_nombre = stringr::str_squish(acr_nomb) |>  tolower())
acr_def
acr_by_name <- acr_def |>
  dplyr::group_split(acr_nomb)
acr_by_name
acr_by_name[[1]] |>  class()

create_gpkg <- function(sf, folder){
  nombre <-
    sf |>
    dplyr::select("acr_nombre") |>
    dplyr::pull("acr_nombre") |>
    unique()
  #nombre
  nombre <- iconv(nombre, from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "")
  #nombre
  file_name <- tolower(gsub("\\s+", "_", nombre))
  #file_name
  sf::st_write(sf,
     paste0("anp/", "gpkg", "/", "acr_", file_name, ".gpkg"))
}
acr_by_name |>  length()
acr_by_name[[32]] |>
  create_gpkg(folder = "gpkg")
files

#' # areas de conservacion privada -------------------------------------------
#' library(tidyverse)
#' library(sf)
#' acp <- sf::read_sf("anp/AreasdeConservacionPrivada.shp") |>
#'   dplyr::select(acp_nomb, acp_ubpo, acp_balec, acp_tirec,
#'                 acp_tipro, acp_titu, geometry)
#' by_name <- acp |>
#'   dplyr::group_split(acp_nomb)
#' by_name |>  length()
#' df <- by_name[[12]]
#' create_acp_gpkg <- function(sf, folder){
#'   nombre <-  sf$acp_nomb |>  unique()
#'   #nombre
#'   nombre <- iconv(nombre, from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "")
#'   #nombre
#'   file_name <- tolower(gsub("\\s+", "_", nombre))
#'   #file_name
#'   sf::st_write(sf,
#'                paste0("anp/", "gpkg", "/", "acp_", file_name, ".gpkg"))
#' }
#'
#'
#' create_acp_gpkg <- function(sf, folder) {
#'   if (!inherits(sf, "sf")) {
#'     clase <- class(sf)
#'     stop(paste0("El argumento 'sf' de clase:", clase ,".Debe ser un objeto de clase 'sf'"))
#'   }
#'   # Verificar si la columna acp_nomb está presente
#'   if (!"acp_nomb" %in% names(sf)) {
#'     stop("La columna 'acp_nomb' no está presente en el objeto 'sf'")
#'   }
#'
#'   nombre <- sf$acp_nomb |> unique()
#'   nombre <- iconv(nombre, from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "")
#'   nombre <- tm::removePunctuation(nombre) |>
#'     stringr::str_squish() |>
#'     stringr::str_trim()
#'   file_name <- tolower(gsub("\\s", "_", nombre)) |>
#'     stringr::str_squish()
#'
#'   sf::st_write(sf, paste0("anp/", folder, "/acp_", file_name, ".gpkg"))
#' }
#'
#' by_name |> length()
#' by_name[[3]]
#'
#' for (i in 1:140) {
#'   create_acp_gpkg(by_name[[i]], folder = "gpkg")
#' }
#'
#' files_acp <- list.files("anp/gpkg",
#'                         pattern = "^acp_", full.names = TRUE)
#' files_acp
#' #file.remove(files_acp)
#'
#' # zonas reservadas --------------------------------------------------------
#'
#'
#' zonas_res <- sf::read_sf("anp/ZonasReservadas.shp") |>
#'   dplyr::select(zr_nomb, zr_sect = anp_sect, zr_ubpo, zr_balec, geometry)
#' zonas_res
#' por_nombre <- zonas_res |>
#'   dplyr::group_split(zr_nomb)
#' length(por_nombre)
#' por_nombre
#'
#'
#' create_zr_gpkg <- function(sf, folder) {
#'   if (!inherits(sf, "sf")) {
#'     clase <- class(sf)
#'     stop(paste0("El argumento 'sf' de clase:", clase ,".Debe ser un objeto de clase 'sf'"))
#'   }
#'   # Verificar si la columna acp_nomb está presente
#'   if (!"zr_nomb" %in% names(sf)) {
#'     stop("La columna 'acp_nomb' no está presente en el objeto 'sf'")
#'   }
#'
#'   nombre <- sf$zr_nomb |> unique()
#'   nombre <- iconv(nombre, from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "")
#'   file_name <- tolower(gsub(" ", "_", nombre)) |>
#'     stringr::str_squish() |>
#'     stringr::str_replace("-", "")
#'
#'   sf::st_write(sf, paste0("anp/", folder, "/zr_", file_name, ".gpkg"))
#' }
#'
#' for (i in 3:8) {
#'   create_zr_gpkg(por_nombre[[i]], folder = "gpkg")
#' }
#'
#'
#'
#' # create metadata ---------------------------------------------------------
#' files <- list.files("anp",
#'                     full.names = TRUE)
#' files
#' create_anp_metada <- function(path){
#'   df <- sf::read_sf(path)
#'   #get col name var
#'   nombres <- names(df)
#'   var_name <- grep("_nomb", nombres , value = TRUE)
#'   #geta anp name
#'   nombre_anp <- df |>
#'     dplyr::as_tibble() |>
#'     dplyr::select(dplyr::all_of(var_name)) |>
#'     purrr::flatten_chr() |>
#'     unique()
#'   file_name <- basename(path)
#'
#'   #categoria
#'   if(grepl("^pn_", file_name)){
#'     categoria <- "Parque Nacional"
#'   }
#'   else if(grepl("^bp_", file_name)){
#'     categoria <-  "Bosque de Protección"
#'   }
#'   else if(grepl("^rn_", file_name)){
#'     categoria <-  "Reserva Nacional"
#'   }
#'   else if(grepl("^rc_", file_name)){
#'     categoria <-  "Reserva Comunal"
#'   }
#'   else if(grepl("^cc_", file_name)){
#'     categoria <-  "Coto de Caza"
#'   }
#'   else if(grepl("^rp_", file_name)){
#'     categoria <-  "Reserva Paisajistica"
#'   }
#'   else if(grepl("^sh_", file_name)){
#'     categoria <-  "Santuario Histórico"
#'   }
#'   else if(grepl("^rvs_", file_name)){
#'     categoria <-  "Refugio de Vida Silvestre"
#'   }
#'   else if(grepl("^sn_", file_name)){
#'     categoria <-  "Santuario Nacional"
#'   }
#'   else if(grepl("^acp_", file_name)){
#'     categoria <-  "Área de Conservación Privada"
#'   }
#'   else if(grepl("^acr_", file_name)){
#'     categoria <-  "Área de Conservación Regional"
#'   }
#'   else if(grepl("^zr_", file_name)){
#'     categoria <-  "Zona Reservada"
#'   }
#'
#'   tibble::tibble(anp_nombre = nombre_anp,
#'                  anp_categoria = categoria,
#'                  download_path = path)
#' }
#'
#' anp_metadata <- files |>
#' map_dfr(~create_anp_metada(.))
#'
#' dim(anp_metadata)
#' files |>  length()
#'
#' anp_metadata |>
#'   count(download_path) |>
#'   filter(n>1)
#'
#' anp_metadata |>
#'   filter(download_path == "anp/acr_angostura-faical.gpkg")
#'
#' anp_data_2 <- anp_metadata |>
#'   filter(str_detect(anp_nombre, "^[A-Z]"))
#'
#' anp_data_2
#' metadata_anp <- anp_data_2|>
#'   mutate(anp_nombre = iconv(anp_nombre,
#'                             from = 'UTF-8',
#'                             to = 'ASCII//TRANSLIT',
#'                             sub = "")) |>
#'   mutate(download_path = basename(download_path),
#'          download_path = paste0("https://github.com/PaulESantos/perugeopkg/raw/master/anp/",
#'                                 download_path))
#'
#' metadata_anp <- readxl::read_excel("metadata_anps.xlsx") |>
#'   dplyr::mutate(anp_nombre = toupper(anp_nombre)) |>
#'   dplyr::mutate(anp_nombre = if_else(anp_nombre == "Q'EROS KOSNIPATA",
#'                                      "QEROS KOSNIPATA",
#'                                      anp_nombre))
#' metadata_anp |>
#'   write_csv("metadata_anp.csv")
#'
#'
#'
#' dplyr::setdiff(files, anp_data_2$download_path)
#'
#' df <- read_sf( "anp/sn_los_manglares_de_tumbes.gpkg")
#'
#' df |>
#'   dplyr::mutate(anp_nombre = "Manglares de Tumbes") |>
#'   sf::st_write("sn_los_manglares_de_tumbes.gpkg")
#'
