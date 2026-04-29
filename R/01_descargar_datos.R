#' Descarga y limpieza de los datos administrativos base de Madrid.
#'
#' Salidas en data/processed/:
#'   - distritos.gpkg  (21 distritos del término municipal)
#'   - barrios.gpkg    (131 barrios)
#'   - municipio.gpkg  (envolvente del término municipal, derivado)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(fs)
})

source("R/utils.R")

dir_create("data/raw", recurse = TRUE)
dir_create("data/processed", recurse = TRUE)

# Fuente: portal de datos abiertos del Ayuntamiento de Madrid (geoportal IDEAM).
# Verificadas en abril 2026.
URLS <- list(
  distritos = "https://geoportal.madrid.es/fsdescargas/IDEAM_WBGEOPORTAL/LIMITES_ADMINISTRATIVOS/Distritos/Distritos.zip",
  barrios   = "https://geoportal.madrid.es/fsdescargas/IDEAM_WBGEOPORTAL/LIMITES_ADMINISTRATIVOS/Barrios/Barrios.zip"
)

# Descarga + extracción --------------------------------------------------------

descargar_y_extraer <- function(url, nombre) {
  zip_path <- file.path("data/raw", paste0(nombre, ".zip"))
  out_dir  <- file.path("data/raw", nombre)

  if (!file.exists(zip_path)) {
    message("Descargando ", nombre, " desde ", url)
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
  } else {
    message("Ya descargado: ", zip_path)
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  unzip(zip_path, exdir = out_dir, overwrite = TRUE)

  shp <- list.files(out_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
  if (is.na(shp)) stop("No se encontró .shp en ", out_dir)
  shp
}

shp_distritos <- descargar_y_extraer(URLS$distritos, "Distritos")
shp_barrios   <- descargar_y_extraer(URLS$barrios,   "Barrios")

# Lectura ----------------------------------------------------------------------

# Encoding habitual del Ayto: ISO-8859-1 (Latin1)
distritos <- st_read(shp_distritos, options = "ENCODING=UTF-8", quiet = TRUE)
barrios   <- st_read(shp_barrios,   options = "ENCODING=UTF-8", quiet = TRUE)

cat("\n=== Distritos ===\n")
print(distritos, n = 3)
cat("CRS:", st_crs(distritos)$srid, " | filas:", nrow(distritos), "\n")

cat("\n=== Barrios ===\n")
print(barrios, n = 3)
cat("CRS:", st_crs(barrios)$srid, " | filas:", nrow(barrios), "\n")

# Reproyección a CRS métrico oficial (EPSG:25830) y validación -----------------

if (st_crs(distritos)$srid != "EPSG:25830") {
  distritos <- st_transform(distritos, CRS_METRICO)
}
if (st_crs(barrios)$srid != "EPSG:25830") {
  barrios <- st_transform(barrios, CRS_METRICO)
}

distritos <- st_make_valid(distritos)
barrios   <- st_make_valid(barrios)

# Derivar municipio (unión de distritos)
municipio <- st_union(distritos) |> st_make_valid() |> st_sf(nombre = "Madrid", geometry = _)

# Guardado --------------------------------------------------------------------

st_write(distritos, "data/processed/distritos.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(barrios,   "data/processed/barrios.gpkg",   delete_dsn = TRUE, quiet = TRUE)
st_write(municipio, "data/processed/municipio.gpkg", delete_dsn = TRUE, quiet = TRUE)

cat("\n=== Resumen final ===\n")
cat("Distritos:", nrow(distritos), "(esperado: 21)\n")
cat("Barrios:  ", nrow(barrios),   "(esperado: 131)\n")
cat("Área municipio:", round(as.numeric(st_area(municipio)) / 1e6, 1), "km² (esperado ~604)\n")

cat("\nGuardado en data/processed/\n")
