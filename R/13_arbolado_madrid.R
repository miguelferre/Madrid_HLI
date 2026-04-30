#' Procesado del arbolado del Ayto de Madrid (Nivel 3 F · regla 3 del 3-30-300).
#'
#' Fuente: Portal de datos abiertos del Ayto, dataset 300761
#'   "Arbolado en parques y zonas verdes de Madrid (detalle)" — Shape, EPSG:25830,
#'   actualizado 2026-02. Incluye TODOS los árboles bajo conservación municipal:
#'   alineación viaria + zonas verdes + parques históricos / forestales.
#'
#' Pipeline:
#'   1. Leer el SHP completo (`data/raw/arbolado/extracted/ARBOLADO_MADRID.shp`)
#'      conservando sólo las columnas mínimas (geometry + tipo si existe).
#'   2. Recortar al término municipal de Madrid.
#'   3. Calcular conteos y densidad de árboles por barrio.
#'   4. Guardar:
#'        - data/processed/arbolado_madrid.gpkg (un punto por árbol, columnas mínimas)
#'        - actualizar hli_barrios.gpkg con n_arboles, dens_arboles_ha
#'
#' La regla 3 ("3 árboles visibles desde la ventana") se aproxima en R/15 con
#' buffers de 50 m sobre los nodos de la red peatonal.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

source("R/utils.R")

shp <- "data/raw/arbolado/extracted/ARBOLADO_MADRID.shp"
out_gpkg <- "data/processed/arbolado_madrid.gpkg"

# Inspección rápida de columnas y conteo --------------------------------------

info <- st_read(shp, query = "SELECT * FROM ARBOLADO_MADRID LIMIT 5", quiet = TRUE)
cat("Columnas del SHP:\n")
print(names(info))
cat("\nPrimeras filas (sin geometry):\n")
print(st_drop_geometry(info))

n_total <- as.integer(
  st_layers(shp)$features[which(st_layers(shp)$name == "ARBOLADO_MADRID")]
)
cat("\nNº total de árboles en el dataset:", n_total, "\n")

# Carga seleccionando sólo columnas útiles ------------------------------------
# (las columnas exactas dependen de lo que veamos arriba — el script asume las
#  habituales del 300761; si alguna no existe se ignora con tryCatch)

cols_deseadas <- c("ASSETNUM", "NUM_PARQUE", "NUM_DTO", "NBRE_DTO",
                   "NUM_BARRIO", "NBRE_BARRI", "CODIGO_ESP",
                   "PERIMETRO", "ALTURA_TOT")
cols_disponibles <- intersect(cols_deseadas, names(info))
cat("\nColumnas que conservaremos:", paste(cols_disponibles, collapse = ", "), "\n")

cat("\nLeyendo SHP completo (puede tardar 1–2 min por el .dbf de 657 MB)...\n")
arboles <- st_read(shp, quiet = TRUE)
cat("  filas:", nrow(arboles), "·  CRS:", st_crs(arboles)$epsg, "\n")

arboles <- arboles[, c(cols_disponibles, attr(arboles, "sf_column"))]

# Recorte al municipio --------------------------------------------------------

municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)

# Forzar 25830 (algunos prj de Esri no se reconocen como EPSG)
st_crs(arboles) <- CRS_METRICO

idx <- st_intersects(arboles, municipio, sparse = FALSE)[, 1]
arboles <- arboles[idx, ]
cat("\nÁrboles dentro del término municipal:", nrow(arboles), "\n")

st_write(arboles, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
cat("✓ Guardado:", out_gpkg, "\n")

# Agregación a barrios --------------------------------------------------------

barrios <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)
barrios$area_ha <- as.numeric(st_area(barrios)) / 1e4

inter <- st_intersects(barrios, arboles)
barrios$n_arboles      <- lengths(inter)
barrios$dens_arboles_ha <- barrios$n_arboles / barrios$area_ha

cat("\nResumen árboles por barrio:\n")
print(summary(barrios$n_arboles))
cat("\nDensidad árboles/ha:\n")
print(summary(barrios$dens_arboles_ha))

cat("\nTop 10 barrios por nº absoluto de árboles:\n")
print(barrios |> st_drop_geometry() |>
        arrange(desc(n_arboles)) |> head(10) |>
        select(NOMBRE, NOMDIS, area_ha, n_arboles, dens_arboles_ha) |>
        mutate(area_ha = round(area_ha, 1),
               dens_arboles_ha = round(dens_arboles_ha, 1)) |>
        as.data.frame(), row.names = FALSE)

cat("\nTop 10 barrios por densidad árboles/ha:\n")
print(barrios |> st_drop_geometry() |>
        arrange(desc(dens_arboles_ha)) |> head(10) |>
        select(NOMBRE, NOMDIS, area_ha, n_arboles, dens_arboles_ha) |>
        mutate(area_ha = round(area_ha, 1),
               dens_arboles_ha = round(dens_arboles_ha, 1)) |>
        as.data.frame(), row.names = FALSE)

cat("\nBottom 10 barrios por densidad árboles/ha:\n")
print(barrios |> st_drop_geometry() |>
        arrange(dens_arboles_ha) |> head(10) |>
        select(NOMBRE, NOMDIS, area_ha, n_arboles, dens_arboles_ha) |>
        mutate(area_ha = round(area_ha, 1),
               dens_arboles_ha = round(dens_arboles_ha, 1)) |>
        as.data.frame(), row.names = FALSE)

# Guardar barrios enriquecidos ------------------------------------------------

st_write(barrios, "data/processed/hli_barrios.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

cat("\n✓ hli_barrios.gpkg enriquecido con n_arboles + dens_arboles_ha.\n")
