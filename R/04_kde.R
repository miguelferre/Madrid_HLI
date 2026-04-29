#' Estimación de densidad por kernel (KDE) en CRS proyectado.
#'
#' Mejora metodológica frente al notebook original:
#'   - KDE en EPSG:25830 (metros), no en grados (que distorsiona).
#'   - Bandwidth automático (regla de Scott) en lugar de n=300 fijo.
#'   - Cálculo con spatstat (estadísticamente correcto para procesos puntuales).
#'   - Recortado al término municipal real.
#'
#' Salidas en data/processed/:
#'   - kde_comida_saludable.tif
#'   - kde_gimnasios.tif
#'   - kde_fast_food.tif

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(spatstat.geom)
  library(spatstat.explore)
  library(dplyr)
})

source("R/utils.R")

municipio   <- st_read("data/processed/municipio.gpkg", quiet = TRUE)
municipio_m <- st_transform(municipio, CRS_METRICO)
ventana     <- as.owin(st_geometry(municipio_m))

# Resolución del raster: 100 m. Madrid municipio ≈ 30 km × 30 km → ~300 × 300 celdas.
RES_M <- 100

#' Calcula KDE proyectado de una capa de POIs sobre Madrid.
#' Devuelve un SpatRaster (terra) con valores de densidad (POIs / km²).
calcular_kde <- function(ruta_gpkg, etiqueta) {
  if (!file.exists(ruta_gpkg)) {
    warning("No existe: ", ruta_gpkg); return(NULL)
  }
  pts <- st_read(ruta_gpkg, quiet = TRUE) |> st_transform(CRS_METRICO)
  pts <- pts[!st_is_empty(pts), ]
  cat(sprintf("→ %s: %d puntos\n", etiqueta, nrow(pts)))

  coords <- st_coordinates(pts)
  ppp_obj <- ppp(coords[, 1], coords[, 2], window = ventana, checkdup = FALSE)

  # Bandwidth de Scott — robusto y rápido para datos urbanos
  sigma <- bw.scott(ppp_obj)
  cat(sprintf("  bandwidth Scott: σ_x=%.0f m, σ_y=%.0f m\n", sigma[1], sigma[2]))

  dens <- density.ppp(
    ppp_obj,
    sigma = mean(sigma),
    eps = RES_M,
    edge = TRUE,
    positive = TRUE
  )

  # Convertir a SpatRaster con CRS y escalado a POIs/km²
  r <- rast(dens) * 1e6
  crs(r) <- "EPSG:25830"
  names(r) <- etiqueta

  # Recortar al municipio
  r <- mask(r, vect(municipio_m))
  r
}

# Cálculo --------------------------------------------------------------------

kde_alimentacion <- calcular_kde("data/processed/osm_comida_saludable.gpkg", "comida_saludable")
kde_gimnasios    <- calcular_kde("data/processed/osm_gimnasios.gpkg",        "gimnasios")
kde_fastfood     <- calcular_kde("data/processed/osm_fast_food.gpkg",        "fast_food")

# Guardar como GeoTIFF -------------------------------------------------------

if (!is.null(kde_alimentacion)) writeRaster(kde_alimentacion, "data/processed/kde_comida_saludable.tif", overwrite = TRUE)
if (!is.null(kde_gimnasios))    writeRaster(kde_gimnasios,    "data/processed/kde_gimnasios.tif",        overwrite = TRUE)
if (!is.null(kde_fastfood))     writeRaster(kde_fastfood,     "data/processed/kde_fast_food.tif",        overwrite = TRUE)

# Resumen --------------------------------------------------------------------

cat("\n=== Resumen KDE (POIs/km²) ===\n")
for (r in list(kde_alimentacion, kde_gimnasios, kde_fastfood)) {
  if (is.null(r)) next
  cat(sprintf("  %-20s  max=%6.1f   media=%5.2f   p95=%5.1f\n",
              names(r),
              global(r, "max", na.rm = TRUE)[[1]],
              global(r, "mean", na.rm = TRUE)[[1]],
              global(r, fun = function(x) quantile(x, 0.95, na.rm = TRUE))[[1]]))
}
