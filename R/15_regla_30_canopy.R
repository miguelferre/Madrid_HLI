#' Regla 30 del 3-30-300 — "30 % de cubierta arbórea en el entorno".
#'
#' Calculamos para cada nodo de la red peatonal el % de superficie cubierta
#' por arbolado en un buffer de 250 m. Un nodo cumple la regla 30 si ese
#' porcentaje es ≥ 30 %. Después agregamos el % de nodos que cumplen por
#' barrio.
#'
#' Fuente: ESA WorldCover 2021 v200 (10 m, clase 10 = Tree cover). Tile
#' N39W006 cubre todo el municipio de Madrid.
#'
#' Estrategia (rápida):
#'   1. Recortar el tile a Madrid + margen 500 m, reproyectar a EPSG:25830 a 10 m.
#'   2. Binarizar (tree = clase 10) → 1/0.
#'   3. focal con kernel circular radio 250 m → para cada píxel, fracción
#'      de cubierta arbórea en su buffer 250 m.
#'   4. extract puntual sobre los nodos peatonales (ya están en 25830).
#'   5. Agregar % nodos con canopy ≥ 30 % por barrio.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(terra)
})

source("R/utils.R")

RASTER_IN <- "data/raw/canopy/ESA_WorldCover_N39W006.tif"
RASTER_OUT <- "data/processed/canopy_pct_250m.tif"
RADIO_M <- 250
UMBRAL_PCT <- 30

# 1. Recorte + reproyección ----------------------------------------------------

municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)
bbox_buf <- st_bbox(st_buffer(municipio, 500))   # margen 500 m

cat("Cargando raster ESA WorldCover...\n")
r0 <- rast(RASTER_IN)
cat("  CRS origen:", crs(r0, proj = TRUE), "\n  res:", res(r0), "\n")

# Recorte en lat/lon primero (más barato)
mun_ll <- st_transform(municipio, 4326)
r1 <- crop(r0, vect(st_buffer(mun_ll, 0.01)))
cat("  Recortado:", dim(r1), "\n")

# Reproyectar a EPSG:25830 a 10m
template <- rast(crs = "EPSG:25830",
                 ext = ext(bbox_buf[c("xmin","xmax","ymin","ymax")]),
                 resolution = 10)
cat("Reproyectando a EPSG:25830 res 10m (target:", dim(template)[1:2], ")...\n")
r2 <- project(r1, template, method = "near")
cat("  raster proyectado.  vals únicos:", paste(unique(values(r2, na.rm = TRUE))[1:10], collapse=","), "\n")

# 2. Binarizar -----------------------------------------------------------------

tree <- r2 == 10        # ESA WorldCover clase 10 = Tree cover
tree <- subst(tree, NA, 0)
cat("\nFracción global de tree-cover en bbox:", round(global(tree, "mean", na.rm = TRUE)[1, 1], 3), "\n")

# 3. Focal sum con ventana circular radio 250m ---------------------------------

# kernel circular: radio 25 píxeles (250 m)
r_pix <- 25L
k <- matrix(0, nrow = 2*r_pix+1, ncol = 2*r_pix+1)
center <- r_pix + 1
for (i in seq_len(nrow(k))) for (j in seq_len(ncol(k))) {
  if (sqrt((i-center)^2 + (j-center)^2) <= r_pix) k[i, j] <- 1
}
n_kernel <- sum(k)
cat("Kernel circular radio 250m: ", nrow(k), "x", ncol(k),
    " px, ", n_kernel, " válidas (área ≈", round(n_kernel*100/1e4, 2), "ha)\n")

cat("\nAplicando focal sum (puede tardar 1-3 min)...\n")
canopy_sum <- focal(tree, w = k, fun = "sum", na.policy = "omit", na.rm = TRUE)
canopy_pct <- 100 * canopy_sum / n_kernel
names(canopy_pct) <- "canopy_pct_250m"

writeRaster(canopy_pct, RASTER_OUT, overwrite = TRUE,
            datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
cat("✓ raster guardado:", RASTER_OUT, "  size:", round(file.size(RASTER_OUT)/1e6, 1), "MB\n")

# 4. Extract sobre nodos peatonales -------------------------------------------

nodos <- st_read("data/processed/red_peatonal_nodos.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)
cat("\nExtrayendo canopy en", nrow(nodos), "nodos...\n")

vals <- terra::extract(canopy_pct, vect(nodos))[, 2]
nodos$canopy_pct_250m <- vals
nodos$regla_30 <- !is.na(vals) & vals >= UMBRAL_PCT

cat("Nodos con regla 30 cumplida (≥30 % canopy en buffer 250m):",
    sum(nodos$regla_30, na.rm = TRUE), "/", nrow(nodos),
    sprintf(" (%.1f%%)\n", 100 * mean(nodos$regla_30, na.rm = TRUE)))
cat("Resumen canopy_pct_250m:\n")
print(summary(nodos$canopy_pct_250m))

st_write(nodos, "data/processed/red_peatonal_nodos.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

# 5. Agregar a barrios --------------------------------------------------------

barrios <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)

nodos_barrio <- st_join(nodos[, c("regla_30", "canopy_pct_250m")],
                          barrios[, "COD_DISBAR"], join = st_within)

agg <- nodos_barrio |>
  st_drop_geometry() |>
  filter(!is.na(COD_DISBAR)) |>
  group_by(COD_DISBAR) |>
  summarise(
    regla_30_pct  = round(100 * mean(regla_30, na.rm = TRUE), 1),
    canopy_med    = round(median(canopy_pct_250m, na.rm = TRUE), 1),
    .groups = "drop"
  )

barrios <- barrios |> left_join(agg, by = "COD_DISBAR")

cat("\n=== Regla 30 por barrio ===\n")
print(summary(barrios$regla_30_pct))

cat("\nTop 10 barrios — % nodos con ≥30% canopy en buffer 250m:\n")
print(barrios |> st_drop_geometry() |>
        arrange(desc(regla_30_pct)) |> head(10) |>
        select(NOMBRE, NOMDIS, regla_30_pct, canopy_med, dens_arboles_ha) |>
        mutate(dens_arboles_ha = round(dens_arboles_ha, 1)) |>
        as.data.frame(), row.names = FALSE)

cat("\nBottom 10 barrios:\n")
print(barrios |> st_drop_geometry() |>
        arrange(regla_30_pct) |> head(10) |>
        select(NOMBRE, NOMDIS, regla_30_pct, canopy_med, dens_arboles_ha) |>
        mutate(dens_arboles_ha = round(dens_arboles_ha, 1)) |>
        as.data.frame(), row.names = FALSE)

st_write(barrios, "data/processed/hli_barrios.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
cat("\n✓ hli_barrios.gpkg enriquecido con regla_30_pct + canopy_med.\n")
