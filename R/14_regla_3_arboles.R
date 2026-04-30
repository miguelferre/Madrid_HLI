#' Regla 3 del 3-30-300 — "ver al menos 3 árboles desde casa".
#'
#' Para cada nodo de la red peatonal de Madrid (≈ 605 K) contamos cuántos
#' árboles del inventario municipal hay en un buffer de 50 m. Un nodo
#' "cumple la regla 3" si tiene 3 o más árboles a ≤ 50 m. Después agregamos
#' el % de nodos que cumplen la regla por barrio (proxy del % de hogares /
#' personas con la regla satisfecha en su entorno inmediato).
#'
#' Justificación del radio: la formulación de Cecil Konijnendijk se refiere
#' a "ver tres árboles desde la ventana"; 50 m equivale aproximadamente al
#' alcance de la línea de visión desde la fachada de un edificio típico.
#'
#' Implementación: rasterize → focal sum → extract puntual. Es O(n) sobre
#' el raster en lugar de O(n²) sobre las geometrías. `st_is_within_distance`
#' con 605 K × 793 K es inviable.
#'
#' Salidas:
#'   - data/processed/red_peatonal_nodos.gpkg enriquecido con n_arb_50m y regla_3
#'   - hli_barrios.gpkg enriquecido con regla_3_pct y n_arb_50m_med

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(terra)
})

source("R/utils.R")

RES_M <- 10        # tamaño de píxel del conteo de árboles
RADIO_M <- 50
UMBRAL_ARB <- 3

# Carga -----------------------------------------------------------------------

nodos <- st_read("data/processed/red_peatonal_nodos.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)
arboles <- st_read("data/processed/arbolado_madrid.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)
municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)
cat("Nodos:", nrow(nodos), "·  Árboles:", nrow(arboles), "\n")

# 1. Raster vacío con extent del municipio + margen ---------------------------

bbox_buf <- st_bbox(st_buffer(municipio, RADIO_M + 50))
template <- rast(crs = "EPSG:25830",
                 ext = ext(bbox_buf[c("xmin","xmax","ymin","ymax")]),
                 resolution = RES_M)
cat("Raster template:", dim(template)[1:2], "px ·  res", RES_M, "m\n")

# 2. Rasterize: conteo de árboles por píxel -----------------------------------

cat("Rasterizando árboles (conteo por píxel)...\n")
arb_v <- vect(arboles)
r_count <- rasterize(arb_v, template, fun = "count", background = 0)
cat("  máx árboles/píxel:", as.integer(global(r_count, "max", na.rm = TRUE)[1, 1]),
    "·  media:", round(global(r_count, "mean", na.rm = TRUE)[1, 1], 4), "\n")

# 3. Focal sum con kernel circular radio 50 m ---------------------------------

r_pix <- as.integer(RADIO_M / RES_M)
k <- matrix(0, nrow = 2*r_pix+1, ncol = 2*r_pix+1)
center <- r_pix + 1
for (i in seq_len(nrow(k))) for (j in seq_len(ncol(k))) {
  if (sqrt((i-center)^2 + (j-center)^2) <= r_pix) k[i, j] <- 1
}
cat("Kernel circular: ", nrow(k), "x", ncol(k), " px ·  válidas:", sum(k), "\n")

cat("Aplicando focal sum (suma de árboles en buffer 50m)...\n")
r_arb <- focal(r_count, w = k, fun = "sum", na.policy = "omit", na.rm = TRUE)
names(r_arb) <- "n_arb_50m"
cat("  máx árboles/buffer:",
    as.integer(global(r_arb, "max", na.rm = TRUE)[1, 1]),
    "·  media:",
    round(global(r_arb, "mean", na.rm = TRUE)[1, 1], 2), "\n")

# 4. Extract puntual sobre nodos ----------------------------------------------

cat("Extrayendo conteo en", nrow(nodos), "nodos...\n")
vals <- terra::extract(r_arb, vect(nodos))[, 2]
vals[is.na(vals)] <- 0L

nodos$n_arb_50m <- as.integer(vals)
nodos$regla_3   <- nodos$n_arb_50m >= UMBRAL_ARB

cat("\nNodos con regla 3 cumplida (≥3 árboles a ≤50m):",
    sum(nodos$regla_3), "/", nrow(nodos),
    sprintf(" (%.1f%%)\n", 100 * mean(nodos$regla_3)))
cat("Resumen n_arb_50m por nodo:\n")
print(summary(nodos$n_arb_50m))

st_write(nodos, "data/processed/red_peatonal_nodos.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

# 5. Agregación por barrio ----------------------------------------------------

barrios <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)

nodos_barrio <- st_join(nodos[, c("regla_3", "n_arb_50m")],
                          barrios[, "COD_DISBAR"], join = st_within)

agg <- nodos_barrio |>
  st_drop_geometry() |>
  filter(!is.na(COD_DISBAR)) |>
  group_by(COD_DISBAR) |>
  summarise(
    regla_3_pct    = round(100 * mean(regla_3, na.rm = TRUE), 1),
    n_arb_50m_med  = as.integer(median(n_arb_50m, na.rm = TRUE)),
    .groups = "drop"
  )

barrios <- barrios |> left_join(agg, by = "COD_DISBAR")

cat("\n=== Regla 3 por barrio ===\n")
print(summary(barrios$regla_3_pct))

cat("\nTop 10 barrios — % nodos con ≥3 árboles a 50m:\n")
print(barrios |> st_drop_geometry() |>
        arrange(desc(regla_3_pct)) |> head(10) |>
        select(NOMBRE, NOMDIS, regla_3_pct, n_arb_50m_med, dens_arboles_ha) |>
        mutate(dens_arboles_ha = round(dens_arboles_ha, 1)) |>
        as.data.frame(), row.names = FALSE)

cat("\nBottom 10 barrios:\n")
print(barrios |> st_drop_geometry() |>
        arrange(regla_3_pct) |> head(10) |>
        select(NOMBRE, NOMDIS, regla_3_pct, n_arb_50m_med, dens_arboles_ha) |>
        mutate(dens_arboles_ha = round(dens_arboles_ha, 1)) |>
        as.data.frame(), row.names = FALSE)

st_write(barrios, "data/processed/hli_barrios.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
cat("\n✓ hli_barrios.gpkg enriquecido con regla_3_pct + n_arb_50m_med.\n")
