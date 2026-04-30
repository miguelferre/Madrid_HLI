#' Accesibilidad peatonal a parques con `dodgr` — Nivel 3 A.
#'
#' Pipeline:
#'   1. Leer la red de calles del extracto Geofabrik de la Comunidad de Madrid
#'      (`data/raw/madrid-latest.osm.pbf`) y recortarla al término municipal.
#'   2. Construir el grafo peatonal con `weight_streetnet(wt_profile = "foot")`.
#'   3. Para cada parque ≥ 1 ha localizar el nodo de red más cercano.
#'   4. Calcular distancias peatonales desde TODOS los parques a TODOS los
#'      nodos del grafo y conservar el mínimo por nodo (= distancia al parque
#'      ≥ 1 ha más cercano).
#'   5. Marcar los nodos como accesibles a 5/10/15 min andando (a 4,8 km/h
#'      ≈ 80 m/min eso son 400 / 800 / 1.200 m peatonales).
#'   6. Para cada barrio del Ayuntamiento calcular el % de nodos accesibles a
#'      cada umbral (proxy del % de hogares/población con acceso).
#'   7. Enriquecer `data/processed/hli_barrios.gpkg` con tres columnas nuevas
#'      `acc_5min`, `acc_10min`, `acc_15min`.
#'
#' Salida adicional:
#'   data/processed/red_peatonal_nodos.gpkg  (nodos con dist_min al parque
#'                                            más cercano y flags acc_*).

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(osmextract)
  library(dodgr)
})

source("R/utils.R")

UMBRAL_M <- c(acc_5min = 400, acc_10min = 800, acc_15min = 1200)

# 1. Red de calles -------------------------------------------------------------

pbf <- "data/raw/madrid-latest.osm.pbf"
gpkg_red <- "data/processed/red_calles_madrid.gpkg"

if (!file.exists(gpkg_red)) {
  cat("Leyendo PBF y filtrando a highways peatonales...\n")
  red <- oe_read(
    pbf, layer = "lines", quiet = TRUE,
    extra_tags = c("foot", "sidewalk", "footway", "access"),
    query = "SELECT * FROM lines WHERE highway IS NOT NULL"
  )
  cat("  Total líneas con highway:", nrow(red), "\n")

  municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE) |>
    st_transform(st_crs(red))
  red <- red[st_intersects(red, municipio, sparse = FALSE)[, 1], ]
  cat("  Líneas dentro de Madrid municipio:", nrow(red), "\n")
  st_write(red, gpkg_red, delete_dsn = TRUE, quiet = TRUE)
} else {
  cat("Usando red en caché:", gpkg_red, "\n")
}

red <- st_read(gpkg_red, quiet = TRUE)
cat("Red cargada:", nrow(red), "líneas · CRS:", st_crs(red)$epsg, "\n")

# 2. Grafo peatonal ------------------------------------------------------------

cat("\nConstruyendo grafo peatonal con dodgr...\n")
graph <- weight_streetnet(red, wt_profile = "foot",
                            type_col = "highway", id_col = "osm_id")
graph <- graph[graph$component == 1, ]   # nos quedamos con la componente mayor
cat("  Aristas:", nrow(graph), "·  componentes -> conservada la mayor\n")

vertices <- dodgr_vertices(graph)
cat("  Vértices:", nrow(vertices), "\n")

# 3. Parques ≥ 1 ha y nodo más cercano ----------------------------------------

parques <- st_read("data/processed/osm_parques.gpkg", quiet = TRUE)
if (!"area_m2" %in% names(parques)) parques$area_m2 <- as.numeric(st_area(parques))
parques_grandes <- parques[parques$area_m2 >= 10000, ]   # ≥ 1 ha
cat("\nParques ≥ 1 ha:", nrow(parques_grandes), "·",
    round(sum(parques_grandes$area_m2)/1e4), "ha\n")

# Centroides en lat/lon (dodgr trabaja en EPSG:4326)
parques_pts <- st_centroid(st_transform(parques_grandes, CRS_GEOGRAFICO))
park_xy <- sf::st_coordinates(parques_pts)

from_ids <- match_pts_to_verts(vertices, park_xy)
cat("  Nodos de origen únicos para los parques:",
    length(unique(from_ids)), "\n")

# 4. Distancia mínima desde cualquier parque a cada nodo -----------------------

cat("\nCalculando distancias peatonales (dodgr_dists)...\n")
chunks <- split(seq_along(from_ids),
                 ceiling(seq_along(from_ids) / 80))
dist_min <- rep(Inf, nrow(vertices))
v_ids <- vertices$id

for (i in seq_along(chunks)) {
  from_chunk <- unique(from_ids[chunks[[i]]])
  d <- dodgr_dists(graph, from = from_chunk, to = v_ids)
  d[is.na(d)] <- Inf
  dist_min <- pmin(dist_min, apply(d, 2, min))
  cat(sprintf("  chunk %d/%d  (from=%d)  min=%.0f  median=%.0f  max=%.0f\n",
              i, length(chunks), length(from_chunk),
              suppressWarnings(min(dist_min[is.finite(dist_min)])),
              suppressWarnings(median(dist_min[is.finite(dist_min)])),
              suppressWarnings(max(dist_min[is.finite(dist_min)]))))
}
vertices$dist_park_min <- dist_min
for (nm in names(UMBRAL_M)) vertices[[nm]] <- dist_min <= UMBRAL_M[nm]

# 5. Convertir vertices a sf y guardar -----------------------------------------

vert_sf <- st_as_sf(vertices, coords = c("x", "y"), crs = CRS_GEOGRAFICO) |>
  st_transform(CRS_METRICO)
st_write(vert_sf, "data/processed/red_peatonal_nodos.gpkg",
          delete_dsn = TRUE, quiet = TRUE)

# 6. Agregar a barrios ---------------------------------------------------------

barrios <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)

vert_sf_b <- st_join(vert_sf, barrios[, "COD_DISBAR"], join = st_within)

acc_barrio <- vert_sf_b |>
  st_drop_geometry() |>
  filter(!is.na(COD_DISBAR)) |>
  group_by(COD_DISBAR) |>
  summarise(
    n_nodos    = n(),
    acc_5min   = round(100 * mean(acc_5min,  na.rm = TRUE), 1),
    acc_10min  = round(100 * mean(acc_10min, na.rm = TRUE), 1),
    acc_15min  = round(100 * mean(acc_15min, na.rm = TRUE), 1),
    dist_park_med = round(median(dist_park_min, na.rm = TRUE)),
    .groups = "drop"
  )

cat("\n=== Accesibilidad peatonal a parques ≥ 1 ha ===\n")
cat("Resumen acc_5min (% nodos con parque ≥1 ha a ≤5 min):\n")
print(summary(acc_barrio$acc_5min))
cat("Resumen acc_15min:\n")
print(summary(acc_barrio$acc_15min))

# Top y bottom según acc_15min
cat("\nTop 10 barrios por acc_15min:\n")
print(acc_barrio |> arrange(desc(acc_15min)) |> head(10) |>
        left_join(barrios |> st_drop_geometry() |>
                    select(COD_DISBAR, NOMBRE, NOMDIS),
                   by = "COD_DISBAR") |>
        select(NOMBRE, NOMDIS, acc_5min, acc_10min, acc_15min, dist_park_med) |>
        as.data.frame(), row.names = FALSE)

cat("\nBottom 10 barrios por acc_15min:\n")
print(acc_barrio |> arrange(acc_15min) |> head(10) |>
        left_join(barrios |> st_drop_geometry() |>
                    select(COD_DISBAR, NOMBRE, NOMDIS),
                   by = "COD_DISBAR") |>
        select(NOMBRE, NOMDIS, acc_5min, acc_10min, acc_15min, dist_park_med) |>
        as.data.frame(), row.names = FALSE)

# 7. Enriquecer hli_barrios.gpkg ----------------------------------------------

hli_enriquecido <- barrios |>
  left_join(acc_barrio |> select(-n_nodos), by = "COD_DISBAR")

st_write(hli_enriquecido, "data/processed/hli_barrios.gpkg",
          delete_dsn = TRUE, quiet = TRUE)

cat("\n✓ hli_barrios.gpkg enriquecido con acc_5min, acc_10min, acc_15min, dist_park_med.\n")
