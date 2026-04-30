#' Agregación de los indicadores HLI a barrios + ranking compuesto.
#'
#' Por cada barrio (131) calculamos:
#'   - densidad de comida saludable     (POIs / km²)
#'   - densidad de gimnasios y deporte  (POIs / km²)
#'   - densidad de fast food            (POIs / km²) — factor inverso
#'   - cubierta de parques              (% área del barrio)
#'
#' Cada indicador se normaliza al rango [0, 1] usando rango robusto
#' (percentiles 5–95) para que un outlier no aplane el resto. El fast
#' food se invierte (1 - x). El HLI es la media simple de los 4
#' componentes — los pesos son explícitos en R/utils.R::PESOS_HLI por
#' si más adelante queremos calibrarlos.
#'
#' Salidas en data/processed/:
#'   - hli_barrios.gpkg  (todos los indicadores + HLI + ranking)
#'   - hli_barrios.csv   (sin geometría, para inspección rápida)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

source("R/utils.R")

PESOS_HLI <- c(comida = 0.25, deporte = 0.25, parques = 0.25, fast_food = 0.25)

# Carga ------------------------------------------------------------------------

barrios   <- st_read("data/processed/barrios.gpkg",          quiet = TRUE) |>
  st_transform(CRS_METRICO)
comida    <- st_read("data/processed/osm_comida_saludable.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)
gimnasios <- st_read("data/processed/osm_gimnasios.gpkg",        quiet = TRUE) |>
  st_transform(CRS_METRICO)
fast_food <- st_read("data/processed/osm_fast_food.gpkg",        quiet = TRUE) |>
  st_transform(CRS_METRICO)
parques   <- st_read("data/processed/osm_parques.gpkg",          quiet = TRUE) |>
  st_transform(CRS_METRICO)

cat(sprintf("Barrios: %d · POIs: comida=%d, deporte=%d, fastfood=%d, parques=%d polígonos\n",
            nrow(barrios), nrow(comida), nrow(gimnasios), nrow(fast_food), nrow(parques)))

# Área del barrio (km²) — recalculamos para usar la geometría real, no Area
barrios$area_km2 <- as.numeric(st_area(barrios)) / 1e6

# Conteo de POIs por barrio ----------------------------------------------------

contar_pois <- function(barrios, pois) {
  inter <- st_intersects(barrios, pois)
  lengths(inter)
}

barrios$n_comida   <- contar_pois(barrios, comida)
barrios$n_deporte  <- contar_pois(barrios, gimnasios)
barrios$n_fastfood <- contar_pois(barrios, fast_food)

barrios$dens_comida   <- barrios$n_comida   / barrios$area_km2
barrios$dens_deporte  <- barrios$n_deporte  / barrios$area_km2
barrios$dens_fastfood <- barrios$n_fastfood / barrios$area_km2

# % cubierta de parques --------------------------------------------------------
# Recortamos los parques al barrio para que polígonos que cruzan límites no
# inflen el porcentaje de barrios pequeños.

parques_union <- st_union(parques)
inter_parques <- st_intersection(barrios["COD_DISBAR"], parques_union)
inter_parques$area_park_m2 <- as.numeric(st_area(inter_parques))
parques_por_barrio <- inter_parques |>
  st_drop_geometry() |>
  group_by(COD_DISBAR) |>
  summarise(area_park_m2 = sum(area_park_m2), .groups = "drop")

barrios <- barrios |>
  left_join(parques_por_barrio, by = "COD_DISBAR") |>
  mutate(area_park_m2 = ifelse(is.na(area_park_m2), 0, area_park_m2),
         pct_parques  = 100 * area_park_m2 / (area_km2 * 1e6))

# Normalización robusta a [0,1] ------------------------------------------------

normalizar_robusto <- function(x, p_lo = 0.05, p_hi = 0.95) {
  q <- quantile(x, c(p_lo, p_hi), na.rm = TRUE)
  if (diff(q) == 0) return(rep(0, length(x)))
  z <- (x - q[1]) / (q[2] - q[1])
  pmin(pmax(z, 0), 1)
}

barrios$z_comida   <- normalizar_robusto(barrios$dens_comida)
barrios$z_deporte  <- normalizar_robusto(barrios$dens_deporte)
barrios$z_parques  <- normalizar_robusto(barrios$pct_parques)
barrios$z_fastfood <- 1 - normalizar_robusto(barrios$dens_fastfood)  # invertido

# HLI compuesto + ranking ------------------------------------------------------

barrios$HLI <- with(barrios,
  PESOS_HLI["comida"]    * z_comida   +
  PESOS_HLI["deporte"]   * z_deporte  +
  PESOS_HLI["parques"]   * z_parques  +
  PESOS_HLI["fast_food"] * z_fastfood
)

barrios$ranking <- rank(-barrios$HLI, ties.method = "min")

# Guardado ---------------------------------------------------------------------

cols_finales <- c("CODDIS", "NOMDIS", "COD_DISBAR", "NOMBRE", "area_km2",
                  "n_comida", "n_deporte", "n_fastfood",
                  "dens_comida", "dens_deporte", "dens_fastfood",
                  "pct_parques",
                  "z_comida", "z_deporte", "z_parques", "z_fastfood",
                  "HLI", "ranking")

salida <- barrios[, cols_finales]
st_write(salida, "data/processed/hli_barrios.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

salida_csv <- salida |> st_drop_geometry() |>
  mutate(across(c(area_km2, dens_comida, dens_deporte, dens_fastfood,
                  pct_parques, z_comida, z_deporte, z_parques, z_fastfood, HLI),
                \(x) round(x, 3)))
write_csv(salida_csv, "data/processed/hli_barrios.csv")

# Reporte ----------------------------------------------------------------------

cat("\n=== Top 10 barrios por HLI ===\n")
print(salida_csv |> arrange(ranking) |> head(10) |>
        select(ranking, NOMBRE, NOMDIS, HLI, dens_comida, dens_deporte, pct_parques, dens_fastfood) |>
        as.data.frame(), row.names = FALSE)

cat("\n=== Bottom 10 barrios por HLI ===\n")
print(salida_csv |> arrange(desc(ranking)) |> head(10) |>
        select(ranking, NOMBRE, NOMDIS, HLI, dens_comida, dens_deporte, pct_parques, dens_fastfood) |>
        as.data.frame(), row.names = FALSE)

cat("\n=== Resumen agregado por distrito ===\n")
print(salida_csv |> group_by(NOMDIS) |>
        summarise(n_barrios = n(), HLI_medio = round(mean(HLI), 3),
                  .groups = "drop") |>
        arrange(desc(HLI_medio)) |> as.data.frame(), row.names = FALSE)

cat(sprintf("\n✓ HLI por barrio guardado: data/processed/hli_barrios.{gpkg,csv}\n"))
