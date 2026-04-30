#' Preparación de datos para el dashboard shinylive (Nivel 3 D).
#'
#' Convierte los GPKG procesados a GeoJSON simplificado embebible en una
#' app shinylive (que viaja al navegador como WebAssembly + assets, sin
#' servidor). Objetivo: payload total < 1 MB.
#'
#' Salidas en `app/data/`:
#'   - barrios.geojson      (131 features, todas las métricas, ~150–250 KB)
#'   - distritos.geojson    (21 features, solo borde, ~30–60 KB)
#'   - municipio.geojson    (1 feature, ~10 KB)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(rmapshaper)
})

source("R/utils.R")

dir.create("app/data", recursive = TRUE, showWarnings = FALSE)

# Carga -----------------------------------------------------------------------

barrios   <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE)
distritos <- st_read("data/processed/distritos.gpkg",   quiet = TRUE)
municipio <- st_read("data/processed/municipio.gpkg",   quiet = TRUE)

# Reproyectar a 4326 (formato GeoJSON estándar) -------------------------------

barrios   <- st_transform(barrios,   CRS_GEOGRAFICO)
distritos <- st_transform(distritos, CRS_GEOGRAFICO)
municipio <- st_transform(municipio, CRS_GEOGRAFICO)

# Simplificar geometrías ------------------------------------------------------
# `keep = 0.05` mantiene 5% de los vértices preservando topología compartida.

cat("Simplificando barrios (131 features)...\n")
barrios_s   <- ms_simplify(barrios,   keep = 0.05, keep_shapes = TRUE)
cat("Simplificando distritos (21 features)...\n")
distritos_s <- ms_simplify(distritos, keep = 0.05, keep_shapes = TRUE)
cat("Simplificando municipio...\n")
municipio_s <- ms_simplify(municipio, keep = 0.05, keep_shapes = TRUE)

# Recortar columnas a las imprescindibles + redondear -------------------------

cols_keep <- c("CODDIS", "NOMDIS", "COD_DISBAR", "NOMBRE",
               "area_km2", "n_comida", "n_deporte", "n_fastfood",
               "dens_comida", "dens_deporte", "dens_fastfood",
               "pct_parques", "HLI", "ranking",
               "renta_neta_persona",
               "acc_5min", "acc_10min", "acc_15min",
               "n_arboles", "dens_arboles_ha",
               "regla_3_pct", "regla_30_pct", "regla_300_pct",
               "score_3_30_300", "score_medio_3_30_300", "rank_3_30_300",
               "canopy_med", "n_arb_50m_med")

cols_have <- intersect(cols_keep, names(barrios_s))
barrios_s <- barrios_s[, cols_have]

# Redondear numéricos para reducir tamaño en JSON
num_cols <- setdiff(cols_have, c("CODDIS", "NOMDIS", "COD_DISBAR", "NOMBRE"))
for (cc in num_cols) {
  if (is.numeric(barrios_s[[cc]])) {
    if (cc %in% c("HLI", "score_medio_3_30_300", "score_3_30_300",
                  "regla_3_pct", "regla_30_pct", "regla_300_pct",
                  "acc_5min", "acc_10min", "acc_15min", "canopy_med",
                  "dens_comida", "dens_deporte", "dens_fastfood",
                  "pct_parques", "dens_arboles_ha")) {
      barrios_s[[cc]] <- round(barrios_s[[cc]], 2)
    } else {
      barrios_s[[cc]] <- round(barrios_s[[cc]])
    }
  }
}

distritos_s <- distritos_s[, intersect(c("CODDIS", "NOMDIS"),
                                          names(distritos_s))]

# Escribir GeoJSON ------------------------------------------------------------

st_write(barrios_s,   "app/data/barrios.geojson",   delete_dsn = TRUE,
         layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=5"), quiet = TRUE)
st_write(distritos_s, "app/data/distritos.geojson", delete_dsn = TRUE,
         layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=5"), quiet = TRUE)
st_write(municipio_s, "app/data/municipio.geojson", delete_dsn = TRUE,
         layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=5"), quiet = TRUE)

# Reporte ---------------------------------------------------------------------

f_kb <- function(p) round(file.size(p) / 1024, 1)
cat(sprintf("\n✓ app/data/barrios.geojson   %s KB · %d features · %d cols\n",
            f_kb("app/data/barrios.geojson"),
            nrow(barrios_s), ncol(barrios_s) - 1))
cat(sprintf("✓ app/data/distritos.geojson %s KB · %d features\n",
            f_kb("app/data/distritos.geojson"), nrow(distritos_s)))
cat(sprintf("✓ app/data/municipio.geojson %s KB · %d features\n",
            f_kb("app/data/municipio.geojson"), nrow(municipio_s)))
cat(sprintf("\nPayload total geográfico: %.1f KB\n",
            f_kb("app/data/barrios.geojson") +
            f_kb("app/data/distritos.geojson") +
            f_kb("app/data/municipio.geojson")))
