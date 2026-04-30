#' Combinación de las tres reglas del 3-30-300 en un score por barrio.
#'
#'  - Regla 3   → ≥ 3 árboles a 50 m del nodo                  (calculada en R/14)
#'  - Regla 30  → ≥ 30 % canopy en buffer 250 m del nodo       (calculada en R/15)
#'  - Regla 300 → parque ≥ 1 ha a ≤ 5 min andando (≤ 400 m)    (calculada en R/11
#'                como `acc_5min`)
#'
#' Para cada nodo evaluamos si cumple las tres a la vez. Después agregamos
#' el % de nodos que cumplen las tres reglas por barrio (score_3_30_300) y
#' el promedio simple de las tres reglas individuales (score_medio).
#'
#' DECISIÓN: el HLI v1 (en `R/07_agregacion_barrios.R`) NO se modifica. El
#' 3-30-300 vive como ranking paralelo. Pendiente con Miguel: decidir si
#' entra en HLI v2 reemplazando densidades brutas.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

source("R/utils.R")

nodos <- st_read("data/processed/red_peatonal_nodos.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)

# Aseguramos columnas presentes
stopifnot(all(c("regla_3", "regla_30", "acc_5min") %in% names(nodos)))

# Cumple las 3 reglas a la vez ------------------------------------------------

nodos$regla_300 <- nodos$acc_5min                 # alias semántico
nodos$cumple_3_30_300 <- nodos$regla_3 & nodos$regla_30 & nodos$regla_300

cat("Nodos cumpliendo las 3 reglas a la vez:",
    sum(nodos$cumple_3_30_300, na.rm = TRUE), "/", nrow(nodos),
    sprintf(" (%.1f%%)\n", 100 * mean(nodos$cumple_3_30_300, na.rm = TRUE)))

cat("\nDescomposición individual de la regla 3·30·300 en Madrid:\n")
cat(sprintf("  Regla 3   (≥3 árboles a 50 m)     : %.1f%%\n",
            100 * mean(nodos$regla_3, na.rm = TRUE)))
cat(sprintf("  Regla 30  (≥30%% canopy a 250 m)   : %.1f%%\n",
            100 * mean(nodos$regla_30, na.rm = TRUE)))
cat(sprintf("  Regla 300 (parque ≥1ha a ≤5 min)  : %.1f%%\n",
            100 * mean(nodos$regla_300, na.rm = TRUE)))

st_write(nodos, "data/processed/red_peatonal_nodos.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

# Agregar a barrios -----------------------------------------------------------

barrios <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)

agg <- nodos[, c("regla_3", "regla_30", "regla_300", "cumple_3_30_300")] |>
  st_join(barrios[, "COD_DISBAR"], join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(COD_DISBAR)) |>
  group_by(COD_DISBAR) |>
  summarise(
    regla_300_pct  = round(100 * mean(regla_300,        na.rm = TRUE), 1),
    score_3_30_300 = round(100 * mean(cumple_3_30_300,  na.rm = TRUE), 1),
    .groups = "drop"
  )

barrios <- barrios |> left_join(agg, by = "COD_DISBAR")

# Las columnas regla_3_pct y regla_30_pct deben existir antes de aquí
# (las añaden R/14 y R/15 respectivamente). Si faltan, score_medio sale
# con NA silenciosos: mejor cortar con un mensaje claro.
stopifnot(
  "Falta regla_3_pct: ejecuta R/14_regla_3_arboles.R antes que R/16."  =
    "regla_3_pct"  %in% names(barrios),
  "Falta regla_30_pct: ejecuta R/15_regla_30_canopy.R antes que R/16." =
    "regla_30_pct" %in% names(barrios)
)

barrios <- barrios |>
  mutate(
    score_medio_3_30_300 = round(
      (regla_3_pct + regla_30_pct + regla_300_pct) / 3, 1
    ),
    rank_3_30_300 = rank(-score_3_30_300, ties.method = "min")
  )

cat("\n=== score_3_30_300 (% nodos cumplen las 3 reglas) por barrio ===\n")
print(summary(barrios$score_3_30_300))

cat("\nTop 15 barrios por score_3_30_300:\n")
print(barrios |> st_drop_geometry() |>
        arrange(desc(score_3_30_300)) |> head(15) |>
        select(NOMBRE, NOMDIS,
               regla_3_pct, regla_30_pct, regla_300_pct,
               score_3_30_300, score_medio_3_30_300) |>
        as.data.frame(), row.names = FALSE)

cat("\nBottom 15 barrios por score_3_30_300:\n")
print(barrios |> st_drop_geometry() |>
        arrange(score_3_30_300) |> head(15) |>
        select(NOMBRE, NOMDIS,
               regla_3_pct, regla_30_pct, regla_300_pct,
               score_3_30_300, score_medio_3_30_300) |>
        as.data.frame(), row.names = FALSE)

# Comparación con HLI v1 ------------------------------------------------------

cat("\n=== Correlación 3-30-300 vs HLI v1 vs renta ===\n")
cor_pearson <- function(x, y) round(cor(x, y, use = "pairwise.complete.obs"), 3)
cat(sprintf("score_3_30_300  vs  HLI         = %s\n",
            cor_pearson(barrios$score_3_30_300, barrios$HLI)))
cat(sprintf("score_3_30_300  vs  renta_neta  = %s\n",
            cor_pearson(barrios$score_3_30_300, barrios$renta_neta_persona)))
cat(sprintf("regla_3_pct     vs  renta_neta  = %s\n",
            cor_pearson(barrios$regla_3_pct, barrios$renta_neta_persona)))
cat(sprintf("regla_30_pct    vs  renta_neta  = %s\n",
            cor_pearson(barrios$regla_30_pct, barrios$renta_neta_persona)))
cat(sprintf("regla_300_pct   vs  renta_neta  = %s\n",
            cor_pearson(barrios$regla_300_pct, barrios$renta_neta_persona)))

# Guardar ---------------------------------------------------------------------

st_write(barrios, "data/processed/hli_barrios.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

# CSV legible
csv <- barrios |> st_drop_geometry() |>
  select(CODDIS, NOMDIS, COD_DISBAR, NOMBRE, area_km2, HLI, ranking,
         regla_3_pct, regla_30_pct, regla_300_pct,
         score_3_30_300, score_medio_3_30_300, rank_3_30_300) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
write.csv(csv, "data/processed/hli_barrios.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

cat("\n✓ hli_barrios.{gpkg,csv} con score_3_30_300, rank_3_30_300 y reglas individuales.\n")
