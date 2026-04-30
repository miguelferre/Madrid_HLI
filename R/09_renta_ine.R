#' Cruce HLI ↔ renta INE — Atlas de Distribución de Renta de los Hogares 2023.
#'
#' Pipeline:
#'   1. Carga el shapefile de secciones censales 2024 (INE) y filtra al
#'      municipio 28079 (Madrid).
#'   2. Carga la tabla 30824 del ADRH ya filtrada a Madrid (CSV en data/raw/ine).
#'      Pivota a wide y se queda con 2023.
#'   3. Une renta × secciones por CUSEC.
#'   4. Asigna cada sección al barrio del Ayto donde cae su centroide
#'      (st_within en EPSG:25830).
#'   5. Agrega renta a barrio (media simple de secciones — sin pesos por
#'      población; se anota como limitación).
#'   6. Joinea con hli_barrios.gpkg y vuelca el resultado enriquecido.
#'
#' Salidas:
#'   data/processed/secciones_madrid.gpkg   (secciones censales del municipio)
#'   data/processed/renta_secciones.gpkg    (secciones + renta 2023)
#'   data/processed/hli_barrios.gpkg        (sobrescrito: ahora con renta)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

source("R/utils.R")

# 1. Secciones censales --------------------------------------------------------

shp_secc <- "data/raw/ine/SECC_CE_20240101.shp"
gpkg_madrid <- "data/processed/secciones_madrid.gpkg"

if (!file.exists(gpkg_madrid)) {
  cat("Leyendo shapefile nacional de secciones censales...\n")
  secc_es <- st_read(shp_secc, quiet = TRUE)
  cat("  Total secciones España:", nrow(secc_es), "\n")
  cat("  Columnas:", paste(names(secc_es), collapse=", "), "\n")
  secc_madrid <- secc_es[secc_es$CUMUN == "28079", ]
  cat("  Secciones de Madrid municipio:", nrow(secc_madrid), "\n")
  st_write(secc_madrid, gpkg_madrid, delete_dsn = TRUE, quiet = TRUE)
  rm(secc_es); gc()
} else {
  cat("Usando secciones cacheadas en", gpkg_madrid, "\n")
}

secc <- st_read(gpkg_madrid, quiet = TRUE) |> st_transform(CRS_METRICO)
cat("Secciones Madrid cargadas:", nrow(secc), "· CRS:", st_crs(secc)$epsg, "\n\n")

# 2. ADRH ----------------------------------------------------------------------

cat("Cargando tabla ADRH 30824 (Madrid) ...\n")
renta_long <- read_delim(
  "data/raw/ine/30824_madrid.csv",
  delim = ";", col_names = FALSE, locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)
names(renta_long) <- c("municipios", "distritos", "secciones", "indicador",
                        "periodo", "total")

# El total viene en formato "19.632" (punto = miles, sin decimales en estos
# indicadores). Limpiamos y casteamos.
renta_long$valor <- as.numeric(gsub("\\.", "", renta_long$total))
renta_long$periodo <- as.integer(renta_long$periodo)

# Solo filas a nivel de SECCIÓN (las demás son agregados municipio/distrito)
renta_secc <- renta_long |>
  filter(!is.na(secciones), secciones != "", periodo == 2023) |>
  mutate(CUSEC = str_extract(secciones, "^\\d{10}"))

cat("Filas renta sección 2023:", nrow(renta_secc),
    "· secciones únicas:", length(unique(renta_secc$CUSEC)), "\n")

# Pivote a wide — un valor por indicador
indicadores_clave <- c(
  "Renta neta media por persona"             = "renta_neta_persona",
  "Renta neta media por hogar"               = "renta_neta_hogar",
  "Mediana de la renta por unidad de consumo"= "mediana_uc"
)

renta_wide <- renta_secc |>
  filter(indicador %in% names(indicadores_clave)) |>
  mutate(ind = unname(indicadores_clave[indicador])) |>
  select(CUSEC, ind, valor) |>
  pivot_wider(names_from = ind, values_from = valor)

cat("Secciones con renta 2023:", nrow(renta_wide), "\n")
cat("Resumen renta_neta_persona (€/persona/año):\n")
print(summary(renta_wide$renta_neta_persona))

# 3. Join renta × secciones ----------------------------------------------------

secc_renta <- secc |>
  left_join(renta_wide, by = "CUSEC")

n_sin_renta <- sum(is.na(secc_renta$renta_neta_persona))
cat("\nSecciones sin renta:", n_sin_renta, "/", nrow(secc_renta),
    sprintf(" (%.1f%%)\n", 100 * n_sin_renta / nrow(secc_renta)))

st_write(secc_renta, "data/processed/renta_secciones.gpkg",
          delete_dsn = TRUE, quiet = TRUE)

# 4. Asignación sección → barrio Ayto ------------------------------------------

barrios <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE) |>
  st_transform(CRS_METRICO)

centroides_secc <- st_centroid(secc_renta)
join_secc <- st_join(centroides_secc, barrios[, "COD_DISBAR"],
                      join = st_within)
secc_renta$COD_DISBAR <- join_secc$COD_DISBAR

n_sin_barrio <- sum(is.na(secc_renta$COD_DISBAR))
cat("Secciones sin barrio asignado:", n_sin_barrio,
    sprintf(" (%.1f%%)\n", 100 * n_sin_barrio / nrow(secc_renta)))

# 5. Agregación a barrio -------------------------------------------------------

renta_barrio <- secc_renta |>
  st_drop_geometry() |>
  filter(!is.na(COD_DISBAR), !is.na(renta_neta_persona)) |>
  group_by(COD_DISBAR) |>
  summarise(
    n_secciones        = n(),
    renta_neta_persona = round(mean(renta_neta_persona, na.rm = TRUE)),
    renta_neta_hogar   = round(mean(renta_neta_hogar,   na.rm = TRUE)),
    mediana_uc         = round(mean(mediana_uc,         na.rm = TRUE)),
    .groups = "drop"
  )

cat("\nBarrios con renta agregada:", nrow(renta_barrio),
    "/", nrow(barrios), "\n")
cat("Resumen renta_neta_persona por barrio (media simple de secciones):\n")
print(summary(renta_barrio$renta_neta_persona))

# 6. Enriquecer hli_barrios.gpkg ----------------------------------------------

hli_enriquecido <- barrios |> left_join(renta_barrio, by = "COD_DISBAR")

# Preparamos la salida final
salida <- hli_enriquecido
st_write(salida, "data/processed/hli_barrios.gpkg",
          delete_dsn = TRUE, quiet = TRUE)

# 7. Reporte HLI ↔ renta -------------------------------------------------------

df <- salida |> st_drop_geometry() |>
  filter(!is.na(renta_neta_persona))

cat(sprintf("\n=== HLI ↔ Renta 2023 (n=%d barrios con renta) ===\n", nrow(df)))
cat(sprintf("Correlación Pearson  HLI ↔ renta_neta_persona: %.3f\n",
            cor(df$HLI, df$renta_neta_persona)))
cat(sprintf("Correlación Spearman HLI ↔ renta_neta_persona: %.3f\n",
            cor(df$HLI, df$renta_neta_persona, method = "spearman")))

cat("\n--- Top 10 renta neta por persona ---\n")
print(df |> arrange(desc(renta_neta_persona)) |> head(10) |>
        select(NOMBRE, NOMDIS, renta_neta_persona, HLI, ranking) |>
        as.data.frame(), row.names = FALSE)

cat("\n--- Bottom 10 renta neta por persona ---\n")
print(df |> arrange(renta_neta_persona) |> head(10) |>
        select(NOMBRE, NOMDIS, renta_neta_persona, HLI, ranking) |>
        as.data.frame(), row.names = FALSE)

# Cuadrantes interesantes para narrativa: barrios "ricos en HLI a pesar de
# renta baja" y barrios "bajos en HLI a pesar de renta alta"
mediana_hli   <- median(df$HLI)
mediana_renta <- median(df$renta_neta_persona)

df$cuadrante <- with(df, case_when(
  HLI >= mediana_hli   & renta_neta_persona >= mediana_renta ~ "alto HLI · renta alta",
  HLI >= mediana_hli   & renta_neta_persona <  mediana_renta ~ "alto HLI · renta baja",
  HLI <  mediana_hli   & renta_neta_persona >= mediana_renta ~ "bajo HLI · renta alta",
  TRUE                                                       ~ "bajo HLI · renta baja"
))

cat("\n--- Distribución por cuadrantes (medianas como umbral) ---\n")
print(df |> count(cuadrante) |> as.data.frame(), row.names = FALSE)

cat("\n--- Top 10 'alto HLI · renta baja' (las gangas) ---\n")
print(df |> filter(cuadrante == "alto HLI · renta baja") |>
        arrange(desc(HLI)) |> head(10) |>
        select(NOMBRE, NOMDIS, HLI, renta_neta_persona) |>
        as.data.frame(), row.names = FALSE)

cat("\n--- Top 10 'bajo HLI · renta alta' (las trampas) ---\n")
print(df |> filter(cuadrante == "bajo HLI · renta alta") |>
        arrange(desc(renta_neta_persona)) |> head(10) |>
        select(NOMBRE, NOMDIS, HLI, renta_neta_persona) |>
        as.data.frame(), row.names = FALSE)

cat("\n✓ Cruce renta-HLI completado. data/processed/hli_barrios.gpkg actualizado.\n")
