#' Descarga las capas OpenStreetMap del análisis HLI Madrid.
#'
#' Tags refinados según verificación con la API Overpass (abril 2026).
#' Todas las capas se recortan al término municipal real (no al bbox).
#'
#' Salidas en data/processed/:
#'   - osm_comida_saludable.gpkg
#'   - osm_gimnasios.gpkg
#'   - osm_fast_food.gpkg
#'   - osm_parques.gpkg

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(osmdata)
  library(fs)
})

source("R/utils.R")

dir_create("data/processed", recurse = TRUE)

# Marco geográfico -------------------------------------------------------------

municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE)
municipio_geo <- st_transform(municipio, CRS_GEOGRAFICO)
bbox <- st_bbox(municipio_geo)

# osmdata usa formato (xmin, ymin, xmax, ymax) en lat/lon
osm_bbox <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
names(osm_bbox) <- c("xmin", "ymin", "xmax", "ymax")

cat("Bbox Madrid municipio (lat/lon):\n"); print(osm_bbox)

# Helper -----------------------------------------------------------------------

#' Lanza una query Overpass robusta. Reintenta una vez si hay 429.
query_osm <- function(features, label) {
  cat("\n→ Descargando: ", label, "\n", sep = "")
  q <- opq(bbox = osm_bbox, timeout = 120) |> add_osm_features(features = features)
  res <- tryCatch(osmdata_sf(q), error = function(e) {
    message("  Reintento tras error: ", conditionMessage(e))
    Sys.sleep(15)
    osmdata_sf(q)
  })
  res
}

#' Combina puntos + centroides de polígonos en una capa de POIs.
poi_unificar <- function(osm, municipio_proj) {
  partes <- list()
  if (!is.null(osm$osm_points) && nrow(osm$osm_points) > 0) {
    partes$pts <- osm$osm_points |> st_transform(CRS_METRICO)
  }
  if (!is.null(osm$osm_polygons) && nrow(osm$osm_polygons) > 0) {
    partes$pol <- osm$osm_polygons |> st_make_valid() |> st_centroid() |>
      st_transform(CRS_METRICO)
  }
  if (!is.null(osm$osm_multipolygons) && nrow(osm$osm_multipolygons) > 0) {
    partes$mp <- osm$osm_multipolygons |> st_make_valid() |> st_centroid() |>
      st_transform(CRS_METRICO)
  }
  if (length(partes) == 0) return(NULL)

  # Unificar columnas (cada elemento puede tener distintos atributos)
  cols_min <- c("osm_id", "name")
  partes <- lapply(partes, function(x) {
    faltan <- setdiff(cols_min, names(x))
    for (f in faltan) x[[f]] <- NA_character_
    x[, cols_min]
  })
  pois <- do.call(rbind, partes)
  pois <- pois[!st_is_empty(pois), ]
  pois <- st_filter(pois, municipio_proj, .predicate = st_intersects)
  unique(pois)
}

#' Combina polígonos + multipolígonos como capa de áreas (para parques).
poligonos_unificar <- function(osm, municipio_proj) {
  partes <- list()
  if (!is.null(osm$osm_polygons) && nrow(osm$osm_polygons) > 0) {
    partes$pol <- osm$osm_polygons |> st_make_valid() |> st_transform(CRS_METRICO) |>
      st_cast("MULTIPOLYGON", warn = FALSE)
  }
  if (!is.null(osm$osm_multipolygons) && nrow(osm$osm_multipolygons) > 0) {
    partes$mp <- osm$osm_multipolygons |> st_make_valid() |> st_transform(CRS_METRICO)
  }
  if (length(partes) == 0) return(NULL)

  cols_min <- c("osm_id", "name")
  partes <- lapply(partes, function(x) {
    faltan <- setdiff(cols_min, names(x))
    for (f in faltan) x[[f]] <- NA_character_
    x[, cols_min]
  })
  pol <- do.call(rbind, partes)
  pol <- pol[!st_is_empty(pol), ]
  pol <- st_filter(pol, municipio_proj, .predicate = st_intersects)
  pol
}

# Descargas --------------------------------------------------------------------

municipio_proj <- st_transform(municipio, CRS_METRICO)

# 1. Comida saludable: greengrocer + marketplace + health_food + extras
osm_alimentacion <- query_osm(
  features = list(
    "shop"    = c("greengrocer", "health_food", "herbalist", "deli", "nutrition_supplements", "farm"),
    "amenity" = "marketplace"
  ),
  label = "comida saludable"
)
comida_saludable <- poi_unificar(osm_alimentacion, municipio_proj)

# 2. Gimnasios y centros deportivos
osm_deporte <- query_osm(
  features = list(
    "leisure" = c("fitness_centre", "fitness_station", "sports_centre")
  ),
  label = "gimnasios y centros deportivos"
)
gimnasios <- poi_unificar(osm_deporte, municipio_proj)

# 3. Fast food
osm_fast <- query_osm(
  features = list("amenity" = "fast_food"),
  label = "fast food"
)
fast_food <- poi_unificar(osm_fast, municipio_proj)

# 4. Parques y zonas verdes — query separada y acotada para evitar timeouts
#    Solo leisure=park (parques urbanos con uso público); descartamos landuse=grass
#    (demasiado genérico, incluye medianas) y natural=wood (Casa de Campo / El Pardo
#    enormes que cuelgan Overpass).
osm_verde <- query_osm(
  features = list("leisure" = "park"),
  label = "parques (leisure=park)"
)
parques_leisure <- poligonos_unificar(osm_verde, municipio_proj)

osm_forest <- query_osm(
  features = list("landuse" = "forest"),
  label = "bosques (landuse=forest)"
)
parques_forest <- poligonos_unificar(osm_forest, municipio_proj)

# Combinar y filtrar superficies pequeñas
parques <- rbind(parques_leisure, parques_forest)
if (!is.null(parques) && nrow(parques) > 0) {
  parques$area_m2 <- as.numeric(st_area(parques))
  parques <- parques[parques$area_m2 >= 1000, ]
}

# Guardar ----------------------------------------------------------------------

guardar <- function(x, ruta) {
  if (is.null(x) || nrow(x) == 0) {
    cat("  ⚠ No hay datos para ", ruta, "\n", sep = "")
    return(invisible(FALSE))
  }
  st_write(x, ruta, delete_dsn = TRUE, quiet = TRUE)
  invisible(TRUE)
}

guardar(comida_saludable, "data/processed/osm_comida_saludable.gpkg")
guardar(gimnasios,        "data/processed/osm_gimnasios.gpkg")
guardar(fast_food,        "data/processed/osm_fast_food.gpkg")
guardar(parques,          "data/processed/osm_parques.gpkg")

# Reporte ----------------------------------------------------------------------

cat("\n=== Resumen capas OSM (Madrid municipio) ===\n")
cat(sprintf("  Comida saludable: %4d POIs (estimado verificador: ~670)\n",
            ifelse(is.null(comida_saludable), 0, nrow(comida_saludable))))
cat(sprintf("  Gimnasios:        %4d POIs\n",
            ifelse(is.null(gimnasios), 0, nrow(gimnasios))))
cat(sprintf("  Fast food:        %4d POIs\n",
            ifelse(is.null(fast_food), 0, nrow(fast_food))))
cat(sprintf("  Parques:          %4d polígonos (>=1000 m²)\n",
            ifelse(is.null(parques), 0, nrow(parques))))
if (!is.null(parques)) {
  area_total_ha <- sum(parques$area_m2) / 1e4
  cat(sprintf("  Área verde total: %.0f ha\n", area_total_ha))
}
