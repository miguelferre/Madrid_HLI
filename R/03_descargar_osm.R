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

# Mirrors Overpass: probaremos en este orden por cada query, saltándonos el
# healthcheck de osmdata::set_overpass_url (que hace una request adicional y
# muere con 429 cuando los servers están saturados).
OVERPASS_MIRRORS <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.openstreetmap.fr/api/interpreter",
  "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter"
)

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

#' Lanza una query Overpass probando varios mirrors en cascada.
#' Cada mirror se reintenta dos veces antes de pasar al siguiente.
query_osm <- function(features, label, mirrors = OVERPASS_MIRRORS) {
  cat("\n→ Descargando: ", label, "\n", sep = "")
  for (url in mirrors) {
    options(osmdata_overpass_url = url)
    cat("  · Mirror: ", url, "\n", sep = "")
    q <- opq(bbox = osm_bbox, timeout = 180) |> add_osm_features(features = features)
    for (intento in 1:2) {
      res <- tryCatch(osmdata_sf(q),
                      error = function(e) { message("    fallo intento ", intento,
                                                    ": ", conditionMessage(e)); NULL })
      if (!is.null(res)) {
        cat("  ✓ OK con ", url, "\n", sep = "")
        return(res)
      }
      Sys.sleep(8)
    }
  }
  stop("Todos los mirrors Overpass fallaron para: ", label)
}

#' Extrae un tag concreto, si existe, devolviendo vector character.
.col_o_na <- function(df, col) {
  if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
}

#' Combina puntos + centroides de polígonos en una capa de POIs,
#' preservando una columna `tipo` con la categoría OSM concreta de cada POI.
poi_unificar <- function(osm, municipio_proj, claves_tag = c("shop", "amenity", "leisure")) {

  procesar <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    # Construir 'tipo' coalescing entre las claves indicadas
    tag_cols <- lapply(claves_tag, .col_o_na, df = x)
    tipo <- Reduce(function(a, b) ifelse(is.na(a), b, a), tag_cols)
    out <- data.frame(
      osm_id = .col_o_na(x, "osm_id"),
      name   = .col_o_na(x, "name"),
      tipo   = tipo,
      stringsAsFactors = FALSE
    )
    st_sf(out, geometry = st_geometry(x))
  }

  partes <- list(
    pts = procesar(osm$osm_points),
    pol = if (!is.null(osm$osm_polygons) && nrow(osm$osm_polygons) > 0)
            procesar(st_set_geometry(osm$osm_polygons,
                       st_centroid(st_make_valid(osm$osm_polygons))$geometry))
          else NULL,
    mp  = if (!is.null(osm$osm_multipolygons) && nrow(osm$osm_multipolygons) > 0)
            procesar(st_set_geometry(osm$osm_multipolygons,
                       st_centroid(st_make_valid(osm$osm_multipolygons))$geometry))
          else NULL
  )
  partes <- partes[!sapply(partes, is.null)]
  if (length(partes) == 0) return(NULL)

  pois <- do.call(rbind, partes) |> st_transform(CRS_METRICO)
  pois <- pois[!st_is_empty(pois), ]
  pois <- pois[!is.na(pois$tipo), ]
  pois <- st_filter(pois, municipio_proj, .predicate = st_intersects)
  pois <- pois[!duplicated(pois$osm_id), ]
  pois
}

#' Combina polígonos + multipolígonos como capa de áreas (para parques),
#' marcándolos con la columna `tipo` que se le pase (p.ej. "park" o "forest").
poligonos_unificar <- function(osm, municipio_proj, tipo) {
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
  pol$tipo <- tipo
  pol <- pol[!st_is_empty(pol), ]
  pol <- st_filter(pol, municipio_proj, .predicate = st_intersects)
  pol
}

# Descargas --------------------------------------------------------------------

municipio_proj <- st_transform(municipio, CRS_METRICO)

# 1. Comida saludable: descomponemos en queries individuales por tag
#    (las queries con muchos features juntos están saturando todos los mirrors).
tags_comida <- list(
  list(features = list("shop"    = "greengrocer"),           label = "shop=greengrocer"),
  list(features = list("shop"    = "health_food"),           label = "shop=health_food"),
  list(features = list("shop"    = "herbalist"),             label = "shop=herbalist"),
  list(features = list("shop"    = "deli"),                  label = "shop=deli"),
  list(features = list("shop"    = "nutrition_supplements"), label = "shop=nutrition_supplements"),
  list(features = list("shop"    = "farm"),                  label = "shop=farm"),
  list(features = list("amenity" = "marketplace"),           label = "amenity=marketplace")
)

partes_comida <- list()
for (tg in tags_comida) {
  osm_part <- query_osm(features = tg$features, label = tg$label)
  partes_comida[[tg$label]] <- poi_unificar(osm_part, municipio_proj)
  Sys.sleep(3)  # respeto al servidor
}
partes_comida <- partes_comida[!sapply(partes_comida, is.null)]
comida_saludable <- if (length(partes_comida) > 0) {
  out <- do.call(rbind, partes_comida)
  out[!duplicated(out$osm_id), ]
} else NULL

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
parques_leisure <- poligonos_unificar(osm_verde, municipio_proj, tipo = "park")

osm_forest <- query_osm(
  features = list("landuse" = "forest"),
  label = "bosques (landuse=forest)"
)
parques_forest <- poligonos_unificar(osm_forest, municipio_proj, tipo = "forest")

# Combinar y filtrar superficies pequeñas
parques <- rbind(parques_leisure, parques_forest)
if (!is.null(parques) && nrow(parques) > 0) {
  parques$area_m2 <- as.numeric(st_area(parques))
  parques <- parques[parques$area_m2 >= 1000, ]
}

# Whitelists -------------------------------------------------------------------
# El parser de osmdata trae nodos accesorios con tags cruzados (parking,
# supermarket, optician…). Filtramos a la whitelist limpia validada con el
# usuario en outputs/maps/02_pois_filtrable.html.

WL_COMIDA   <- c("greengrocer", "marketplace", "health_food", "farm")
WL_DEPORTE  <- c("sports_centre", "fitness_station", "fitness_centre")
WL_FASTFOOD <- c("fast_food")

# Parques: base = leisure=park. Excluimos landuse=forest porque OSM mete dentro
# del polígono forest del Pardo el casco urbano residencial (~4.000 hab.) y
# además el resto de polígonos forest grandes son ruido (Aeropuerto, Cuatro
# Vientos, Casco Histórico Vallecas). PERO rescatamos por nombre los parques
# urbanos emblemáticos que en OSM solo están etiquetados como forest, no park.
WL_PARQUES         <- c("park")
RESCATE_FOREST     <- c("Casa de Campo")

if (!is.null(comida_saludable))
  comida_saludable <- comida_saludable[comida_saludable$tipo %in% WL_COMIDA, ]
if (!is.null(gimnasios))
  gimnasios <- gimnasios[gimnasios$tipo %in% WL_DEPORTE, ]
if (!is.null(fast_food))
  fast_food <- fast_food[fast_food$tipo %in% WL_FASTFOOD, ]
if (!is.null(parques)) {
  mantener <- parques$tipo %in% WL_PARQUES |
              (parques$tipo == "forest" & parques$name %in% RESCATE_FOREST)
  parques <- parques[mantener, ]
  # Reetiquetamos los rescatados a "park" para coherencia downstream
  parques$tipo[parques$name %in% RESCATE_FOREST] <- "park"
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

resumen_tipo <- function(x, label) {
  cat(sprintf("\n%s — total %d POIs:\n", label, nrow(x)))
  print(sort(table(x$tipo), decreasing = TRUE))
}

cat("\n=== Resumen capas OSM (Madrid municipio) ===\n")
if (!is.null(comida_saludable)) resumen_tipo(comida_saludable, "Comida saludable")
if (!is.null(gimnasios))        resumen_tipo(gimnasios,        "Gimnasios y deporte")
if (!is.null(fast_food))        resumen_tipo(fast_food,        "Fast food")
if (!is.null(parques)) {
  area_total_ha <- sum(parques$area_m2) / 1e4
  cat(sprintf("\nParques: %d polígonos (>=1000 m²) · área total %.0f ha\n",
              nrow(parques), area_total_ha))
  cat("Desglose por tipo:\n")
  print(parques |> sf::st_drop_geometry() |>
        dplyr::group_by(tipo) |>
        dplyr::summarise(n = dplyr::n(), area_ha = round(sum(area_m2)/1e4)) |>
        as.data.frame())
}
