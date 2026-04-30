#' Mapa interactivo filtrable de los POIs OSM (validación visual).
#'
#' Carga las 4 capas de POIs (`data/processed/osm_*.gpkg`) y las representa
#' sobre el municipio con MapTiler dataviz-light. Cada subtag (`tipo`) es una
#' capa independiente, toggleable, con color distinto, popup `name + tipo`
#' y buscador por nombre.
#'
#' Salida: outputs/maps/02_pois_filtrable.html

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(leaflet)
  library(leaflet.extras)
  library(htmlwidgets)
  library(htmltools)
})

source("R/utils.R")
configurar_pandoc()

dir.create("outputs/maps", recursive = TRUE, showWarnings = FALSE)

# Carga de datos ---------------------------------------------------------------

municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE) |>
  st_transform(CRS_GEOGRAFICO)
distritos <- st_read("data/processed/distritos.gpkg", quiet = TRUE) |>
  st_transform(CRS_GEOGRAFICO)

leer_pois <- function(ruta) {
  if (!file.exists(ruta)) { warning("No existe ", ruta); return(NULL) }
  st_read(ruta, quiet = TRUE) |> st_transform(CRS_GEOGRAFICO)
}

comida    <- leer_pois("data/processed/osm_comida_saludable.gpkg")
gimnasios <- leer_pois("data/processed/osm_gimnasios.gpkg")
fast_food <- leer_pois("data/processed/osm_fast_food.gpkg")
parques   <- leer_pois("data/processed/osm_parques.gpkg")

# Defensa: si parques no trae 'tipo' (versión previa de R/03) inventamos uno
if (!is.null(parques) && !"tipo" %in% names(parques)) parques$tipo <- "park"

# Paletas por subtag -----------------------------------------------------------

colores <- c(
  # comida saludable
  greengrocer = "#2c7a4b", marketplace = "#1d4ed8", health_food = "#0f766e",
  farm = "#65a30d", deli = "#f59e0b", herbalist = "#dc2626",
  nutrition_supplements = "#a855f7",
  # deporte
  fitness_centre = "#1d4ed8", sports_centre = "#7c3aed", fitness_station = "#06b6d4",
  # fast food
  fast_food = "#b91c1c",
  # parques
  park = "#16a34a", forest = "#365314"
)
color_de <- function(t) ifelse(t %in% names(colores), colores[t], "#6b7280") |> unname()

popup_pois <- function(name, tipo, familia) {
  nm <- ifelse(is.na(name) | name == "", "<em>(sin nombre)</em>", htmlEscape(name))
  sprintf(
    "<div style='font-family:Inter,sans-serif;min-width:160px'>
       <strong style='font-size:13px;color:#1f2937'>%s</strong><br>
       <span style='color:#6b7280;font-size:11px'>%s · <code>%s</code></span>
     </div>",
    nm, familia, htmlEscape(tipo)
  )
}

# Estado mutable ---------------------------------------------------------------
overlay_groups <- c()

# Mapa base --------------------------------------------------------------------

attrib_maptiler <- '&copy; <a href="https://www.maptiler.com/copyright/">MapTiler</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'

mapa <- leaflet(
  options = leafletOptions(zoomControl = TRUE, minZoom = 10, maxZoom = 18)
) |>
  addTiles(
    urlTemplate = maptiler_tiles("dataviz-light"),
    attribution = attrib_maptiler,
    group = "MapTiler · DataViz light"
  ) |>
  addTiles(
    urlTemplate = maptiler_tiles("streets-v2"),
    attribution = attrib_maptiler,
    group = "MapTiler · Streets"
  ) |>
  addPolygons(
    data = municipio, fill = FALSE, color = "black",
    weight = 2.5, opacity = 1, group = "Término municipal"
  ) |>
  addPolygons(
    data = distritos, fill = FALSE, color = "#1f2937",
    weight = 0.8, opacity = 0.6, group = "Distritos"
  )
overlay_groups <- c(overlay_groups, "Término municipal", "Distritos")

# Parques (polígonos) ----------------------------------------------------------

if (!is.null(parques) && nrow(parques) > 0) {
  for (t in sort(unique(parques$tipo))) {
    sub   <- parques[parques$tipo == t, ]
    grupo <- sprintf("Parques · %s (%d)", t, nrow(sub))
    label_lab <- ifelse(
      is.na(sub$name) | sub$name == "",
      sprintf("(%s, %.1f ha)", sub$tipo, sub$area_m2 / 1e4),
      sprintf("%s — %.1f ha", sub$name, sub$area_m2 / 1e4)
    )
    mapa <- mapa |>
      addPolygons(
        data = sub,
        fillColor = color_de(t), fillOpacity = 0.35,
        color = color_de(t), weight = 0.8, opacity = 0.7,
        label = label_lab,
        group = grupo
      )
    overlay_groups <- c(overlay_groups, grupo)
  }
}

# POIs (puntos) ----------------------------------------------------------------

añadir_pois <- function(map, sf_pois, familia, prefix_grupo, ov) {
  if (is.null(sf_pois) || nrow(sf_pois) == 0) return(list(map = map, ov = ov))
  tipos <- sort(unique(sf_pois$tipo))
  for (t in tipos) {
    sub    <- sf_pois[sf_pois$tipo == t, ]
    grupo  <- sprintf("%s · %s (%d)", prefix_grupo, t, nrow(sub))
    popups <- mapply(popup_pois, sub$name, sub$tipo, MoreArgs = list(familia = familia),
                     USE.NAMES = FALSE)
    labels <- ifelse(is.na(sub$name) | sub$name == "",
                     paste0("(", sub$tipo, ")"), sub$name)
    map <- map |>
      addCircleMarkers(
        data = sub,
        radius = 5, weight = 1, color = "white", opacity = 0.9,
        fillColor = color_de(t), fillOpacity = 0.85,
        popup = popups,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-family" = "Inter, sans-serif"),
          textsize = "12px"
        ),
        group = grupo
      )
    ov <- c(ov, grupo)
  }
  list(map = map, ov = ov)
}

paso <- añadir_pois(mapa, comida,    "Comida saludable",    "Comida",   overlay_groups)
mapa <- paso$map; overlay_groups <- paso$ov

paso <- añadir_pois(mapa, gimnasios, "Gimnasios y deporte", "Deporte",  overlay_groups)
mapa <- paso$map; overlay_groups <- paso$ov

paso <- añadir_pois(mapa, fast_food, "Fast food",            "Fast food", overlay_groups)
mapa <- paso$map; overlay_groups <- paso$ov

# Búsqueda por nombre ----------------------------------------------------------
# Featuregroup oculto que indexa todos los POIs nombrados.

todos <- list(comida, gimnasios, fast_food) |>
  Filter(Negate(is.null), x = _)

if (length(todos) > 0) {
  cols <- c("osm_id", "name", "tipo", "geom")
  todos <- lapply(todos, function(x) {
    geomcol <- attr(x, "sf_column")
    names(x)[names(x) == geomcol] <- "geom"
    st_geometry(x) <- "geom"
    x[, c("osm_id", "name", "tipo")]
  })
  idx <- do.call(rbind, todos)
  idx <- idx[!is.na(idx$name) & idx$name != "", ]
  if (nrow(idx) > 0) {
    mapa <- mapa |>
      addCircleMarkers(
        data = idx,
        radius = 1, opacity = 0, fillOpacity = 0,
        label = idx$name,
        group = "_search_index"
      ) |>
      hideGroup("_search_index") |>
      addSearchFeatures(
        targetGroups = "_search_index",
        options = searchFeaturesOptions(
          zoom = 17, openPopup = FALSE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
          textPlaceholder = "Buscar POI por nombre…"
        )
      )
  }
}

# Layer control + leyenda ------------------------------------------------------

mapa <- mapa |>
  addLayersControl(
    baseGroups    = c("MapTiler · DataViz light", "MapTiler · Streets"),
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addControl(
    html = "<div style='background:rgba(255,255,255,0.95);padding:8px 12px;
              border-radius:6px;font-family:Inter,sans-serif;font-size:12px;
              box-shadow:0 1px 3px rgba(0,0,0,0.1);max-width:260px;line-height:1.45'>
              <strong style='color:#2c7a4b'>POIs OSM · validación</strong><br>
              <span style='color:#6b7280'>Cada subtag es una capa independiente.<br>
              Activa/desactiva para auditar la mezcla.</span>
            </div>",
    position = "topright"
  ) |>
  setView(lng = -3.703, lat = 40.430, zoom = 12)

ruta_salida <- file.path(getwd(), "outputs/maps/02_pois_filtrable.html")
saveWidget(mapa, file = ruta_salida, selfcontained = TRUE,
           title = "Madrid · POIs OSM (validación)")

cat("✓ Mapa interactivo guardado:", ruta_salida, "\n")
