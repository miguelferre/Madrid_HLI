#' Mapa base de Madrid: distritos + barrios.
#'
#' Sirve como sanity check geográfico y como primera lámina del proyecto.
#' Salidas:
#'   - outputs/figures/01_distritos_barrios.png  (estático, alta resolución)
#'   - outputs/maps/01_distritos_barrios.html    (interactivo con MapTiler)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(htmlwidgets)
  library(MetBrewer)
})

source("R/utils.R")

# Pandoc viene embebido en Quarto en Windows; lo enlazamos para saveWidget(selfcontained=TRUE)
configurar_pandoc()

# Cargar datos procesados
distritos <- st_read("data/processed/distritos.gpkg", quiet = TRUE)
barrios   <- st_read("data/processed/barrios.gpkg",   quiet = TRUE)
municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE)

# Centroides de distrito para etiquetas
distrito_labels <- distritos |>
  st_centroid() |>
  mutate(NOMBRE = stringr::str_to_title(NOMBRE))

# --- Mapa estático: barrios coloreados por distrito ----------------------------

paleta <- met.brewer("Signac", n = 21, type = "continuous")

mapa_static <- ggplot() +
  geom_sf(data = barrios,
          aes(fill = factor(CODDIS)),
          color = "white", linewidth = 0.15) +
  geom_sf(data = distritos,
          fill = NA, color = "grey20", linewidth = 0.5) +
  geom_sf(data = municipio,
          fill = NA, color = "black", linewidth = 0.9) +
  geom_sf_text(data = distrito_labels,
               aes(label = NOMBRE),
               size = 2.7, color = "grey10",
               fontface = "bold", check_overlap = TRUE) +
  scale_fill_manual(values = unname(paleta), guide = "none") +
  labs(
    title = "Madrid · 21 distritos, 131 barrios",
    subtitle = "Punto de partida geográfico del análisis HLI",
    caption = "Fuente: Ayuntamiento de Madrid · Geoportal IDEAM (CC BY 4.0)"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#1f2937"),
    plot.subtitle = element_text(size = 11, color = "#4b5563", margin = margin(b = 12)),
    plot.caption  = element_text(size = 8, color = "#6b7280", hjust = 0, margin = margin(t = 12)),
    plot.margin   = margin(15, 15, 15, 15),
    plot.background = element_rect(fill = "#fafaf7", color = NA)
  )

ggsave("outputs/figures/01_distritos_barrios.png",
       mapa_static, width = 10, height = 10, dpi = 220, bg = "#fafaf7")

cat("✓ Mapa estático guardado: outputs/figures/01_distritos_barrios.png\n")

# --- Mapa interactivo: leaflet con tile MapTiler -------------------------------

# Reproyectar a WGS84 para leaflet
distritos_geo <- st_transform(distritos, CRS_GEOGRAFICO)
barrios_geo   <- st_transform(barrios,   CRS_GEOGRAFICO)
municipio_geo <- st_transform(municipio, CRS_GEOGRAFICO)

# Atribución MapTiler
attrib_maptiler <- '&copy; <a href="https://www.maptiler.com/copyright/">MapTiler</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'

popup_barrio <- sprintf(
  "<div style='font-family:Inter,sans-serif'>
     <strong style='font-size:14px;color:#2c7a4b'>%s</strong><br>
     <span style='color:#6b7280;font-size:12px'>Distrito: %s</span><br>
     <span style='color:#6b7280;font-size:12px'>Área: %.2f km²</span>
   </div>",
  stringr::str_to_title(barrios_geo$NOMBRE),
  stringr::str_to_title(barrios_geo$NOMDIS),
  barrios_geo$Area / 1e6
)

mapa_interactivo <- leaflet(
  options = leafletOptions(zoomControl = TRUE, minZoom = 10, maxZoom = 17)
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
    data = barrios_geo,
    fillColor = "#2c7a4b",
    fillOpacity = 0.15,
    color = "#2c7a4b", weight = 0.6, opacity = 0.9,
    highlightOptions = highlightOptions(
      color = "#d97706", weight = 2.5, bringToFront = TRUE, fillOpacity = 0.35
    ),
    label = lapply(popup_barrio, htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-family" = "Inter, sans-serif"),
      textsize = "12px", direction = "auto"
    ),
    group = "Barrios"
  ) |>
  addPolygons(
    data = distritos_geo,
    fill = FALSE,
    color = "#1f2937", weight = 1.5, opacity = 0.85,
    group = "Distritos"
  ) |>
  addPolygons(
    data = municipio_geo,
    fill = FALSE,
    color = "black", weight = 2.5, opacity = 1,
    group = "Término municipal"
  ) |>
  addLayersControl(
    baseGroups = c("MapTiler · DataViz light", "MapTiler · Streets"),
    overlayGroups = c("Barrios", "Distritos", "Término municipal"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addControl(
    html = "<div style='background:rgba(255,255,255,0.95);padding:8px 12px;
                    border-radius:6px;font-family:Inter,sans-serif;font-size:13px;
                    box-shadow:0 1px 3px rgba(0,0,0,0.1);'>
              <strong style='color:#2c7a4b'>Madrid</strong> · 21 distritos · 131 barrios
            </div>",
    position = "topright"
  ) |>
  setView(lng = -3.703, lat = 40.430, zoom = 11)

saveWidget(mapa_interactivo,
           file = file.path(getwd(), "outputs/maps/01_distritos_barrios.html"),
           selfcontained = TRUE,
           title = "Madrid · Distritos y barrios")

cat("✓ Mapa interactivo guardado: outputs/maps/01_distritos_barrios.html\n")
