#' Cartografía de la accesibilidad peatonal a parques ≥ 1 ha.
#'
#' Salidas:
#'   outputs/figures/07_acc_5min.png         coroplético % a 5 min
#'   outputs/figures/08_acc_15min.png        coroplético % a 15 min
#'   outputs/figures/09_panel_accesibilidad.png   lámina dual 5/15 min
#'   outputs/maps/05_accesibilidad_interactivo.html

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(MetBrewer)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
})

source("R/utils.R")
configurar_pandoc()

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/maps",    recursive = TRUE, showWarnings = FALSE)

hli       <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE)
distritos <- st_read("data/processed/distritos.gpkg",   quiet = TRUE) |>
  st_transform(st_crs(hli))
municipio <- st_read("data/processed/municipio.gpkg",   quiet = TRUE) |>
  st_transform(st_crs(hli))
nodos     <- st_read("data/processed/red_peatonal_nodos.gpkg", quiet = TRUE) |>
  st_transform(st_crs(hli))
parques   <- st_read("data/processed/osm_parques.gpkg", quiet = TRUE) |>
  st_transform(st_crs(hli))
if (!"area_m2" %in% names(parques)) parques$area_m2 <- as.numeric(st_area(parques))
parques_grandes <- parques[parques$area_m2 >= 10000, ]

# --- Coropléticos -------------------------------------------------------------

paleta_acc <- as.character(met.brewer("Demuth", n = 11, type = "continuous"))

coro <- function(var, titulo, subtit) {
  ggplot() +
    geom_sf(data = hli, aes(fill = .data[[var]]),
            color = "white", linewidth = 0.12) +
    geom_sf(data = distritos, fill = NA, color = "grey25", linewidth = 0.4) +
    geom_sf(data = municipio, fill = NA, color = "black", linewidth = 0.7) +
    scale_fill_gradientn(colors = paleta_acc, name = "% nodos",
                          limits = c(0, 100), breaks = seq(0, 100, 25),
                          guide = guide_colorbar(barheight = unit(8, "lines"),
                                                  barwidth = unit(0.5, "lines"),
                                                  ticks.colour = NA, frame.colour = NA)) +
    labs(title = titulo, subtitle = subtit) +
    theme_void(base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", size = 15, color = "#1f2937"),
      plot.subtitle    = element_text(size = 10, color = "#4b5563", margin = margin(b = 10)),
      plot.background  = element_rect(fill = "#fafaf7", color = NA),
      plot.margin      = margin(12, 12, 12, 12)
    )
}

p5  <- coro("acc_5min",
            "Acceso a parque ≥1 ha en 5 min andando",
            "Nodos del callejero con un parque grande a ≤400 m peatonales")
p15 <- coro("acc_15min",
            "Acceso a parque ≥1 ha en 15 min andando",
            "Nodos del callejero con un parque grande a ≤1.200 m peatonales")

ggsave("outputs/figures/07_acc_5min.png",  p5,  width = 10, height = 10, dpi = 220, bg = "#fafaf7")
ggsave("outputs/figures/08_acc_15min.png", p15, width = 10, height = 10, dpi = 220, bg = "#fafaf7")
cat("✓ outputs/figures/07_acc_5min.png\n")
cat("✓ outputs/figures/08_acc_15min.png\n")

# Lámina dual con caption y subtítulo común
panel_acc <- (p5 + theme(plot.subtitle = element_blank()) |
              p15 + theme(plot.subtitle = element_blank())) +
  plot_annotation(
    title = "Madrid · Accesibilidad peatonal a parques de ≥ 1 ha",
    subtitle = "Calculada sobre la red real de calles con dodgr (perfil peatonal a 4,8 km/h)",
    caption = "Datos: OSM (Geofabrik) · Ayto. de Madrid · Análisis: Miguel Ferreiro García",
    theme = theme(
      plot.background = element_rect(fill = "#fafaf7", color = NA),
      plot.title      = element_text(face = "bold", size = 17, color = "#1f2937", hjust = 0.5),
      plot.subtitle   = element_text(size = 11, color = "#4b5563", hjust = 0.5,
                                      margin = margin(b = 8)),
      plot.caption    = element_text(size = 8, color = "#6b7280", hjust = 0.5,
                                      margin = margin(t = 10))
    )
  )
ggsave("outputs/figures/09_panel_accesibilidad.png", panel_acc,
       width = 14, height = 8, dpi = 220, bg = "#fafaf7")
cat("✓ outputs/figures/09_panel_accesibilidad.png\n")

# --- Mapa interactivo ---------------------------------------------------------

hli_geo       <- st_transform(hli, CRS_GEOGRAFICO)
distritos_geo <- st_transform(distritos, CRS_GEOGRAFICO)
parques_geo   <- st_transform(parques_grandes, CRS_GEOGRAFICO)

# Submuestreamos los nodos sin acceso a 15 min para no saturar el mapa.
nodos_sin_acc <- nodos[!nodos$acc_15min, ]
if (nrow(nodos_sin_acc) > 4000)
  nodos_sin_acc <- nodos_sin_acc[sample.int(nrow(nodos_sin_acc), 4000), ]
nodos_sin_acc_geo <- st_transform(nodos_sin_acc, CRS_GEOGRAFICO)

pal_acc <- colorNumeric(palette = paleta_acc, domain = c(0, 100), na.color = "#cccccc")

popup_html <- function(d) {
  sprintf(
    "<div style='font-family:Inter,sans-serif;min-width:240px'>
       <div style='border-bottom:1px solid #e5e7eb;padding-bottom:6px;margin-bottom:6px'>
         <strong style='font-size:14px;color:#1f2937'>%s</strong><br>
         <span style='color:#6b7280;font-size:11px'>%s · ranking HLI #%d</span>
       </div>
       <table style='font-size:11px;color:#374151;border-collapse:collapse;width:100%%'>
         <tr><td><strong>Acceso 5 min</strong></td>  <td style='text-align:right'><strong>%.1f %%</strong></td></tr>
         <tr><td><strong>Acceso 10 min</strong></td> <td style='text-align:right'><strong>%.1f %%</strong></td></tr>
         <tr><td><strong>Acceso 15 min</strong></td> <td style='text-align:right'><strong>%.1f %%</strong></td></tr>
         <tr><td>Mediana al parque más cercano</td>  <td style='text-align:right'>%s m</td></tr>
         <tr><td colspan='2' style='border-top:1px solid #e5e7eb;padding-top:4px'>
           <em>HLI</em> %.3f · <em>Renta</em> %s €/año</td></tr>
       </table>
     </div>",
    htmlEscape(d$NOMBRE), htmlEscape(d$NOMDIS), d$ranking,
    d$acc_5min, d$acc_10min, d$acc_15min,
    formatC(d$dist_park_med, big.mark = ".", format = "d"),
    d$HLI,
    if (is.na(d$renta_neta_persona)) "—"
    else formatC(d$renta_neta_persona, big.mark = ".", format = "d")
  )
}
popups <- vapply(seq_len(nrow(hli_geo)), \(i) popup_html(hli_geo[i, ]), character(1))

attrib_maptiler <- basemap_attribution()

mk_acc_layer <- function(map, valor, group) {
  addPolygons(map,
    data = hli_geo,
    fillColor = pal_acc(valor), fillOpacity = 0.85,
    color = "white", weight = 0.5, opacity = 0.9,
    highlightOptions = highlightOptions(color = "#111827", weight = 2.5,
                                          bringToFront = TRUE, fillOpacity = 0.92),
    popup = popups,
    label = lapply(sprintf("<strong>%s</strong> — %.1f %%",
                            hli_geo$NOMBRE, valor), htmltools::HTML),
    labelOptions = labelOptions(textsize = "12px"),
    group = group
  )
}

mapa <- leaflet(options = leafletOptions(zoomControl = TRUE,
                                          minZoom = 10, maxZoom = 16)) |>
  addTiles(urlTemplate = basemap_tiles("positron"),
            attribution = attrib_maptiler,
            group = "CartoDB Positron") |>
  mk_acc_layer(hli_geo$acc_5min,  "Acceso 5 min")  |>
  mk_acc_layer(hli_geo$acc_10min, "Acceso 10 min") |>
  mk_acc_layer(hli_geo$acc_15min, "Acceso 15 min") |>
  addPolygons(
    data = parques_geo,
    fillColor = "#16a34a", fillOpacity = 0.5,
    color = "#0f5132", weight = 0.5, opacity = 0.8,
    label = lapply(sprintf("%s — %.1f ha",
                            ifelse(is.na(parques_geo$name), "(sin nombre)", parques_geo$name),
                            parques_geo$area_m2/1e4),
                    htmltools::HTML),
    group = "Parques ≥ 1 ha"
  ) |>
  addCircleMarkers(
    data = nodos_sin_acc_geo,
    radius = 1.4, color = "#b91c1c", weight = 0,
    fillColor = "#b91c1c", fillOpacity = 0.55,
    group = "Nodos sin parque a 15 min"
  ) |>
  addPolygons(data = distritos_geo, fill = FALSE, color = "#1f2937",
               weight = 1.2, opacity = 0.7, group = "Distritos") |>
  addLegend(position = "bottomright", pal = pal_acc, values = c(0, 100),
             title = "% nodos accesibles", opacity = 1, bins = 5) |>
  addLayersControl(
    baseGroups = "CartoDB Positron",
    overlayGroups = c("Acceso 5 min", "Acceso 10 min", "Acceso 15 min",
                      "Parques ≥ 1 ha", "Nodos sin parque a 15 min", "Distritos"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(c("Acceso 5 min", "Acceso 10 min",
              "Parques ≥ 1 ha", "Nodos sin parque a 15 min")) |>
  addControl(
    html = "<div style='background:rgba(255,255,255,0.95);padding:10px 14px;
              border-radius:6px;font-family:Inter,sans-serif;font-size:12px;
              box-shadow:0 1px 3px rgba(0,0,0,0.1);max-width:300px;line-height:1.5'>
              <strong style='color:#2c7a4b;font-size:13px'>Acceso peatonal a parques ≥ 1 ha</strong><br>
              <span style='color:#6b7280'>Cálculo isocrónico real con dodgr.<br>
              Cambia entre 5/10/15 min para ver el efecto.</span>
            </div>",
    position = "topright"
  ) |>
  setView(lng = -3.703, lat = 40.430, zoom = 11)

saveWidget(mapa,
            file = file.path(getwd(), "outputs/maps/05_accesibilidad_interactivo.html"),
            selfcontained = TRUE,
            title = "Madrid · Accesibilidad peatonal a parques")
cat("✓ outputs/maps/05_accesibilidad_interactivo.html\n")
