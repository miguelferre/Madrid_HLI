#' Cartografía del HLI por barrio: lámina estática + mapa interactivo.
#'
#' Lámina (PNG): coroplético del HLI con etiquetas en top 5 / bottom 5
#' y destacados narrativos (Sol como caso extremo de fast food).
#'
#' Mapa interactivo (HTML): CartoDB Positron con tooltip que muestra
#' los cuatro componentes y el ranking, capas alternativas para cada
#' indicador y leyenda continua.
#'
#' Salidas:
#'   - outputs/figures/03_hli_choropleth.png
#'   - outputs/maps/03_hli_interactivo.html

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
  library(MetBrewer)
})

source("R/utils.R")
configurar_pandoc()

dir.create("outputs/maps",    recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Carga ------------------------------------------------------------------------

hli       <- st_read("data/processed/hli_barrios.gpkg", quiet = TRUE)
distritos <- st_read("data/processed/distritos.gpkg",   quiet = TRUE) |>
  st_transform(st_crs(hli))
municipio <- st_read("data/processed/municipio.gpkg",   quiet = TRUE) |>
  st_transform(st_crs(hli))

# --- Lámina estática ----------------------------------------------------------

# Centroides para etiquetas (top 5 + bottom 5 + destacado Sol si no estuviera)
top5 <- hli |> arrange(ranking) |> head(5) |>
  mutate(rol = "top")
bot5 <- hli |> arrange(desc(ranking)) |> head(5) |>
  mutate(rol = "bot")
destacados <- bind_rows(top5, bot5) |>
  distinct(COD_DISBAR, .keep_all = TRUE)
sol <- hli[hli$NOMBRE == "Sol", ]
if (!sol$COD_DISBAR %in% destacados$COD_DISBAR) {
  destacados <- bind_rows(destacados, sol |> mutate(rol = "bot"))
}

destacados_centroides <- destacados |>
  st_centroid() |>
  mutate(etiqueta = sprintf("#%d %s", ranking, NOMBRE))

paleta_hli <- as.character(met.brewer("Hiroshige", n = 11, type = "continuous")) |> rev()

mapa_static <- ggplot() +
  geom_sf(data = hli,
          aes(fill = HLI),
          color = "white", linewidth = 0.12) +
  geom_sf(data = distritos,
          fill = NA, color = "grey25", linewidth = 0.4) +
  geom_sf(data = municipio,
          fill = NA, color = "black", linewidth = 0.7) +
  ggrepel::geom_label_repel(
    data = destacados_centroides,
    aes(label = etiqueta, geometry = geom,
        color = rol),
    stat = "sf_coordinates",
    size = 2.6, fontface = "bold",
    label.size = 0.15, label.r = unit(0.15, "lines"),
    label.padding = unit(0.18, "lines"),
    fill = alpha("white", 0.92),
    segment.color = "grey35", segment.size = 0.3,
    min.segment.length = 0, force = 4,
    box.padding = 0.4, max.overlaps = 30, seed = 42
  ) +
  scale_fill_gradientn(
    colors = unname(paleta_hli),
    name   = "HLI",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    guide  = guide_colorbar(barheight = unit(8, "lines"),
                            barwidth  = unit(0.5, "lines"),
                            ticks.colour = NA, frame.colour = NA)
  ) +
  scale_color_manual(values = c(top = "#0f5132", bot = "#842029"), guide = "none") +
  labs(
    title    = "Madrid · Healthy Living Index por barrio",
    subtitle = "Indicador compuesto: comida saludable, deporte, parques (positivos) y fast food (inverso)",
    caption  = "Datos: OpenStreetMap (ODbL) · Geoportal IDEAM · Ayuntamiento de Madrid · Análisis: Miguel Ferreiro García"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 17, color = "#1f2937"),
    plot.subtitle    = element_text(size = 11, color = "#4b5563", margin = margin(b = 12)),
    plot.caption     = element_text(size = 8,  color = "#6b7280", hjust = 0, margin = margin(t = 12)),
    plot.background  = element_rect(fill = "#fafaf7", color = NA),
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 9),
    plot.margin      = margin(15, 15, 15, 15)
  )

ggsave("outputs/figures/03_hli_choropleth.png", mapa_static,
       width = 11, height = 11, dpi = 220, bg = "#fafaf7")

cat("✓ Lámina estática guardada: outputs/figures/03_hli_choropleth.png\n")

# --- Mapa interactivo ---------------------------------------------------------

hli_geo <- st_transform(hli, CRS_GEOGRAFICO)
distritos_geo <- st_transform(distritos, CRS_GEOGRAFICO)

paleta_leaflet <- colorNumeric(palette = paleta_hli,
                                domain = c(0, 1), na.color = "#cccccc")

popup_html <- function(d) {
  sprintf(
    "<div style='font-family:Inter,sans-serif;min-width:240px'>
       <div style='border-bottom:1px solid #e5e7eb;padding-bottom:6px;margin-bottom:6px'>
         <strong style='font-size:14px;color:#1f2937'>%s</strong><br>
         <span style='color:#6b7280;font-size:11px'>%s · ranking #%d de 131</span>
       </div>
       <table style='font-size:11px;color:#374151;border-collapse:collapse;width:100%%'>
         <tr><td><strong>HLI</strong></td><td style='text-align:right'><strong>%.3f</strong></td></tr>
         <tr><td>Comida saludable</td><td style='text-align:right'>%d POIs · %.1f /km²</td></tr>
         <tr><td>Deporte</td>          <td style='text-align:right'>%d POIs · %.1f /km²</td></tr>
         <tr><td>Fast food</td>        <td style='text-align:right'>%d POIs · %.1f /km²</td></tr>
         <tr><td>Cubierta parques</td> <td style='text-align:right'>%.1f %%</td></tr>
       </table>
     </div>",
    htmlEscape(d$NOMBRE), htmlEscape(d$NOMDIS), d$ranking,
    d$HLI,
    d$n_comida,   d$dens_comida,
    d$n_deporte,  d$dens_deporte,
    d$n_fastfood, d$dens_fastfood,
    d$pct_parques
  )
}
popups <- vapply(seq_len(nrow(hli_geo)),
                  \(i) popup_html(hli_geo[i, ]), character(1))

attrib_maptiler <- basemap_attribution()

# Capa por componente: paleta y dominio propio para cada uno
mk_layer <- function(map, valor, group, paleta_opt, lim = NULL, fmt = "%.1f", unidad = "") {
  if (is.null(lim)) lim <- range(valor, na.rm = TRUE)
  cols <- as.character(met.brewer(paleta_opt, n = 11, type = "continuous"))
  pal  <- colorNumeric(cols, domain = lim, na.color = "#cccccc")
  addPolygons(map,
    data = hli_geo,
    fillColor = pal(valor), fillOpacity = 0.85,
    color = "white", weight = 0.5, opacity = 0.9,
    highlightOptions = highlightOptions(
      color = "#111827", weight = 2, bringToFront = TRUE, fillOpacity = 0.9),
    label = lapply(sprintf("<strong>%s</strong> — %s%s",
                            hli_geo$NOMBRE, sprintf(fmt, valor), unidad),
                    htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-family" = "Inter, sans-serif"), textsize = "12px"),
    group = group
  )
}

mapa <- leaflet(options = leafletOptions(zoomControl = TRUE,
                                          minZoom = 10, maxZoom = 16)) |>
  addTiles(urlTemplate = basemap_tiles("positron"),
            attribution = attrib_maptiler,
            group = "CartoDB Positron") |>
  addTiles(urlTemplate = basemap_tiles("voyager"),
            attribution = attrib_maptiler,
            group = "CartoDB Voyager") |>
  addPolygons(
    data = hli_geo,
    fillColor = paleta_leaflet(hli_geo$HLI), fillOpacity = 0.85,
    color = "white", weight = 0.5, opacity = 0.9,
    highlightOptions = highlightOptions(
      color = "#111827", weight = 2.5, bringToFront = TRUE, fillOpacity = 0.92),
    popup = popups,
    label = lapply(sprintf("<strong>%s</strong> — HLI %.2f (#%d)",
                            hli_geo$NOMBRE, hli_geo$HLI, hli_geo$ranking),
                    htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-family" = "Inter, sans-serif"), textsize = "12px"),
    group = "HLI compuesto"
  ) |>
  mk_layer(hli_geo$dens_comida,   "Densidad comida saludable", "VanGogh3",
            fmt = "%.1f", unidad = " POIs/km²") |>
  mk_layer(hli_geo$dens_deporte,  "Densidad deporte",           "Hokusai2",
            fmt = "%.1f", unidad = " POIs/km²") |>
  mk_layer(hli_geo$dens_fastfood, "Densidad fast food",         "Tam",
            fmt = "%.1f", unidad = " POIs/km²") |>
  mk_layer(hli_geo$pct_parques,   "Cubierta parques",           "Hiroshige",
            fmt = "%.1f", unidad = " %") |>
  addPolygons(
    data = distritos_geo,
    fill = FALSE, color = "#1f2937", weight = 1.2, opacity = 0.7,
    group = "Distritos"
  ) |>
  addLegend(
    position = "bottomright",
    pal = paleta_leaflet, values = c(0, 1),
    title = "HLI",
    opacity = 1, bins = 5
  ) |>
  addLayersControl(
    baseGroups = c("CartoDB Positron", "CartoDB Voyager"),
    overlayGroups = c("HLI compuesto",
                      "Densidad comida saludable",
                      "Densidad deporte",
                      "Densidad fast food",
                      "Cubierta parques",
                      "Distritos"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(c("Densidad comida saludable", "Densidad deporte",
              "Densidad fast food", "Cubierta parques")) |>
  addControl(
    html = "<div style='background:rgba(255,255,255,0.95);padding:10px 14px;
              border-radius:6px;font-family:Inter,sans-serif;font-size:12px;
              box-shadow:0 1px 3px rgba(0,0,0,0.1);max-width:300px;line-height:1.5'>
              <strong style='color:#2c7a4b;font-size:13px'>HLI · Madrid</strong><br>
              <span style='color:#6b7280'>Healthy Living Index agregado a 131 barrios.<br>
              Click en un barrio para ver el desglose.</span>
            </div>",
    position = "topright"
  ) |>
  setView(lng = -3.703, lat = 40.430, zoom = 11)

saveWidget(mapa,
            file = file.path(getwd(), "outputs/maps/03_hli_interactivo.html"),
            selfcontained = TRUE,
            title = "Madrid · Healthy Living Index")

cat("✓ Mapa interactivo guardado: outputs/maps/03_hli_interactivo.html\n")
