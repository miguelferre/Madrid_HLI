#' Cartografía del 3-30-300 — láminas estáticas + mapa interactivo.
#'
#' Salidas:
#'   outputs/figures/10_regla_3.png            coroplético % nodos con ≥3 árboles a 50m
#'   outputs/figures/11_regla_30.png           coroplético % nodos con ≥30% canopy 250m
#'   outputs/figures/12_regla_300.png          coroplético % nodos con parque ≥1ha a ≤5 min
#'   outputs/figures/13_score_3_30_300.png     coroplético score combinado
#'   outputs/figures/14_panel_3_30_300.png     lámina 4 paneles (regla 3, 30, 300, score)
#'   outputs/maps/06_3_30_300_interactivo.html mapa interactivo con capas seleccionables

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

# --- Coropléticos -------------------------------------------------------------

paleta_verde <- as.character(met.brewer("VanGogh3", n = 11, type = "continuous"))
paleta_score <- as.character(met.brewer("Hokusai2", n = 11, type = "continuous"))

coro <- function(var, titulo, subtit, paleta = paleta_verde) {
  ggplot() +
    geom_sf(data = hli, aes(fill = .data[[var]]),
            color = "white", linewidth = 0.12) +
    geom_sf(data = distritos, fill = NA, color = "grey25", linewidth = 0.4) +
    geom_sf(data = municipio, fill = NA, color = "black", linewidth = 0.7) +
    scale_fill_gradientn(colors = paleta, name = "% nodos",
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

p3   <- coro("regla_3_pct",
             "Regla 3 · ≥ 3 árboles a 50 m",
             "% nodos del callejero con tres árboles inventariados a ≤ 50 m")
p30  <- coro("regla_30_pct",
             "Regla 30 · ≥ 30 % canopy en 250 m",
             "% nodos cuyo entorno (250 m) tiene 30 % o más de cubierta arbórea")
p300 <- coro("regla_300_pct",
             "Regla 300 · parque ≥ 1 ha a 5 min",
             "% nodos con un parque grande a ≤ 400 m peatonales")
ps   <- coro("score_3_30_300",
             "Score 3-30-300 · cumple las tres",
             "% nodos del barrio que cumplen las tres reglas a la vez",
             paleta = paleta_score)

ggsave("outputs/figures/10_regla_3.png",        p3,   width = 10, height = 10, dpi = 220, bg = "#fafaf7")
ggsave("outputs/figures/11_regla_30.png",       p30,  width = 10, height = 10, dpi = 220, bg = "#fafaf7")
ggsave("outputs/figures/12_regla_300.png",      p300, width = 10, height = 10, dpi = 220, bg = "#fafaf7")
ggsave("outputs/figures/13_score_3_30_300.png", ps,   width = 10, height = 10, dpi = 220, bg = "#fafaf7")
cat("✓ outputs/figures/10_regla_3.png · 11_regla_30.png · 12_regla_300.png · 13_score_3_30_300.png\n")

# Lámina 4 paneles
panel <- ((p3   + theme(plot.subtitle = element_blank())) |
          (p30  + theme(plot.subtitle = element_blank()))) /
        ((p300 + theme(plot.subtitle = element_blank())) |
          (ps   + theme(plot.subtitle = element_blank()))) +
  plot_annotation(
    title = "Madrid · Regla 3-30-300 (Konijnendijk) · barrio a barrio",
    subtitle = paste("3 árboles a 50 m  ·  30 % canopy a 250 m  ·  parque ≥ 1 ha a 5 min  ·  score combinado",
                      "evaluado sobre 605 K nodos del callejero"),
    caption = "Datos: Ayto. de Madrid (arbolado) · ESA WorldCover 2021 · OSM · Análisis: Miguel Ferreiro García",
    theme = theme(
      plot.background = element_rect(fill = "#fafaf7", color = NA),
      plot.title      = element_text(face = "bold", size = 17, color = "#1f2937", hjust = 0.5),
      plot.subtitle   = element_text(size = 11, color = "#4b5563", hjust = 0.5,
                                      margin = margin(b = 8)),
      plot.caption    = element_text(size = 8, color = "#6b7280", hjust = 0.5,
                                      margin = margin(t = 10))
    )
  )
ggsave("outputs/figures/14_panel_3_30_300.png", panel,
       width = 16, height = 16, dpi = 220, bg = "#fafaf7")
cat("✓ outputs/figures/14_panel_3_30_300.png\n")

# --- Mapa interactivo ---------------------------------------------------------

hli_geo       <- st_transform(hli, CRS_GEOGRAFICO)
distritos_geo <- st_transform(distritos, CRS_GEOGRAFICO)

pal_v <- colorNumeric(palette = paleta_verde, domain = c(0, 100), na.color = "#cccccc")
pal_s <- colorNumeric(palette = paleta_score, domain = c(0, 60),  na.color = "#cccccc")

popup_html <- function(d) {
  sprintf(
    "<div style='font-family:Inter,sans-serif;min-width:260px'>
       <div style='border-bottom:1px solid #e5e7eb;padding-bottom:6px;margin-bottom:6px'>
         <strong style='font-size:14px;color:#1f2937'>%s</strong><br>
         <span style='color:#6b7280;font-size:11px'>%s · ranking 3-30-300 #%d</span>
       </div>
       <table style='font-size:11px;color:#374151;border-collapse:collapse;width:100%%'>
         <tr><td><strong>Regla 3</strong>   (≥3 árb. 50 m)</td>   <td style='text-align:right'><strong>%.1f %%</strong></td></tr>
         <tr><td><strong>Regla 30</strong>  (≥30%% canopy 250 m)</td><td style='text-align:right'><strong>%.1f %%</strong></td></tr>
         <tr><td><strong>Regla 300</strong> (parque ≥1ha 5 min)</td><td style='text-align:right'><strong>%.1f %%</strong></td></tr>
         <tr><td colspan='2' style='border-top:1px solid #e5e7eb;padding-top:4px'>
           <strong style='color:#0f766e'>Score 3-30-300:</strong>
           <span style='float:right'><strong>%.1f %%</strong></span></td></tr>
         <tr><td>Densidad árboles/ha</td><td style='text-align:right'>%.1f</td></tr>
         <tr><td>Canopy mediano (250 m)</td><td style='text-align:right'>%.1f %%</td></tr>
         <tr><td colspan='2' style='border-top:1px solid #e5e7eb;padding-top:4px'>
           <em>HLI v1</em> %.3f · ranking #%d · <em>Renta</em> %s €</td></tr>
       </table>
     </div>",
    htmlEscape(d$NOMBRE), htmlEscape(d$NOMDIS), d$rank_3_30_300,
    d$regla_3_pct, d$regla_30_pct, d$regla_300_pct,
    d$score_3_30_300,
    d$dens_arboles_ha, d$canopy_med,
    d$HLI, d$ranking,
    if (is.na(d$renta_neta_persona)) "—"
    else formatC(d$renta_neta_persona, big.mark = ".", format = "d")
  )
}
popups <- vapply(seq_len(nrow(hli_geo)), \(i) popup_html(hli_geo[i, ]), character(1))

attrib_maptiler <- basemap_attribution()

mk_layer <- function(map, valor, pal, group) {
  addPolygons(map,
    data = hli_geo,
    fillColor = pal(valor), fillOpacity = 0.85,
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
  mk_layer(hli_geo$score_3_30_300, pal_s, "Score 3-30-300") |>
  mk_layer(hli_geo$regla_3_pct,    pal_v, "Regla 3 · árboles 50 m") |>
  mk_layer(hli_geo$regla_30_pct,   pal_v, "Regla 30 · canopy 250 m") |>
  mk_layer(hli_geo$regla_300_pct,  pal_v, "Regla 300 · parque 5 min") |>
  addPolygons(data = distritos_geo, fill = FALSE, color = "#1f2937",
               weight = 1.2, opacity = 0.7, group = "Distritos") |>
  addLegend(position = "bottomright", pal = pal_s, values = c(0, 60),
             title = "Score 3-30-300 (% nodos)", opacity = 1, bins = 5,
             group = "Score 3-30-300") |>
  addLegend(position = "bottomright", pal = pal_v, values = c(0, 100),
             title = "% nodos cumplen la regla", opacity = 1, bins = 5,
             group = "Regla 3 · árboles 50 m") |>
  addLayersControl(
    baseGroups = "CartoDB Positron",
    overlayGroups = c("Score 3-30-300",
                      "Regla 3 · árboles 50 m",
                      "Regla 30 · canopy 250 m",
                      "Regla 300 · parque 5 min",
                      "Distritos"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(c("Regla 3 · árboles 50 m",
              "Regla 30 · canopy 250 m",
              "Regla 300 · parque 5 min")) |>
  addControl(
    html = "<div style='background:rgba(255,255,255,0.95);padding:10px 14px;
              border-radius:6px;font-family:Inter,sans-serif;font-size:12px;
              box-shadow:0 1px 3px rgba(0,0,0,0.1);max-width:330px;line-height:1.5'>
              <strong style='color:#0f766e;font-size:13px'>Regla 3-30-300 (Konijnendijk)</strong><br>
              <span style='color:#6b7280'>3 árboles desde la ventana · 30 % canopy en el barrio · parque a 5 min andando.<br>
              Cambia de capa para ver cada regla por separado.</span>
            </div>",
    position = "topright"
  ) |>
  setView(lng = -3.703, lat = 40.430, zoom = 11)

saveWidget(mapa,
            file = file.path(getwd(), "outputs/maps/06_3_30_300_interactivo.html"),
            selfcontained = TRUE,
            title = "Madrid · 3-30-300")
cat("✓ outputs/maps/06_3_30_300_interactivo.html\n")
