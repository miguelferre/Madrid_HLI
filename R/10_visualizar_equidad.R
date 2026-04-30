#' Cartografía y diagnóstico del eje HLI ↔ renta.
#'
#' Salidas:
#'   outputs/figures/04_renta_choropleth.png   coroplético de renta neta
#'   outputs/figures/05_hli_vs_renta.png        scatter con cuadrantes
#'   outputs/figures/06_panel_equidad.png       lámina dual HLI / renta
#'   outputs/maps/04_equidad_interactivo.html   mapa con HLI, renta y residual

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
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

df <- hli |> st_drop_geometry()

# Residual del HLI tras quitar el efecto lineal de la renta — útil para
# detectar barrios con HLI por encima/debajo de lo esperado a su nivel de renta.
modelo  <- lm(HLI ~ renta_neta_persona, data = df)
df$HLI_residual <- residuals(modelo)
hli$HLI_residual <- df$HLI_residual

mediana_hli   <- median(df$HLI,                na.rm = TRUE)
mediana_renta <- median(df$renta_neta_persona, na.rm = TRUE)

df$cuadrante <- with(df, dplyr::case_when(
  HLI >= mediana_hli   & renta_neta_persona >= mediana_renta ~ "alto HLI · renta alta",
  HLI >= mediana_hli   & renta_neta_persona <  mediana_renta ~ "alto HLI · renta baja",
  HLI <  mediana_hli   & renta_neta_persona >= mediana_renta ~ "bajo HLI · renta alta",
  TRUE                                                       ~ "bajo HLI · renta baja"
))

# --- Coroplético de renta -----------------------------------------------------

paleta_renta <- as.character(met.brewer("Tam", n = 11, type = "continuous"))

mapa_renta <- ggplot() +
  geom_sf(data = hli, aes(fill = renta_neta_persona),
          color = "white", linewidth = 0.12) +
  geom_sf(data = distritos, fill = NA, color = "grey25", linewidth = 0.4) +
  geom_sf(data = municipio, fill = NA, color = "black", linewidth = 0.7) +
  scale_fill_gradientn(
    colors = paleta_renta,
    name   = "€/persona/año",
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    guide  = guide_colorbar(barheight = unit(8, "lines"),
                             barwidth  = unit(0.5, "lines"),
                             ticks.colour = NA, frame.colour = NA)
  ) +
  labs(
    title    = "Madrid · Renta neta media por persona (INE ADRH 2023)",
    subtitle = "Agregada del nivel sección censal a barrio del Ayuntamiento",
    caption  = "Datos: INE Atlas de Distribución de Renta de los Hogares · Análisis: Miguel Ferreiro García"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 16, color = "#1f2937"),
    plot.subtitle    = element_text(size = 11, color = "#4b5563", margin = margin(b = 12)),
    plot.caption     = element_text(size = 8,  color = "#6b7280", hjust = 0, margin = margin(t = 12)),
    plot.background  = element_rect(fill = "#fafaf7", color = NA),
    plot.margin      = margin(15, 15, 15, 15)
  )

ggsave("outputs/figures/04_renta_choropleth.png", mapa_renta,
       width = 10, height = 10, dpi = 220, bg = "#fafaf7")
cat("✓ outputs/figures/04_renta_choropleth.png\n")

# --- Scatter HLI × renta con cuadrantes ---------------------------------------

# Etiquetar las "gangas" (top 5 por HLI con renta < mediana) y "trampas"
# (top 5 por renta con HLI < mediana).
gangas <- df |>
  filter(cuadrante == "alto HLI · renta baja") |>
  arrange(desc(HLI)) |> head(5) |> mutate(rol = "ganga")
trampas <- df |>
  filter(cuadrante == "bajo HLI · renta alta") |>
  arrange(desc(renta_neta_persona)) |> head(5) |> mutate(rol = "trampa")
extremos <- df |>
  filter(NOMBRE %in% c("Sol", "Comillas", "Abrantes")) |>
  mutate(rol = ifelse(NOMBRE == "Sol", "extremo_bajo", "extremo_alto"))
etiquetar <- bind_rows(gangas, trampas, extremos) |>
  distinct(NOMBRE, .keep_all = TRUE)

scatter <- ggplot(df, aes(renta_neta_persona, HLI)) +
  geom_hline(yintercept = mediana_hli,   linetype = "dashed", color = "grey55") +
  geom_vline(xintercept = mediana_renta, linetype = "dashed", color = "grey55") +
  geom_smooth(method = "lm", se = TRUE, color = "#374151",
              fill = "grey85", linewidth = 0.5, alpha = 0.4) +
  geom_point(aes(color = cuadrante), size = 2.4, alpha = 0.85) +
  ggrepel::geom_label_repel(
    data = etiquetar,
    aes(label = NOMBRE, color = cuadrante),
    size = 3, fontface = "bold",
    label.size = 0.15, label.padding = unit(0.18, "lines"),
    fill = alpha("white", 0.92),
    segment.color = "grey45", segment.size = 0.3,
    min.segment.length = 0, force = 5, box.padding = 0.4,
    max.overlaps = 25, seed = 42
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = c(
    "alto HLI · renta alta" = "#0f5132",
    "alto HLI · renta baja" = "#0d6efd",
    "bajo HLI · renta alta" = "#d97706",
    "bajo HLI · renta baja" = "#842029"
  )) +
  labs(
    title    = "El HLI no compra con dinero",
    subtitle = sprintf(
      "Correlación Pearson: %.2f · Spearman: %.2f · n = %d barrios. Las dotaciones de\nbarrio (mercados, polideportivos, parques) se concentran en el sur obrero",
      cor(df$HLI, df$renta_neta_persona),
      cor(df$HLI, df$renta_neta_persona, method = "spearman"),
      nrow(df)),
    x = "Renta neta media por persona (€/año, INE ADRH 2023)",
    y = "Healthy Living Index (0–1)",
    color = NULL,
    caption = "Líneas a trazos: medianas. Banda gris: regresión lineal con IC95."
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#1f2937"),
    plot.subtitle = element_text(size = 10, color = "#4b5563",
                                  margin = margin(b = 14), lineheight = 1.2),
    plot.caption  = element_text(size = 8, color = "#6b7280", hjust = 0),
    legend.position = "top", legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "#fafaf7", color = NA)
  )

ggsave("outputs/figures/05_hli_vs_renta.png", scatter,
       width = 10, height = 8, dpi = 220, bg = "#fafaf7")
cat("✓ outputs/figures/05_hli_vs_renta.png\n")

# --- Lámina dual HLI / renta con extremos compartidos -------------------------

# Reusa la paleta del choropleth HLI ya usada en R/08
paleta_hli <- rev(as.character(met.brewer("Hiroshige", n = 11, type = "continuous")))

mini_choropleth <- function(data, var, paleta, titulo, leyenda,
                              fmt = scales::label_number()) {
  ggplot() +
    geom_sf(data = data, aes(fill = .data[[var]]),
            color = "white", linewidth = 0.1) +
    geom_sf(data = distritos, fill = NA, color = "grey25", linewidth = 0.35) +
    geom_sf(data = municipio, fill = NA, color = "black", linewidth = 0.6) +
    scale_fill_gradientn(
      colors = paleta, name = leyenda, labels = fmt,
      guide = guide_colorbar(barheight = unit(7, "lines"),
                              barwidth = unit(0.45, "lines"),
                              ticks.colour = NA, frame.colour = NA)
    ) +
    labs(title = titulo) +
    theme_void(base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", size = 13, color = "#1f2937",
                                       hjust = 0.5, margin = margin(b = 6)),
      legend.title     = element_text(size = 9),
      legend.text      = element_text(size = 8),
      plot.background  = element_rect(fill = "#fafaf7", color = NA),
      plot.margin      = margin(8, 8, 8, 8)
    )
}

p_hli   <- mini_choropleth(hli, "HLI",                paleta_hli,    "HLI", "0–1")
p_renta <- mini_choropleth(hli, "renta_neta_persona", paleta_renta,  "Renta neta",
                            "€/año",
                            scales::label_number(big.mark = ".", decimal.mark = ","))

panel <- (p_hli | p_renta) +
  plot_annotation(
    title = "Madrid · HLI y renta no van de la mano",
    subtitle = sprintf("Correlación Pearson %.2f. El sur obrero domina el HLI; el norte rico domina la renta.",
                       cor(df$HLI, df$renta_neta_persona)),
    caption = "Datos: OpenStreetMap (ODbL) · INE Atlas Distribución Renta Hogares 2023 · Ayuntamiento de Madrid · Análisis: Miguel Ferreiro García",
    theme = theme(
      plot.background = element_rect(fill = "#fafaf7", color = NA),
      plot.title      = element_text(face = "bold", size = 17, color = "#1f2937", hjust = 0.5),
      plot.subtitle   = element_text(size = 11, color = "#4b5563", hjust = 0.5,
                                      margin = margin(b = 8)),
      plot.caption    = element_text(size = 8, color = "#6b7280", hjust = 0.5,
                                      margin = margin(t = 10))
    )
  )

ggsave("outputs/figures/06_panel_equidad.png", panel,
       width = 14, height = 8, dpi = 220, bg = "#fafaf7")
cat("✓ outputs/figures/06_panel_equidad.png\n")

# --- Mapa interactivo: HLI / renta / residual ---------------------------------

hli_geo <- st_transform(hli, CRS_GEOGRAFICO)
distritos_geo <- st_transform(distritos, CRS_GEOGRAFICO)

pal_hli      <- colorNumeric(palette = paleta_hli,
                              domain = c(0, 1), na.color = "#cccccc")
pal_renta    <- colorNumeric(palette = paleta_renta,
                              domain = range(hli_geo$renta_neta_persona, na.rm = TRUE),
                              na.color = "#cccccc")
# Residual: divergente (negativo = HLI por debajo del esperado, positivo = encima)
rng_res      <- max(abs(range(hli_geo$HLI_residual, na.rm = TRUE)))
pal_residual <- colorNumeric(palette = c("#842029", "#e5e7eb", "#0f5132"),
                              domain = c(-rng_res, rng_res), na.color = "#cccccc")

popup_html <- function(d) {
  sprintf(
    "<div style='font-family:Inter,sans-serif;min-width:240px'>
       <div style='border-bottom:1px solid #e5e7eb;padding-bottom:6px;margin-bottom:6px'>
         <strong style='font-size:14px;color:#1f2937'>%s</strong><br>
         <span style='color:#6b7280;font-size:11px'>%s · ranking #%d</span>
       </div>
       <table style='font-size:11px;color:#374151;border-collapse:collapse;width:100%%'>
         <tr><td><strong>HLI</strong></td>             <td style='text-align:right'><strong>%.3f</strong></td></tr>
         <tr><td>Renta neta / persona</td>             <td style='text-align:right'>%s €/año</td></tr>
         <tr><td>Renta neta / hogar</td>               <td style='text-align:right'>%s €/año</td></tr>
         <tr><td>HLI residual (vs. renta)</td>         <td style='text-align:right'>%+.3f</td></tr>
         <tr><td colspan='2' style='border-top:1px solid #e5e7eb;padding-top:4px'>
           <em>Componentes del HLI</em></td></tr>
         <tr><td>Comida saludable</td>  <td style='text-align:right'>%.1f /km²</td></tr>
         <tr><td>Deporte</td>           <td style='text-align:right'>%.1f /km²</td></tr>
         <tr><td>Fast food (inverso)</td><td style='text-align:right'>%.1f /km²</td></tr>
         <tr><td>Cubierta parques</td>   <td style='text-align:right'>%.1f %%</td></tr>
       </table>
     </div>",
    htmlEscape(d$NOMBRE), htmlEscape(d$NOMDIS), d$ranking,
    d$HLI,
    formatC(d$renta_neta_persona, big.mark = ".", format = "d"),
    formatC(d$renta_neta_hogar,   big.mark = ".", format = "d"),
    d$HLI_residual,
    d$dens_comida, d$dens_deporte, d$dens_fastfood, d$pct_parques
  )
}
popups <- vapply(seq_len(nrow(hli_geo)), \(i) popup_html(hli_geo[i, ]), character(1))

attrib_maptiler <- basemap_attribution()

mapa <- leaflet(options = leafletOptions(zoomControl = TRUE,
                                          minZoom = 10, maxZoom = 16)) |>
  addTiles(urlTemplate = basemap_tiles("positron"),
            attribution = attrib_maptiler,
            group = "CartoDB Positron") |>
  addPolygons(
    data = hli_geo, fillColor = pal_hli(hli_geo$HLI), fillOpacity = 0.85,
    color = "white", weight = 0.5, opacity = 0.9,
    highlightOptions = highlightOptions(color = "#111827", weight = 2.5,
                                          bringToFront = TRUE, fillOpacity = 0.92),
    popup = popups,
    label = lapply(sprintf("<strong>%s</strong> — HLI %.2f",
                            hli_geo$NOMBRE, hli_geo$HLI), htmltools::HTML),
    labelOptions = labelOptions(textsize = "12px"),
    group = "HLI"
  ) |>
  addPolygons(
    data = hli_geo, fillColor = pal_renta(hli_geo$renta_neta_persona), fillOpacity = 0.85,
    color = "white", weight = 0.5, opacity = 0.9,
    highlightOptions = highlightOptions(color = "#111827", weight = 2.5,
                                          bringToFront = TRUE, fillOpacity = 0.92),
    popup = popups,
    label = lapply(sprintf("<strong>%s</strong> — %s €/año",
                            hli_geo$NOMBRE,
                            formatC(hli_geo$renta_neta_persona, big.mark = ".", format = "d")),
                    htmltools::HTML),
    labelOptions = labelOptions(textsize = "12px"),
    group = "Renta neta / persona"
  ) |>
  addPolygons(
    data = hli_geo, fillColor = pal_residual(hli_geo$HLI_residual), fillOpacity = 0.85,
    color = "white", weight = 0.5, opacity = 0.9,
    highlightOptions = highlightOptions(color = "#111827", weight = 2.5,
                                          bringToFront = TRUE, fillOpacity = 0.92),
    popup = popups,
    label = lapply(sprintf("<strong>%s</strong> — residual %+.3f",
                            hli_geo$NOMBRE, hli_geo$HLI_residual), htmltools::HTML),
    labelOptions = labelOptions(textsize = "12px"),
    group = "HLI residual (vs. renta)"
  ) |>
  addPolygons(data = distritos_geo, fill = FALSE, color = "#1f2937",
               weight = 1.2, opacity = 0.7, group = "Distritos") |>
  addLegend(position = "bottomright", pal = pal_hli, values = c(0, 1),
             title = "HLI", opacity = 1, bins = 5, group = "HLI") |>
  addLegend(position = "bottomright", pal = pal_renta,
             values = hli_geo$renta_neta_persona,
             title = "Renta neta (€/año)", opacity = 1, bins = 5,
             group = "Renta neta / persona",
             labFormat = labelFormat(big.mark = ".")) |>
  addLegend(position = "bottomright", pal = pal_residual,
             values = c(-rng_res, rng_res),
             title = "HLI residual", opacity = 1, bins = 5,
             group = "HLI residual (vs. renta)") |>
  addLayersControl(
    baseGroups = "CartoDB Positron",
    overlayGroups = c("HLI", "Renta neta / persona",
                      "HLI residual (vs. renta)", "Distritos"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(c("Renta neta / persona", "HLI residual (vs. renta)")) |>
  addControl(
    html = "<div style='background:rgba(255,255,255,0.95);padding:10px 14px;
              border-radius:6px;font-family:Inter,sans-serif;font-size:12px;
              box-shadow:0 1px 3px rgba(0,0,0,0.1);max-width:300px;line-height:1.5'>
              <strong style='color:#2c7a4b;font-size:13px'>HLI · renta · residual</strong><br>
              <span style='color:#6b7280'>Compara las tres lecturas. El residual
              señala los barrios con HLI por encima (verde) o por debajo (rojo)
              de lo que su renta haría esperar.</span>
            </div>",
    position = "topright"
  ) |>
  setView(lng = -3.703, lat = 40.430, zoom = 11)

saveWidget(mapa,
            file = file.path(getwd(), "outputs/maps/04_equidad_interactivo.html"),
            selfcontained = TRUE,
            title = "Madrid · HLI vs Renta")
cat("✓ outputs/maps/04_equidad_interactivo.html\n")
