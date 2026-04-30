#' Madrid Healthy Living Index — dashboard interactivo
#' Despliegue: shinylive::export(appdir = "app", destdir = "docs/app")

library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# Carga de datos --------------------------------------------------------------

barrios   <- st_read("data/barrios.geojson",   quiet = TRUE)
distritos <- st_read("data/distritos.geojson", quiet = TRUE)
municipio <- st_read("data/municipio.geojson", quiet = TRUE)

# Catálogo de métricas (etiqueta · paleta · dirección · descripción) ----------

METRICAS <- list(
  HLI = list(
    label = "HLI v1 (índice compuesto)",
    paleta = c("#440154","#3b528b","#21918c","#5ec962","#fde725"),
    domain = c(0, 1), unidad = "",
    desc = "Media de 4 indicadores: comida saludable, deporte, parques (+) y fast food (−)."
  ),
  score_3_30_300 = list(
    label = "Score 3-30-300 (% nodos cumplen las tres)",
    paleta = c("#fde725","#5ec962","#21918c","#3b528b","#440154"),
    domain = c(0, 60), unidad = "%",
    desc = "Porcentaje de nodos del callejero que cumplen las tres reglas a la vez."
  ),
  regla_3_pct = list(
    label = "Regla 3 — ≥3 árboles a 50 m",
    paleta = c("#fff7bc","#fee391","#fec44f","#fe9929","#cc4c02"),
    domain = c(0, 100), unidad = "%",
    desc = "% de nodos del callejero con tres o más árboles inventariados a ≤50 m."
  ),
  regla_30_pct = list(
    label = "Regla 30 — ≥30 % canopy en 250 m",
    paleta = c("#f7fcb9","#addd8e","#41ab5d","#238443","#005a32"),
    domain = c(0, 100), unidad = "%",
    desc = "% de nodos cuyo entorno (250 m) tiene 30 % o más de cubierta arbórea (ESA WorldCover)."
  ),
  regla_300_pct = list(
    label = "Regla 300 — parque ≥1 ha a 5 min",
    paleta = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
    domain = c(0, 100), unidad = "%",
    desc = "% de nodos con un parque ≥1 ha a ≤400 m peatonales (isócronas con dodgr)."
  ),
  renta_neta_persona = list(
    label = "Renta neta por persona (€/año)",
    paleta = c("#f7fbff","#c6dbef","#6baed6","#2171b5","#08306b"),
    domain = c(11000, 36000), unidad = " €",
    desc = "Atlas de Distribución de Renta de los Hogares 2023 (INE), agregado a barrio."
  ),
  dens_arboles_ha = list(
    label = "Densidad de árboles (n/ha)",
    paleta = c("#fff7bc","#fee391","#fec44f","#fe9929","#cc4c02"),
    domain = c(0, 100), unidad = "/ha",
    desc = "Árboles del inventario municipal por hectárea de barrio."
  ),
  pct_parques = list(
    label = "Cubierta de parques (%)",
    paleta = c("#f7fcb9","#addd8e","#41ab5d","#238443","#005a32"),
    domain = c(0, 80), unidad = "%",
    desc = "Porcentaje del barrio cubierto por parques OSM (recortados al barrio)."
  )
)

VAR_RADAR <- c("HLI", "regla_3_pct", "regla_30_pct", "regla_300_pct",
               "score_3_30_300", "dens_arboles_ha", "pct_parques")
VAR_RADAR_LAB <- c("HLI", "Regla 3", "Regla 30", "Regla 300",
                    "Score 3-30-300", "Árb./ha", "% parques")

normaliza_01 <- function(x) {
  q <- quantile(x, c(0.05, 0.95), na.rm = TRUE)
  if (diff(q) == 0) return(rep(0, length(x)))
  pmin(pmax((x - q[1]) / diff(q), 0), 1)
}

barrios_norm <- as.data.frame(st_drop_geometry(barrios))
for (v in VAR_RADAR) barrios_norm[[paste0(v, "_n")]] <- normaliza_01(barrios_norm[[v]])

# UI --------------------------------------------------------------------------

ui <- page_navbar(
  title = "Madrid · Healthy Living Index",
  theme = bs_theme(bootswatch = "minty"),
  fillable = TRUE,

  nav_panel(
    title = "Mapa",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h5("Métrica", style = "margin-top:0"),
        selectInput("var", NULL,
                    choices = setNames(names(METRICAS),
                                          sapply(METRICAS, \(x) x$label)),
                    selected = "score_3_30_300"),
        uiOutput("desc"),
        hr(),
        selectInput("distrito_filt", "Filtrar por distrito (opcional):",
                    choices = c("Todos" = "ALL", sort(unique(barrios$NOMDIS))),
                    selected = "ALL"),
        hr(),
        h6("Top 5 barrios"),
        tableOutput("top5"),
        h6("Bottom 5 barrios"),
        tableOutput("bot5")
      ),
      leafletOutput("mapa", height = "85vh")
    )
  ),

  nav_panel(
    title = "Comparar",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h5("Selecciona barrios"),
        selectizeInput("barrios_sel", NULL,
                       choices = sort(barrios$NOMBRE),
                       multiple = TRUE,
                       selected = c("Castellana", "Comillas", "Atalaya", "Sol"),
                       options = list(maxItems = 6,
                                       placeholder = "Hasta 6 barrios")),
        helpText("Cada eje del radar está normalizado al rango",
                  tags$strong("[p5, p95]"),
                  "del conjunto de barrios. 1 = top 5 % de Madrid."),
        hr(),
        h6("Tabla comparativa"),
        DTOutput("tabla_comp", height = "auto")
      ),
      plotlyOutput("radar", height = "85vh")
    )
  ),

  nav_panel(
    title = "Equidad",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("HLI v1 ↔ renta neta por persona"),
        plotlyOutput("scatter_hli", height = "75vh")
      ),
      card(
        card_header("Score 3-30-300 ↔ renta neta por persona"),
        plotlyOutput("scatter_330", height = "75vh")
      )
    )
  ),

  nav_panel(
    title = "Datos",
    card(
      card_header("Tabla completa por barrio · busca, ordena, exporta"),
      DTOutput("tabla_full", height = "85vh")
    )
  ),

  nav_spacer(),
  nav_item(tags$a(href = "https://github.com/miguelferre/Madrid_HLI",
                    "GitHub", target = "_blank"))
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  filt <- reactive({
    if (input$distrito_filt == "ALL") barrios
    else barrios[barrios$NOMDIS == input$distrito_filt, ]
  })

  output$desc <- renderUI({
    m <- METRICAS[[input$var]]
    div(style = "color:#4b5563;font-size:12px;line-height:1.5",
        tags$em(m$desc))
  })

  output$top5 <- renderTable({
    m <- METRICAS[[input$var]]
    df <- st_drop_geometry(filt())
    df |> arrange(desc(.data[[input$var]])) |> head(5) |>
      transmute(Barrio = NOMBRE,
                Distrito = NOMDIS,
                Valor = round(.data[[input$var]], 2))
  }, striped = TRUE, spacing = "xs", width = "100%")

  output$bot5 <- renderTable({
    df <- st_drop_geometry(filt())
    df |> arrange(.data[[input$var]]) |> head(5) |>
      transmute(Barrio = NOMBRE,
                Distrito = NOMDIS,
                Valor = round(.data[[input$var]], 2))
  }, striped = TRUE, spacing = "xs", width = "100%")

  output$mapa <- renderLeaflet({
    m <- METRICAS[[input$var]]
    df <- filt()
    pal <- colorNumeric(m$paleta, domain = m$domain, na.color = "#cccccc")

    popups <- sprintf(
      "<div style='font-family:Inter;min-width:240px'>
         <strong style='font-size:14px'>%s</strong><br>
         <span style='color:#6b7280;font-size:11px'>%s</span>
         <table style='font-size:11px;margin-top:6px;width:100%%'>
           <tr><td><strong>%s</strong></td><td style='text-align:right'>
              <strong>%s%s</strong></td></tr>
           <tr><td>HLI v1</td><td style='text-align:right'>%.3f (#%d)</td></tr>
           <tr><td>Score 3-30-300</td><td style='text-align:right'>%.1f %% (#%d)</td></tr>
           <tr><td>Renta neta</td><td style='text-align:right'>%s €</td></tr>
           <tr><td>Acceso 5 min</td><td style='text-align:right'>%.1f %%</td></tr>
           <tr><td>Densidad árboles</td><td style='text-align:right'>%.1f / ha</td></tr>
         </table></div>",
      df$NOMBRE, df$NOMDIS,
      m$label,
      formatC(df[[input$var]], format = "f", digits = 2, big.mark = "."),
      m$unidad,
      df$HLI, df$ranking,
      df$score_3_30_300, df$rank_3_30_300,
      ifelse(is.na(df$renta_neta_persona), "—",
              formatC(df$renta_neta_persona, big.mark = ".", format = "d")),
      df$acc_5min, df$dens_arboles_ha
    )

    leaflet(options = leafletOptions(minZoom = 10, maxZoom = 16)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(data = df,
                  fillColor = pal(df[[input$var]]),
                  fillOpacity = 0.85,
                  color = "white", weight = 0.5,
                  highlightOptions = highlightOptions(
                    color = "#111827", weight = 2.5,
                    fillOpacity = 0.92, bringToFront = TRUE),
                  popup = popups,
                  label = lapply(sprintf("<strong>%s</strong>", df$NOMBRE),
                                  HTML)) |>
      addPolygons(data = distritos, fill = FALSE, color = "#1f2937",
                  weight = 1.2, opacity = 0.7) |>
      addLegend(position = "bottomright", pal = pal, values = m$domain,
                title = paste0(m$label),
                opacity = 1, bins = 5) |>
      setView(lng = -3.703, lat = 40.430, zoom = 11)
  })

  # --- Comparar ------------------------------------------------------------

  output$radar <- renderPlotly({
    sel <- input$barrios_sel
    if (length(sel) == 0) return(plotly_empty(type = "scatterpolar"))
    df <- barrios_norm |> filter(NOMBRE %in% sel)

    fig <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
    pal <- c("#0f766e","#dc2626","#2563eb","#ca8a04","#7c3aed","#db2777")
    for (i in seq_len(nrow(df))) {
      vals <- as.numeric(df[i, paste0(VAR_RADAR, "_n")])
      fig <- fig |> add_trace(
        r = c(vals, vals[1]),
        theta = c(VAR_RADAR_LAB, VAR_RADAR_LAB[1]),
        name = df$NOMBRE[i],
        line = list(color = pal[(i-1) %% length(pal) + 1], width = 2),
        marker = list(size = 6),
        opacity = 0.55
      )
    }
    fig |> layout(
      polar = list(radialaxis = list(range = c(0, 1),
                                       tickformat = ".1f",
                                       gridcolor = "#e5e7eb"),
                    bgcolor = "#fafaf7"),
      showlegend = TRUE,
      paper_bgcolor = "#fafaf7",
      margin = list(t = 30, b = 30)
    )
  })

  output$tabla_comp <- renderDT({
    sel <- input$barrios_sel
    if (length(sel) == 0) return(NULL)
    df <- st_drop_geometry(barrios) |>
      filter(NOMBRE %in% sel) |>
      transmute(Barrio = NOMBRE,
                HLI = round(HLI, 3),
                `R3 %` = regla_3_pct,
                `R30 %` = regla_30_pct,
                `R300 %` = regla_300_pct,
                `Score` = score_3_30_300,
                `Renta` = renta_neta_persona)
    datatable(df, options = list(dom = "t", paging = FALSE),
              rownames = FALSE)
  })

  # --- Equidad -------------------------------------------------------------

  output$scatter_hli <- renderPlotly({
    df <- st_drop_geometry(barrios) |>
      filter(!is.na(renta_neta_persona))
    fit <- lm(HLI ~ renta_neta_persona, data = df)
    df$pred <- predict(fit, df)
    plot_ly(df, x = ~renta_neta_persona, y = ~HLI,
            text = ~paste0(NOMBRE, " · ", NOMDIS,
                            "<br>HLI: ", round(HLI, 3),
                            "<br>Renta: ", formatC(renta_neta_persona, big.mark = ".", format = "d"), " €"),
            hoverinfo = "text", type = "scatter", mode = "markers",
            marker = list(color = "#0f766e", size = 8, opacity = 0.7,
                          line = list(color = "white", width = 1))) |>
      add_lines(x = ~renta_neta_persona, y = ~pred,
                line = list(color = "#dc2626", width = 2, dash = "dash"),
                hoverinfo = "skip", showlegend = FALSE) |>
      layout(xaxis = list(title = "Renta neta por persona (€/año)"),
              yaxis = list(title = "HLI v1"),
              annotations = list(
                list(x = 0.98, y = 0.98, xref = "paper", yref = "paper",
                     text = sprintf("Pearson = %.3f",
                                     cor(df$HLI, df$renta_neta_persona)),
                     showarrow = FALSE, font = list(size = 14, color = "#dc2626"))),
              paper_bgcolor = "#fafaf7", plot_bgcolor = "#fafaf7")
  })

  output$scatter_330 <- renderPlotly({
    df <- st_drop_geometry(barrios) |>
      filter(!is.na(renta_neta_persona) & !is.na(score_3_30_300))
    fit <- lm(score_3_30_300 ~ renta_neta_persona, data = df)
    df$pred <- predict(fit, df)
    plot_ly(df, x = ~renta_neta_persona, y = ~score_3_30_300,
            text = ~paste0(NOMBRE, " · ", NOMDIS,
                            "<br>Score: ", round(score_3_30_300, 1), " %",
                            "<br>Renta: ", formatC(renta_neta_persona, big.mark = ".", format = "d"), " €"),
            hoverinfo = "text", type = "scatter", mode = "markers",
            marker = list(color = "#7c3aed", size = 8, opacity = 0.7,
                          line = list(color = "white", width = 1))) |>
      add_lines(x = ~renta_neta_persona, y = ~pred,
                line = list(color = "#dc2626", width = 2, dash = "dash"),
                hoverinfo = "skip", showlegend = FALSE) |>
      layout(xaxis = list(title = "Renta neta por persona (€/año)"),
              yaxis = list(title = "Score 3-30-300 (% nodos)"),
              annotations = list(
                list(x = 0.98, y = 0.98, xref = "paper", yref = "paper",
                     text = sprintf("Pearson = %.3f",
                                     cor(df$score_3_30_300, df$renta_neta_persona)),
                     showarrow = FALSE, font = list(size = 14, color = "#dc2626"))),
              paper_bgcolor = "#fafaf7", plot_bgcolor = "#fafaf7")
  })

  # --- Tabla ---------------------------------------------------------------

  output$tabla_full <- renderDT({
    df <- st_drop_geometry(barrios) |>
      transmute(Distrito = NOMDIS, Barrio = NOMBRE,
                `HLI` = round(HLI, 3), `Rank HLI` = ranking,
                `Score 3-30-300` = score_3_30_300,
                `Rank 3-30-300` = rank_3_30_300,
                `R3 %` = regla_3_pct,
                `R30 %` = regla_30_pct,
                `R300 %` = regla_300_pct,
                `Renta €` = renta_neta_persona,
                `Árb./ha` = round(dens_arboles_ha, 1),
                `Canopy med %` = canopy_med,
                `Acc 5 min %` = acc_5min,
                `Acc 15 min %` = acc_15min,
                `% Parques` = round(pct_parques, 1))
    datatable(df,
              extensions = c("Buttons", "FixedHeader"),
              options = list(
                pageLength = 25,
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel"),
                fixedHeader = TRUE,
                scrollX = TRUE),
              rownames = FALSE) |>
      formatStyle("HLI", fontWeight = "bold")
  })
}

shinyApp(ui, server)
