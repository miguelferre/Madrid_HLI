#' Lámina multipanel con las tres capas de densidad KDE.
#' Salida: outputs/figures/02_kde_panel.png

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(ggplot2)
  library(patchwork)
  library(viridis)
  library(dplyr)
})

source("R/utils.R")

municipio <- st_read("data/processed/municipio.gpkg", quiet = TRUE)
distritos <- st_read("data/processed/distritos.gpkg", quiet = TRUE)

cargar <- function(ruta) rast(ruta)

kde_a <- cargar("data/processed/kde_comida_saludable.tif")
kde_g <- cargar("data/processed/kde_gimnasios.tif")
kde_f <- cargar("data/processed/kde_fast_food.tif")

# Convertir cada raster a data.frame para ggplot
raster_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  df
}

mapa_kde <- function(r, titulo, subtitulo, paleta) {
  df <- raster_to_df(r)
  p95 <- quantile(df$valor, 0.99, na.rm = TRUE)

  ggplot() +
    geom_raster(data = df, aes(x, y, fill = pmin(valor, p95))) +
    geom_sf(data = distritos, fill = NA, color = "white", linewidth = 0.25, alpha = 0.6) +
    geom_sf(data = municipio, fill = NA, color = "white", linewidth = 0.7) +
    scale_fill_gradientn(
      colours = paleta,
      name = "POIs / km²",
      guide = guide_colorbar(barheight = unit(8, "lines"), barwidth = unit(0.4, "lines"),
                              ticks.colour = NA, frame.colour = NA)
    ) +
    coord_sf(crs = st_crs(municipio), expand = FALSE) +
    labs(title = titulo, subtitle = subtitulo) +
    theme_void(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#f3f4f6"),
      plot.subtitle = element_text(size = 10, color = "#d1d5db", margin = margin(b = 6)),
      plot.background = element_rect(fill = "#1f2937", color = NA),
      panel.background = element_rect(fill = "#1f2937", color = NA),
      legend.title = element_text(size = 9, color = "#f3f4f6"),
      legend.text = element_text(size = 8, color = "#d1d5db"),
      legend.position = "right",
      plot.margin = margin(8, 8, 8, 8)
    )
}

p1 <- mapa_kde(kde_a, "Comida saludable",  "Fruterías, mercados, herbolarios y especializadas", viridis(20, option = "G"))
p2 <- mapa_kde(kde_g, "Actividad física",  "Gimnasios, polideportivos y estaciones de calistenia", viridis(20, option = "D"))
p3 <- mapa_kde(kde_f, "Comida rápida",     "Fast food (factor inverso del índice)",                viridis(20, option = "B"))

panel <- (p1 | p2 | p3) +
  plot_annotation(
    title = "Madrid · densidades KDE de equipamientos urbanos",
    subtitle = "Estimación proyectada (EPSG:25830) con bandwidth de Scott — recortada al término municipal",
    caption = "Datos: OpenStreetMap (ODbL) · Análisis: Miguel Ferreiro García",
    theme = theme(
      plot.background = element_rect(fill = "#1f2937", color = NA),
      plot.title = element_text(face = "bold", size = 17, color = "white", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "#d1d5db", hjust = 0.5, margin = margin(b = 12)),
      plot.caption = element_text(size = 8, color = "#9ca3af", hjust = 0.5, margin = margin(t = 10))
    )
  )

ggsave("outputs/figures/02_kde_panel.png", panel,
       width = 16, height = 7, dpi = 200, bg = "#1f2937")

cat("✓ Lámina KDE guardada: outputs/figures/02_kde_panel.png\n")
