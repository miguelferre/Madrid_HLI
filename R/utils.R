#' Utilidades transversales del proyecto Madrid HLI.
#'
#' Funciones de apoyo: rutas, paletas, helpers de descarga y normalización.

# CRS proyectado oficial para España peninsular (ETRS89 / UTM zona 30N).
CRS_METRICO <- 25830L
CRS_GEOGRAFICO <- 4326L

# Bounding box aproximada del término municipal de Madrid (lat/lon WGS84).
MADRID_BBOX <- c(xmin = -3.89, ymin = 40.31, xmax = -3.52, ymax = 40.56)

#' Devuelve la URL de un proveedor público de tiles SIN api key.
#'
#' Decisión: los HTML interactivos publicados en GitHub Pages NO deben
#' incluir API keys personales (la key MapTiler quedaría expuesta en
#' cleartext en cada `urlTemplate`). Por defecto usamos los tiles
#' gratuitos de CartoDB / Stadia Maps que no requieren autenticación.
#' Si en local prefieres usar MapTiler, ponla en `.Renviron` y configura
#' explícitamente `addTiles(urlTemplate = maptiler_tiles_local(...))`.
basemap_tiles <- function(style = "positron") {
  switch(style,
    positron = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
    voyager  = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png",
    dark     = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
    "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
  )
}

basemap_attribution <- function() {
  paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> ',
    'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
  )
}

# Compatibilidad con scripts antiguos: alias que NO mete API key.
maptiler_tiles <- function(style = "streets-v2") basemap_tiles("positron")
maptiler_key   <- function() ""

#' Configura pandoc apuntando al binario embebido en Quarto.
#' Necesario para `htmlwidgets::saveWidget(selfcontained = TRUE)` en Windows
#' cuando no hay un Pandoc instalado en el sistema.
configurar_pandoc <- function() {
  rutas <- c(
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/tools/pandoc.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/tools/x86_64/pandoc.exe"),
    file.path(Sys.getenv("ProgramFiles"), "RStudio/bin/quarto/bin/tools/pandoc.exe")
  )
  pandoc <- rutas[file.exists(rutas)][1]
  if (is.na(pandoc)) {
    warning("No se encontró pandoc.exe; saveWidget(selfcontained=TRUE) puede fallar.")
    return(invisible(NULL))
  }
  Sys.setenv(RSTUDIO_PANDOC = dirname(pandoc))
  invisible(pandoc)
}

#' Normaliza un raster o vector numérico al rango [0, 1].
normalizar_01 <- function(x, invertir = FALSE) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(x * 0)
  z <- (x - rng[1]) / diff(rng)
  if (invertir) z <- 1 - z
  z
}
