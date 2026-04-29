#' Utilidades transversales del proyecto Madrid HLI.
#'
#' Funciones de apoyo: rutas, paletas, helpers de descarga y normalización.

# CRS proyectado oficial para España peninsular (ETRS89 / UTM zona 30N).
CRS_METRICO <- 25830L
CRS_GEOGRAFICO <- 4326L

# Bounding box aproximada del término municipal de Madrid (lat/lon WGS84).
MADRID_BBOX <- c(xmin = -3.89, ymin = 40.31, xmax = -3.52, ymax = 40.56)

#' Obtiene la API key de MapTiler desde .Renviron.
maptiler_key <- function() {
  key <- Sys.getenv("MAPTILER_API_KEY", unset = "")
  if (!nzchar(key)) {
    stop("MAPTILER_API_KEY no encontrada. Copia .Renviron.example a .Renviron y rellena la clave.")
  }
  key
}

#' Construye la URL de un estilo MapTiler para usar como tile en leaflet.
maptiler_tiles <- function(style = "streets-v2") {
  sprintf("https://api.maptiler.com/maps/%s/{z}/{x}/{y}.png?key=%s", style, maptiler_key())
}

#' Normaliza un raster o vector numérico al rango [0, 1].
normalizar_01 <- function(x, invertir = FALSE) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(x * 0)
  z <- (x - rng[1]) / diff(rng)
  if (invertir) z <- 1 - z
  z
}
