# Madrid HLI — Healthy Living Index

> Análisis espacial del nivel de vida saludable en la ciudad de Madrid, cruzado con el estándar urbanístico **3-30-300** y datos socioeconómicos.

![status](https://img.shields.io/badge/estado-en%20desarrollo-orange) ![R](https://img.shields.io/badge/R-4.5-blue) ![license](https://img.shields.io/badge/license-MIT-green)

## ¿Qué hace este proyecto?

Combina datos abiertos de OpenStreetMap, INE, Ayuntamiento de Madrid y satélites Sentinel/Meta para responder preguntas concretas sobre la habitabilidad saludable del municipio:

- ¿Dónde es **fácil** llevar un estilo de vida saludable en Madrid?
- ¿Cuántos barrios cumplen la regla **3-30-300** de urbanismo verde de Cecil Konijnendijk?
- ¿Hay **desigualdad** entre barrios? ¿Correlaciona el índice con la renta?
- ¿Cuáles son los barrios mejor y peor servidos?

## Hallazgo principal (versión actual)

El **HLI correlaciona negativamente con la renta** en Madrid (Pearson −0,26; Spearman −0,25). Los barrios con mejor dotación pública para una vida saludable se concentran en el **sur obrero** (Carabanchel, Usera, Puente de Vallecas), mientras que los barrios de mayor renta del **norte** (Salamanca, Chamartín, Moncloa) caen al fondo del ranking. La equidad en oferta de mercados, polideportivos y parques no la compra el dinero.

| | alto HLI | bajo HLI |
|---|---|---|
| **renta alta** | 30 barrios | 36 barrios — *trampas* |
| **renta baja** | 36 barrios — *gangas* | 29 barrios |

- *Top "gangas":* Comillas, Abrantes, Pradolongo, Zofío, San Isidro
- *Top "trampas":* Valdemarín, Recoletos, Fuentelarreina, El Viso, Castellana
- *Caso extremo:* Sol — HLI 0,12, 123 fast food/km², 0% cubierta verde

### Hallazgo complementario · 3-30-300

Al evaluar la **regla 3-30-300** sobre los 605 K nodos del callejero, Madrid suspende: **solo el 9 % mediano** de los barrios cumple las tres reglas a la vez. La descomposición es reveladora:

- **Regla 3** (≥ 3 árboles a 50 m) → 64 % a nivel ciudad. Se cumple casi en todas partes salvo Casa de Campo, Aeropuerto y El Pardo (zonas no urbanizadas con bosque no inventariado individualmente).
- **Regla 30** (≥ 30 % canopy en 250 m) → solo 27 % de los nodos. **Toda Salamanca, Centro y Retiro caen a 0 %** porque los árboles viario de alineación, aunque numerosos, no llegan al 30 % de cubierta continua.
- **Regla 300** (parque ≥ 1 ha a 5 min) → patrón ya identificado: el norte rico está peor servido que el sur obrero (ρ con renta = −0,24).

El **score combinado** borra la correlación con renta (Pearson −0,04 vs −0,26 del HLI v1): cada componente capta una dimensión distinta y se compensan. Top barrios 3-30-300: **Atalaya** (Ciudad Lineal) 60 %, **Marroquina** (Moratalaz) 48 %, **Vinateros** 39 %.

## Estructura del repositorio

```
Madrid_HLI/
├── index.qmd              # documento Quarto principal (en construcción)
├── R/                     # pipeline numerado de scripts
│   ├── 01_descargar_admin.R       distritos + barrios + municipio
│   ├── 02_mapa_base.R             mapa base estático e interactivo
│   ├── 03_descargar_osm.R         POIs OSM (4 capas, queries por-tag)
│   ├── 04_kde.R                   KDE proyectado de densidades
│   ├── 05_mapas_kde.R             lámina KDE multipanel
│   ├── 06_mapa_pois_interactivo.R mapa filtrable de validación
│   ├── 07_agregacion_barrios.R    HLI por barrio + ranking
│   ├── 08_mapa_hli.R              cartografía del ranking
│   ├── 09_renta_ine.R             cruce con ADRH 2023
│   ├── 10_visualizar_equidad.R    scatter + lámina dual
│   ├── 11_isocronas_dodgr.R       accesibilidad peatonal a parques (Nivel 3 A)
│   ├── 12_mapa_accesibilidad.R    cartografía de la accesibilidad
│   ├── 13_arbolado_madrid.R       inventario de árboles del Ayto
│   ├── 14_regla_3_arboles.R       regla 3 — ≥3 árboles a 50 m
│   ├── 15_regla_30_canopy.R       regla 30 — ≥30 % canopy en 250 m
│   ├── 16_score_3_30_300.R        score combinado por barrio
│   ├── 17_mapa_3_30_300.R         cartografía + mapa interactivo
│   └── 18_preparar_dashboard.R    GeoJSON simplificado para shinylive
├── app/                  # dashboard Shiny (deployable como WebAssembly)
│   ├── app.R                       UI + server (4 pestañas)
│   └── data/                       barrios.geojson · distritos.geojson · municipio.geojson
├── data/
│   ├── raw/               # descargas crudas (gitignored)
│   └── processed/         # datasets procesados (.gpkg + .csv)
├── outputs/
│   ├── maps/              # mapas interactivos HTML
│   ├── figures/           # láminas estáticas
│   └── tables/            # tablas resumen
├── docs/                  # publicación GitHub Pages
└── legacy/                # versión original del notebook
```

## Estado del proyecto

| Bloque | Estado |
|---|---|
| Descarga y limpieza de datos administrativos | ✅ |
| POIs OSM (comida, deporte, fast food, parques) | ✅ |
| KDE proyectado y mapas estáticos | ✅ |
| Mapa interactivo de validación de POIs | ✅ |
| Agregación a 131 barrios + HLI v1 + ranking | ✅ |
| Cartografía del HLI (estática + interactiva) | ✅ |
| Cruce con renta INE (ADRH 2023) | ✅ |
| Visualización del eje HLI ↔ renta | ✅ |
| Isocronas peatonales con `dodgr` | ✅ |
| Regla 3-30-300 (arbolado + canopy + accesibilidad) | ✅ |
| Dashboard `shinylive` en GitHub Pages | ✅ |

## Salidas destacadas

- `outputs/figures/03_hli_choropleth.png` — coroplético del HLI por barrio.
- `outputs/figures/05_hli_vs_renta.png` — scatter HLI ↔ renta con cuadrantes.
- `outputs/figures/06_panel_equidad.png` — lámina dual HLI / renta.
- `outputs/maps/02_pois_filtrable.html` — validación visual de los POIs OSM.
- `outputs/maps/03_hli_interactivo.html` — HLI con desglose por componente.
- `outputs/maps/04_equidad_interactivo.html` — HLI, renta y residual.
- `outputs/maps/05_accesibilidad_interactivo.html` — accesibilidad peatonal a parques ≥1 ha (5/10/15 min).
- `outputs/maps/06_3_30_300_interactivo.html` — regla 3-30-300 barrio a barrio.
- `outputs/figures/14_panel_3_30_300.png` — lámina con las 3 reglas + score combinado.
- `docs/app/index.html` — **dashboard `shinylive`** (mapa + comparativa + equidad + tabla descargable).

## Cómo reproducirlo

> Pendiente de finalización al cierre del proyecto.

Requisitos:
- R 4.5+
- Rtools 4.5 (Windows)
- Quarto 1.9+
- Una API key gratuita de [MapTiler](https://www.maptiler.com/) en `.Renviron` como `MAPTILER_API_KEY=...`

```r
# Restaurar el entorno exacto
renv::restore()

# Pipeline completo (los scripts son idempotentes salvo R/03 que llama Overpass)
for (s in sort(list.files("R", pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) {
  message("→ ", s)
  source(s)
}

# Renderizar el documento
quarto::quarto_render("index.qmd")
```

## Metodología

1. **Capas de entrada (OSM):** comida saludable, gimnasios, fast food, parques. Whitelist explícita de subtags validada visualmente para limpiar el ruido cruzado del parser. `landuse=forest` se excluye salvo el rescate manual de Casa de Campo (1.373 ha) que en OSM no aparece como `leisure=park`.
2. **KDE proyectado** (EPSG:25830) con bandwidth de Scott — densidades estadísticamente correctas.
3. **Agregación a 131 barrios:** densidad de POIs por km² + porcentaje de cubierta de parques.
4. **HLI compuesto:** media de cuatro indicadores normalizados al rango [0, 1] con cuantiles 5–95 (robusto a outliers tipo Sol con 123 fast food/km²); fast food invertido.
5. **Renta:** Atlas de Distribución de Renta de los Hogares 2023 del INE — agregada de sección censal a barrio del Ayuntamiento por intersección espacial (centroide).
6. **Accesibilidad peatonal con `dodgr`:** sobre la red de calles del extracto Geofabrik (152 K segmentos · 605 K vértices) se calcula la distancia mínima al parque ≥ 1 ha más cercano y se marcan los nodos accesibles a 5 / 10 / 15 min (perfil peatonal a 4,8 km/h ≈ 80 m/min).
7. **Regla 3-30-300 (Konijnendijk):** evaluada por nodo del callejero. *Regla 3* — ≥ 3 árboles a 50 m, calculada con `terra::rasterize` + focal sum sobre los 793 K árboles del inventario municipal. *Regla 30* — ≥ 30 % de cubierta arbórea en buffer 250 m, derivada del raster ESA WorldCover 2021 v200 (10 m, clase Tree cover). *Regla 300* — parque ≥ 1 ha a ≤ 5 min andando, equivale a `acc_5min` del paso 6. El score combinado es el % de nodos del barrio que cumplen las tres a la vez.
8. **Dashboard interactivo (`shinylive`):** la app `app/app.R` se exporta como WebAssembly puro y queda servida desde GitHub Pages sin necesidad de servidor Shiny. Permite explorar las métricas barrio a barrio, comparar hasta seis barrios en un radar plotly y descargar el dataset completo en CSV/Excel.

## Fuentes de datos

| Capa | Fuente | Licencia |
|---|---|---|
| POIs (gimnasios, comida, fast food, parques) | OpenStreetMap (Overpass API) | ODbL |
| Distritos y barrios | Ayuntamiento de Madrid (Geoportal IDEAM) | CC BY 4.0 |
| Renta por sección censal | INE — Atlas de Distribución de Renta de los Hogares 2023 (tabla 30824) | Reutilización libre |
| Cartografía secciones censales | INE — Cartografía Digitalizada 2024 | Reutilización libre |
| Arbolado urbano | Ayuntamiento de Madrid (datos.madrid.es, dataset 300761) | CC BY 4.0 |
| Cubierta arbórea | ESA WorldCover 2021 v200 (10 m) | CC BY 4.0 |
| Basemaps | MapTiler | API key personal |

## Autor

**Miguel Ferreiro García** — proyecto inicialmente desarrollado en un curso de SIG con R, refactorizado en 2026 como pieza de portfolio.

## Licencia

[MIT](LICENSE) para el código. Datos bajo sus respectivas licencias originales.
