# Madrid HLI — Healthy Living Index

> Análisis espacial del nivel de vida saludable en la ciudad de Madrid, cruzado con el estándar urbanístico **3-30-300** y datos socioeconómicos.

![status](https://img.shields.io/badge/estado-en%20desarrollo-orange) ![R](https://img.shields.io/badge/R-4.5-blue) ![license](https://img.shields.io/badge/license-MIT-green)

## ¿Qué hace este proyecto?

Combina datos abiertos de OpenStreetMap, INE, Ayuntamiento de Madrid y satélites Sentinel/Meta para responder a preguntas concretas sobre la habitabilidad saludable del municipio:

- ¿Dónde es **fácil** llevar un estilo de vida saludable en Madrid?
- ¿Cuántos barrios cumplen la regla **3-30-300** de urbanismo verde de Cecil Konijnendijk?
- ¿Hay **desigualdad** entre barrios? ¿Correlaciona el índice con la renta?
- ¿Cuáles son los barrios mejor y peor servidos?

## Estructura del repositorio

```
Madrid_HLI/
├── index.qmd              # documento Quarto principal (en construcción)
├── R/                     # funciones reutilizables
├── data/
│   ├── raw/               # descargas crudas (gitignored)
│   └── processed/         # datasets procesados
├── outputs/
│   ├── maps/              # mapas interactivos HTML
│   ├── figures/           # láminas estáticas para el README
│   └── tables/            # tablas resumen CSV
├── docs/                  # publicación GitHub Pages
└── legacy/                # versión original del notebook
```

## Cómo reproducirlo

> Esta sección se completará al final del proyecto.

Requisitos previos:
- R 4.5+
- Rtools 4.5 (Windows)
- Quarto

```r
# Restaurar el entorno exacto
renv::restore()

# Renderizar el documento
quarto::quarto_render("index.qmd")
```

## Metodología

> Documentación detallada en construcción. Resumen:

1. **Capas de entrada** (OSM): comida saludable, gimnasios, fast food, parques.
2. **KDE proyectado** (EPSG:25830) para densidades.
3. **Isocronas peatonales** con `dodgr` sobre la red OSM real.
4. **Cubierta arbórea**: arbolado del Ayto + Meta Canopy Height 1 m.
5. **Agregación a barrios** (131 barrios de Madrid).
6. **Cruce con renta** (INE — Atlas de Distribución de Renta de los Hogares).

## Fuentes de datos

| Capa | Fuente | Licencia |
|---|---|---|
| POIs (gimnasios, comida, fast food) | OpenStreetMap | ODbL |
| Distritos y barrios | Ayuntamiento de Madrid | CC BY 4.0 |
| Renta por sección censal | INE — ADRH | Reutilización libre |
| Arbolado urbano | Ayuntamiento de Madrid | CC BY 4.0 |
| Cubierta arbórea | Meta/WRI Global Canopy Height 2024 | CC BY 4.0 |
| Basemaps | MapTiler | API key personal |

## Autor

**Miguel Ferreiro García** — Proyecto inicialmente desarrollado en un curso de SIG con R, refactorizado en 2026 como pieza de portfolio.

## Licencia

[MIT](LICENSE) — código. Datos bajo sus respectivas licencias originales.
