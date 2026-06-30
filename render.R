#!/usr/bin/env Rscript
# =====================================================================
# render.R - Wrapper de renderizado del libro con pre-procesado.
# ---------------------------------------------------------------------
# Para cada uso:
#   1. Crea (o limpia) una carpeta _build/ con los ficheros del libro.
#   2. Copia config, assets, y los .Rmd numerados ya procesados por
#      numerar_tablas_figuras.R.
#   3. Lanza bookdown::render_book() desde _build/, dejando el resultado
#      en _book/ junto al proyecto.
#
# El número de capítulo se infiere del prefijo del nombre del fichero,
# por ejemplo "05-analisis.Rmd" -> capítulo 5.
#
# Uso:
#   Rscript render.R                 # Formato por defecto
#   Rscript render.R bookdown::pdf_book
# =====================================================================

source("numerar_tablas_figuras.R")

# --- 1. Configuración --------------------------------------------------

# Carpeta de trabajo donde se prepara y renderiza el libro
build_dir  <- "_build"

# Carpeta de salida del libro. Si _bookdown.yml define `output_dir`, ese
# valor manda (se respeta el flujo existente del proyecto). Si no, _book.
output_dir <- "_book"
if (file.exists("_bookdown.yml") &&
    requireNamespace("yaml", quietly = TRUE)) {
  yml_data <- yaml::read_yaml("_bookdown.yml")
  if (!is.null(yml_data$output_dir)) output_dir <- yml_data$output_dir
}

# Ficheros de configuración / portada / preámbulo que hay que llevar
# tal cual a _build/. Si tu proyecto usa otros nombres, edítalos aquí.
config_files <- c(
  "index.Rmd",          # capítulo 0, lleva el YAML de cabecera
  "_bookdown.yml",
  "_output.yml",
  "preamble.tex",
  "style.css",
  "_common.R",
  "DESCRIPTION"
)

# Subcarpetas con assets (imágenes, datos, descargas, etc.).
asset_dirs <- c(
  "figuras",
  "images",
  "data",
  "download",
  "css",
  "js"
)

# Formato de salida. Si no se pasa por argumento, se deja NULL para que
# bookdown use el primer formato definido en _output.yml (esto preserva
# bs4_book, gitbook, pdf_book... según lo que tengas configurado en el
# proyecto).
args <- commandArgs(trailingOnly = TRUE)
formato <- if (length(args) >= 1) args[1] else NULL

# --- 2. Limpiar y crear _build/ ---------------------------------------

if (dir.exists(build_dir)) unlink(build_dir, recursive = TRUE)
dir.create(build_dir)

cat(sprintf("[1/4] Carpeta '%s/' creada.\n", build_dir))

# --- 3. Copiar config y assets ----------------------------------------

copiados_config <- 0
for (f in config_files) {
  if (file.exists(f)) {
    file.copy(f, file.path(build_dir, f), overwrite = TRUE)
    copiados_config <- copiados_config + 1
  }
}

copiados_assets <- 0
for (d in asset_dirs) {
  if (dir.exists(d)) {
    file.copy(d, build_dir, recursive = TRUE)
    copiados_assets <- copiados_assets + 1
  }
}

# Ficheros sueltos de datos en la raíz (xlsx, csv, bib, etc.) que los chunks
# o pandoc podrían requerir por nombre simple sin path.
data_files <- list.files(
  pattern = "\\.(xlsx|xls|csv|tsv|rds|RData|rda|txt|json|parquet|bib|csl)$",
  ignore.case = TRUE
)
copiados_datos <- 0
for (f in data_files) {
  file.copy(f, file.path(build_dir, f), overwrite = TRUE)
  copiados_datos <- copiados_datos + 1
}

cat(sprintf("[2/4] Copiados %d archivos de config, %d carpetas de assets y %d ficheros de datos.\n",
            copiados_config, copiados_assets, copiados_datos))

# --- 4. Procesar Rmd numerados ---------------------------------------

# Ficheros de capítulo: empiezan por dígitos y terminan en .Rmd
rmds <- list.files(pattern = "^[0-9]+.*\\.Rmd$")

if (length(rmds) == 0) {
  warning("No se encontraron ficheros tipo 'NN-titulo.Rmd' en la raíz.")
}

for (f in rmds) {
  procesar_rmd(f,
               ruta_salida = file.path(build_dir, f),
               verbose = FALSE)
}

cat(sprintf("[3/4] Procesados %d capítulos.\n", length(rmds)))

# --- 5. Renderizar ----------------------------------------------------

cat(sprintf("[4/4] Renderizando con %s ...\n",
            if (is.null(formato)) "formato del _output.yml" else formato))

# Antes de renderizar, cambiamos el directorio de trabajo a _build/ para
# que knitr resuelva los paths relativos de los chunks (lecturas de
# Excel, CSVs, etc.) desde ahí.
#
# Guardamos el directorio del proyecto en `.proyecto_dir` (con punto
# inicial). Los chunks del libro suelen ejecutar `rm(list = ls())`, que
# NO afecta a nombres ocultos (los que empiezan por punto). Así nuestra
# variable sobrevive al render aunque algún chunk limpie el global env.

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_dir_abs <- normalizePath(output_dir)
.proyecto_dir <- normalizePath(getwd())

# Encapsulamos en función para que `on.exit` funcione correctamente
# (a diferencia de cuando se usa a nivel top-level con `source()`).
render_libro <- function(build_dir, formato, output_dir_abs) {
  on.exit(setwd(.proyecto_dir), add = TRUE)
  setwd(build_dir)
  bookdown::render_book(
    input         = ".",
    output_format = formato,
    output_dir    = output_dir_abs
  )
}

render_libro(build_dir, formato, output_dir_abs)

cat(sprintf("\n[OK] Libro renderizado en '%s/'\n", output_dir))
