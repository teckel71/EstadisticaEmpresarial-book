# Estadística Empresarial

Manual teórico-práctico de la asignatura **Estadística Empresarial** del
**Grado en Administración y Dirección de Empresas** de la Universidad de
Castilla-La Mancha (Facultad de Derecho y Ciencias Sociales de Ciudad Real).

El libro está construido con [R Markdown](https://rmarkdown.rstudio.com/) y
[**bookdown**](https://bookdown.org/yihui/bookdown/), y publicado en GitHub
Pages.

## Estructura

Cada capítulo se corresponde con un apartado del temario oficial recogido en
la guía docente:

- `index.Rmd` — Portada, metadatos y cabecera del libro.
- `00-prefacio.Rmd` — Prefacio.
- `01-introduccion.Rmd` — Introducción.
- `02-estadistica_economica_empresarial.Rmd` — Tema 1, apartado 1.1.
- `03-variable_unidimensional.Rmd` — Tema 1, apartado 1.2.
- `04-variable_bidimensional.Rmd` — Tema 2, apartado 2.1.
- `05-regresion_correlacion.Rmd` — Tema 2, apartado 2.2.
- `06-numeros_indice.Rmd` — Tema 3.
- `07-introduccion_probabilidad.Rmd` — Tema 4, apartado 4.1.
- `08-variables_aleatorias.Rmd` — Tema 4, apartado 4.2.
- `09-caracteristicas_variables_aleatorias.Rmd` — Tema 4, apartado 4.3.
- `10-modelos_discretos.Rmd` — Tema 5, apartado 5.1.
- `11-modelos_continuos.Rmd` — Tema 5, apartado 5.2.
- `12-convergencia_teoremas_limite.Rmd` — Tema 5, apartado 5.3.
- `13-references.Rmd` — Bibliografía.

## Compilación

Desde la consola de R, en el directorio del proyecto:

```r
bookdown::render_book("index.Rmd", "bookdown::bs4_book")
```

O bien, desde RStudio: pestaña **Build > Build Book**.

El resultado se genera en la carpeta `docs/`, que es la que utiliza GitHub
Pages para servir el libro.
