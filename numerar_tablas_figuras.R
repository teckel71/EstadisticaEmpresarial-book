#!/usr/bin/env Rscript
# =====================================================================
# numerar_tablas_figuras.R  (v2)
# ---------------------------------------------------------------------
# Postprocesador de ficheros .Rmd para numerar tablas y figuras
# manualmente y de forma consistente.
#
# Reglas resumidas:
#   - Las TABLAS se numeran con "**Tabla X.Y.** <descripción opcional>"
#     centrado debajo, donde X = nº de capítulo, Y = orden en el cap.
#   - Las FIGURAS con "**Figura X.Y.** <descripción opcional>" igual.
#   - Soporta:
#       * Llamadas estáticas a kable / kbl / gt / gt_plt_summary /
#         flextable / datatable (con o sin condicional html/docx/pdf).
#       * Imágenes markdown ![alt](ruta) (excluyendo iconos .hicon).
#       * Llamadas a ggplot / vis_miss / ggpairs / grid.arrange /
#         patchwork::wrap_plots / fviz_* / plot / barplot / hist.
#       * Tablas markdown manuales (líneas con | + separador |---|).
#       * Chunks mixtos: si tienen p.ej. una tabla y dos figuras, emite
#         los tres bloques de numeración en orden tras el chunk.
#   - NO numera (deja que bookdown haga la numeración automática):
#       * Chunks que contienen un bucle (for/while/lapply/sapply/map*).
#         Además, intenta quitar los `caption = ...` de esos bucles
#         para que bookdown tampoco genere doble numeración rara.
#   - NO toca:
#       * Iconos .hicon en titulares.
#       * Chunks con eval = FALSE (código pedagógico).
#       * Definiciones de función dentro de chunks.
#
# Uso:
#   Rscript numerar_tablas_figuras.R entrada.Rmd [salida.Rmd] [num_cap]
#   procesar_rmd("foo.Rmd")                                       # desde R
# =====================================================================

procesar_rmd <- function(ruta_entrada,
                        ruta_salida = NULL,
                        num_capitulo = NULL,
                        incluir_caption_tabla = TRUE,
                        incluir_alt_figura    = TRUE,
                        envolver_alt_simple   = TRUE,
                        verbose = TRUE) {

  # ===================================================================
  # 1. Configuración y lectura
  # ===================================================================
  if (is.null(num_capitulo)) {
    nombre <- basename(ruta_entrada)
    m <- regmatches(nombre, regexpr("^[0-9]+", nombre))
    if (length(m) == 0) {
      stop("No se pudo inferir el número de capítulo del nombre del ",
           "fichero. Pase num_capitulo explícitamente.")
    }
    num_capitulo <- as.integer(m)
  }
  if (is.null(ruta_salida)) {
    base <- tools::file_path_sans_ext(ruta_entrada)
    ext  <- tools::file_ext(ruta_entrada)
    ruta_salida <- paste0(base, "_numerado.", ext)
  }

  raw <- readChar(ruta_entrada, file.info(ruta_entrada)$size, useBytes = TRUE)
  eol_original <- if (grepl("\r\n", raw, fixed = TRUE)) "\r\n" else "\n"
  lineas <- readLines(ruta_entrada, warn = FALSE, encoding = "UTF-8")
  lineas <- gsub("\r$", "", lineas)

  # Pre-procesado: unir imágenes markdown que vienen partidas en varias
  # líneas. Patrón: línea que contiene `![[...` o `![...` sin cerrar el
  # paréntesis final. Las juntamos con las siguientes hasta encontrar `)`.
  unir_imagenes_multilinea <- function(lns) {
    if (length(lns) == 0L) return(lns)
    resultado <- character(0)
    i <- 1L
    while (i <= length(lns)) {
      linea <- lns[i]
      tiene_inicio <- grepl("!\\[", linea, perl = TRUE)
      if (tiene_inicio) {
        # ¿Tiene `)` después del `![`? Si sí, está completa.
        pos_corchete <- regexpr("!\\[", linea, perl = TRUE)
        substr_tras <- substr(linea, as.integer(pos_corchete),
                              nchar(linea))
        if (!grepl("\\)", substr_tras, perl = TRUE) && i < length(lns)) {
          # Continúa: ir uniendo líneas hasta encontrar `)`
          unida <- linea
          j <- i + 1L
          while (j <= length(lns) && !grepl("\\)", unida, perl = TRUE)) {
            # Une con espacio (no \n) para que las regex funcionen
            unida <- paste(unida, lns[j])
            j <- j + 1L
          }
          # Solo unir si REALMENTE encontramos el `)`
          if (grepl("\\)", unida, perl = TRUE)) {
            resultado <- c(resultado, unida)
            i <- j
            next
          }
        }
      }
      resultado <- c(resultado, linea)
      i <- i + 1L
    }
    resultado
  }
  lineas <- unir_imagenes_multilinea(lineas)

  # ===================================================================
  # 2. Patrones (constantes)
  # ===================================================================
  re_func_tabla <- paste(
    "\\bkable\\s*\\(",
    "\\bkbl\\s*\\(",
    "\\bgt_plt_summary\\s*\\(",
    "\\bflextable\\s*\\(",
    "\\bdatatable\\s*\\(",
    "\\bgt\\s*\\(",
    "\\bpander\\s*\\(",
    "\\bpander_return\\s*\\(",
    "\\bpandoc\\.table\\s*\\(",
    sep = "|")
  re_func_figura <- paste(
    "\\bggplot\\s*\\(",
    "\\bvis_miss\\s*\\(",
    "\\bggpairs\\s*\\(",
    "\\bggarrange\\s*\\(",
    "\\bggMarginal\\s*\\(",
    "\\bgrid\\.arrange\\s*\\(",
    "\\bgrid\\.draw\\s*\\(",
    "\\bplot_annotation\\s*\\(",
    "\\bwrap_plots\\s*\\(",
    "\\bcreate_patchwork\\s*\\(",   # función auxiliar del usuario
    "\\bfviz_[a-z_]+\\s*\\(",
    "\\bautoplot\\s*\\(",
    "(?<![._a-zA-Z0-9])plot\\s*\\(",
    "\\bbarplot\\s*\\(",
    "\\bboxplot\\s*\\(",
    "\\bhist\\s*\\(",
    "\\bpairs\\s*\\(",
    "\\bmosaicplot\\s*\\(",
    "\\bmosaic\\s*\\(",
    "\\bcorrplot\\s*\\(",
    "\\bassoc\\s*\\(",
    sep = "|")
  re_func_bucle <- paste(
    "\\bfor\\s*\\(",
    "\\bwhile\\s*\\(",
    "\\blapply\\s*\\(",
    "\\bsapply\\s*\\(",
    "\\bvapply\\s*\\(",
    "\\bmapply\\s*\\(",
    "\\bmap[a-z_]*\\s*\\(",
    "\\bwalk[a-z_]*\\s*\\(",
    sep = "|")

  re_chunk_inicio <- "^```\\s*\\{r[^}]*\\}\\s*$"
  re_chunk_fin    <- "^```\\s*$"
  re_heading      <- "^#"

  re_cap_lit      <- 'caption\\s*=\\s*"([^"]*)"\\s*,?\\s*'

  re_tabla_md_sep <- "^\\|[[:space:]\\-:|]+\\|[[:space:]]*$"
  re_tabla_md_row <- "^\\|.*\\|[[:space:]]*$"

  # ===================================================================
  # 3. Helpers
  # ===================================================================
  bloque_numeracion <- function(tipo, n_cap, n, descripcion = NULL) {
    etiqueta <- if (tipo == "tabla") "Tabla" else "Figura"
    cabecera <- paste0("**", etiqueta, " ", n_cap, ".", n, "**")
    if (!is.null(descripcion) && nzchar(trimws(descripcion))) {
      cabecera <- paste0("**", etiqueta, " ", n_cap, ".", n, ".** ",
                         trimws(descripcion))
    }
    c("",
      "::: {style=\"text-align: center;\"}",
      cabecera,
      ":::",
      "")
  }

  es_chunk_inicio <- function(linea) grepl(re_chunk_inicio, linea, perl = TRUE)
  es_chunk_fin    <- function(linea) grepl(re_chunk_fin,    linea, perl = TRUE)
  es_eval_false   <- function(h) grepl("eval\\s*=\\s*F(ALSE)?\\b", h, perl = TRUE)
  es_titular      <- function(l) grepl(re_heading, l, perl = TRUE)

  # -------------------------------------------------------------------
  # 3.1. Analizar contenido de un chunk
  # -------------------------------------------------------------------
  # Devuelve list(secuencia, captions, posiciones, tiene_bucle,
  #               contenedores, diferidos_tentativos).
  # Recibe contenedores y diferidos tentativos iniciales (estado global
  # heredado de chunks previos en el mismo documento). Esto permite
  # capturar patrones como `var <- f(...)` en un chunk y `var[[N]]` en
  # otro posterior.
  analizar_chunk <- function(buffer,
                             contenedores_iniciales = list(),
                             diferidos_tentativos_iniciales = list()) {
    en_def         <- FALSE
    cuerpo_abierto <- FALSE
    nivel_llaves   <- 0L
    tiene_bucle    <- FALSE

    seq_tipo      <- character(0)
    seq_cap       <- character(0)
    seq_posicion  <- integer(0)

    diferidos    <- list()        # tipo conocido (kable, ggplot, etc.)
    diferidos_tentativos <- diferidos_tentativos_iniciales
    contenedores <- contenedores_iniciales

    en_kable <- FALSE

    # Asignación multi-línea: si vemos `var <- expr_que_continua_en_lineas
    # siguientes`, marcamos `var` como receptora y asociamos las
    # subsiguientes llamadas a kable/ggplot a `var` (en lugar de
    # contarlas como elementos directos del chunk).
    asig_multi_var   <- NA_character_
    asig_multi_paren <- 0L
    asig_multi_primera_func <- NA_character_  # primera función llamada
                                              # en la asignación multi

    # Funciones que NO marcan tentativo (asignación trivial de
    # estructura, no resultado de "presentación" de modelo o similar).
    lista_negra_funcs <- c(
      "read_excel", "read.csv", "read_csv", "read_tsv", "read.delim",
      "read.table", "readRDS", "read_rds", "load",
      "data.frame", "tibble", "as.data.frame", "as_tibble",
      "list", "c", "vector", "numeric", "character", "logical",
      "factor", "as.factor", "as.numeric", "as.character",
      "as.integer", "as.logical", "as.Date",
      "paste", "paste0", "sprintf", "format", "formatC",
      "seq", "seq_len", "seq_along", "rep",
      "length", "nrow", "ncol", "dim",
      "matrix", "array",
      "max", "min", "sum", "mean", "median", "sd", "var",
      "round", "floor", "ceiling",
      "names", "colnames", "rownames",
      "subset", "filter", "select", "mutate", "summarize", "summarise",
      "group_by", "arrange",
      "attr", "attributes",
      "xtabs", "table"
    )
    es_funcion_lista_negra <- function(fname) {
      if (is.na(fname) || !nzchar(fname)) return(FALSE)
      fname %in% lista_negra_funcs
    }  # nivel de paréntesis sin cerrar

    re_terminal      <- "\\b(?:create_patchwork|wrap_plots)\\s*\\("
    re_asig_inicio   <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*(<-|=)\\s*"
    re_asig_lista    <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\[\\[[^\\]]+\\]\\]\\s*(<-|=)\\s*"
    re_asig_lista_rhs <- paste0(
      "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\[\\[[^\\]]+\\]\\]\\s*",
      "(?:<-|=)\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*$")
    re_solo_nombre   <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*$"
    re_solo_lista    <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\[\\[[^\\]]+\\]\\]\\s*$"
    re_solo_dollar   <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\$\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*$"
    re_print_var     <- "\\bprint\\s*\\(\\s*([A-Za-z_.][A-Za-z_.0-9]*)"
    # Patchwork inline: línea con SOLO nombres unidos por +, /, |
    # (sin paréntesis con argumentos, sin números, sin strings).
    # Acepta paréntesis de agrupación: (g1 | g2) / (g3 | g4)
    re_patchwork_inline <- "^[\\s()A-Za-z_.0-9+/|]+$"
    # Una línea "continúa" si termina con %>%, +, |, ,, o paréntesis abierto
    re_continua      <- "(?:%>%|\\+|\\||,)\\s*$"

    # Helper: cuenta caracteres específicos en una cadena
    contar_ch <- function(txt, ch) {
      sum(strsplit(txt, "", fixed = TRUE)[[1]] == ch)
    }

    for (i in seq_along(buffer)) {
      linea <- buffer[i]

      # ¿Abre una nueva definición de función?
      if (grepl("<-\\s*function\\s*\\(", linea, perl = TRUE)) {
        en_def         <- TRUE
        cuerpo_abierto <- FALSE
        nivel_llaves   <- 0L
        en_kable       <- FALSE
        asig_multi_var   <- NA_character_
        asig_multi_paren <- 0L
      }

      if (en_def) {
        n_abre   <- contar_ch(linea, "{")
        n_cierra <- contar_ch(linea, "}")
        if (!cuerpo_abierto) {
          if (n_abre > 0L) {
            cuerpo_abierto <- TRUE
            nivel_llaves   <- n_abre - n_cierra
            if (nivel_llaves <= 0L) {
              en_def         <- FALSE
              cuerpo_abierto <- FALSE
              nivel_llaves   <- 0L
            }
          }
        } else {
          nivel_llaves <- nivel_llaves + n_abre - n_cierra
          if (nivel_llaves <= 0L) {
            en_def         <- FALSE
            cuerpo_abierto <- FALSE
            nivel_llaves   <- 0L
          }
        }
        next
      }

      if (grepl(re_func_bucle, linea, perl = TRUE)) tiene_bucle <- TRUE

      tiene_terminal <- grepl(re_terminal, linea, perl = TRUE)

      # --- Verificar si seguimos en asignación multi-línea ---
      if (!is.na(asig_multi_var)) {
        # Asociar kable/ggplot/terminal en esta línea a asig_multi_var
        if (grepl(re_func_tabla, linea, perl = TRUE)) {
          diferidos[[asig_multi_var]] <- "tabla"
          en_kable <- TRUE
        }
        if (grepl(re_func_figura, linea, perl = TRUE) ||
            grepl(re_terminal, linea, perl = TRUE)) {
          diferidos[[asig_multi_var]] <- "figura"
        }
        # Actualizar nivel de paréntesis
        asig_multi_paren <- asig_multi_paren +
                            contar_ch(linea, "(") - contar_ch(linea, ")")
        # ¿Termina la asignación?
        if (asig_multi_paren <= 0L &&
            !grepl(re_continua, linea, perl = TRUE)) {
          # Si no se detectó ningún tipo conocido en la cadena
          # multi-línea Y la primera función llamada no está en la
          # lista negra de funciones triviales, marcamos como
          # tentativo (la RHS era una llamada a función desconocida).
          if (is.null(diferidos[[asig_multi_var]]) &&
              !es_funcion_lista_negra(asig_multi_primera_func)) {
            diferidos_tentativos[[asig_multi_var]] <- "tabla"
          }
          asig_multi_var   <- NA_character_
          asig_multi_paren <- 0L
          asig_multi_primera_func <- NA_character_
        }
        next
      }

      # 1) Línea con SOLO un nombre de variable (impresión de objeto)
      m_solo <- regexec(re_solo_nombre, linea, perl = TRUE)
      r_solo <- regmatches(linea, m_solo)[[1]]
      if (length(r_solo) >= 2 && !tiene_terminal) {
        var_name <- r_solo[2]
        if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          diferidos[[var_name]] <- NULL
        }
        next
      }

      # 2) Línea con SOLO var[[expr]] (impresión de elemento de lista)
      m_solo_l <- regexec(re_solo_lista, linea, perl = TRUE)
      r_solo_l <- regmatches(linea, m_solo_l)[[1]]
      if (length(r_solo_l) >= 2 && !tiene_terminal) {
        var_name <- r_solo_l[2]
        if (!is.null(contenedores[[var_name]])) {
          seq_tipo     <- c(seq_tipo, contenedores[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          next
        } else if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos[[var_name]]
          next
        } else if (!is.null(diferidos_tentativos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos_tentativos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos_tentativos[[var_name]]
          next
        }
        next
      }

      # 2b) Línea con SOLO var$nombre (impresión de elemento de lista)
      m_solo_d <- regexec(re_solo_dollar, linea, perl = TRUE)
      r_solo_d <- regmatches(linea, m_solo_d)[[1]]
      if (length(r_solo_d) >= 3 && !tiene_terminal) {
        var_name <- r_solo_d[2]
        if (!is.null(contenedores[[var_name]])) {
          seq_tipo     <- c(seq_tipo, contenedores[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
        } else if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos[[var_name]]
        } else if (!is.null(diferidos_tentativos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos_tentativos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos_tentativos[[var_name]]
        }
        next
      }

      # 2c) Patrón especial: `generar_solucion(ARG)` solita.
      #     Esta función custom del libro produce 1 figura como side
      #     effect (un mosaico) y crea una lista global `solucion_<ARG>`
      #     con tablas accesibles via `$Informacion` y `$Coeficientes`.
      #     Registramos la figura aquí y dejamos un contenedor para los
      #     usos posteriores de `solucion_<ARG>$X`.
      re_generar_sol <- paste0(
        "^\\s*generar_solucion\\s*\\(\\s*",
        "([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\)\\s*$")
      m_gs <- regexec(re_generar_sol, linea, perl = TRUE)
      r_gs <- regmatches(linea, m_gs)[[1]]
      if (length(r_gs) >= 2) {
        arg_name <- r_gs[2]
        seq_tipo     <- c(seq_tipo, "figura")
        seq_cap      <- c(seq_cap,  NA_character_)
        seq_posicion <- c(seq_posicion, i)
        contenedores[[paste0("solucion_", arg_name)]] <- "tabla"
        next
      }

      # 2d) Línea con patchwork inline tipo `g1 / g2`, `g1 + g2 + g3`
      #     o `(g1 | g2) / (g3 | g4)`. Solo se detecta si hay al menos
      #     un operador +,/,| y al menos un nombre que sea figura
      #     diferida.
      if (grepl(re_patchwork_inline, linea, perl = TRUE) &&
          grepl("[+/|]", linea, perl = TRUE) &&
          !grepl("\\bfunction\\b", linea, perl = TRUE)) {
        # Extraer los nombres de las variables
        nombres <- regmatches(linea,
                              gregexpr("[A-Za-z_.][A-Za-z_.0-9]*",
                                       linea, perl = TRUE))[[1]]
        # ¿Alguno es figura diferida?
        es_patchwork <- any(sapply(nombres, function(n) {
          !is.null(diferidos[[n]]) && diferidos[[n]] == "figura"
        }))
        if (es_patchwork) {
          seq_tipo     <- c(seq_tipo, "figura")
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          # Consumir las figuras diferidas usadas
          for (n in nombres) {
            if (!is.null(diferidos[[n]]) && diferidos[[n]] == "figura") {
              diferidos[[n]] <- NULL
            }
          }
          next
        }
      }

      # 3) print(var[[...]]) o print(var) que consume diferido/contenedor
      m_pr <- regexec(re_print_var, linea, perl = TRUE)
      r_pr <- regmatches(linea, m_pr)[[1]]
      if (length(r_pr) >= 2) {
        var_name <- r_pr[2]
        consumido <- FALSE
        if (!is.null(contenedores[[var_name]])) {
          seq_tipo     <- c(seq_tipo, contenedores[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          consumido <- TRUE
        } else if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          diferidos[[var_name]] <- NULL
          consumido <- TRUE
        }
        if (consumido) next
      }

      # 4a) Asignación a elemento de lista con RHS = otro nombre simple:
      #     `lista[[idx]] <- otra_var`. Si otra_var es diferida, el
      #     contenedor hereda su tipo.
      m_arl <- regexec(re_asig_lista_rhs, linea, perl = TRUE)
      r_arl <- regmatches(linea, m_arl)[[1]]
      if (length(r_arl) >= 3) {
        lhs <- r_arl[2]; rhs <- r_arl[3]
        if (!is.null(diferidos[[rhs]])) {
          contenedores[[lhs]] <- diferidos[[rhs]]
        } else if (!is.null(contenedores[[rhs]])) {
          contenedores[[lhs]] <- contenedores[[rhs]]
        }
        next
      }

      # 4b) Asignación a elemento de lista con expresión inline:
      m_asig_l <- regexec(re_asig_lista, linea, perl = TRUE)
      r_asig_l <- regmatches(linea, m_asig_l)[[1]]
      if (length(r_asig_l) >= 2) {
        var_name <- r_asig_l[2]
        if (grepl(re_func_tabla, linea, perl = TRUE)) {
          contenedores[[var_name]] <- "tabla"
          en_kable <- TRUE
        } else if (grepl(re_func_figura, linea, perl = TRUE) ||
                   grepl(re_terminal, linea, perl = TRUE)) {
          contenedores[[var_name]] <- "figura"
        } else if (grepl(re_continua, linea, perl = TRUE) ||
                   (contar_ch(linea, "(") - contar_ch(linea, ")")) > 0L) {
          # asignación a lista multi-línea (el kable vendrá más abajo)
          asig_multi_var <- var_name
          asig_multi_paren <- contar_ch(linea, "(") - contar_ch(linea, ")")
          # marcar como contenedor (tipo se decidirá al detectar kable/ggplot)
        }
        next
      }

      # 5) Asignación normal: var <- algo
      m_asig <- regexec(re_asig_inicio, linea, perl = TRUE)
      r_asig <- regmatches(linea, m_asig)[[1]]
      es_asignacion <- length(r_asig) >= 2
      var_name <- if (es_asignacion) r_asig[2] else NA_character_

      hay_tabla  <- grepl(re_func_tabla,  linea, perl = TRUE)
      hay_figura <- grepl(re_func_figura, linea, perl = TRUE)

      if (tiene_terminal && es_asignacion) {
        diferidos[[var_name]] <- "figura"
      } else if (tiene_terminal) {
        seq_tipo     <- c(seq_tipo, "figura")
        seq_cap      <- c(seq_cap,  NA_character_)
        seq_posicion <- c(seq_posicion, i)
      } else if (es_asignacion) {
        if (hay_tabla) {
          diferidos[[var_name]] <- "tabla"
          en_kable <- TRUE
        }
        if (hay_figura) {
          diferidos[[var_name]] <- "figura"
        }
        # ¿La asignación continúa en líneas siguientes?
        if (!hay_tabla && !hay_figura) {
          n_p_abre  <- contar_ch(linea, "(")
          n_p_cierr <- contar_ch(linea, ")")
          # Extraer la primera función llamada en el RHS
          primera_func <- NA_character_
          m_pf <- regexec(
            "(?:<-|=)\\s*([A-Za-z_.][A-Za-z_.0-9]*(?:::[A-Za-z_.][A-Za-z_.0-9]*)?)\\s*\\(",
            linea, perl = TRUE)
          r_pf <- regmatches(linea, m_pf)[[1]]
          if (length(r_pf) >= 2) {
            primera_func <- sub("^.*::", "", r_pf[2])  # quitar namespace
          }
          if (grepl(re_continua, linea, perl = TRUE) ||
              n_p_abre > n_p_cierr) {
            asig_multi_var   <- var_name
            asig_multi_paren <- n_p_abre - n_p_cierr
            asig_multi_primera_func <- primera_func
          } else if (n_p_abre > 0L &&
                     !es_funcion_lista_negra(primera_func)) {
            # asignación a llamada de función desconocida (sin tabla
            # ni figura conocidas, ni en lista negra). Marcamos como
            # tentativo: solo contará si después vemos var[[expr]] o
            # var$nombre.
            diferidos_tentativos[[var_name]] <- "tabla"
          }
        }
      } else {
        # Filtrar pander("texto literal") — no es tabla, es solo título.
        es_pander_texto <- grepl(
          "\\bpander(?:_return)?\\s*\\(\\s*\"",
          linea, perl = TRUE)
        m_tab <- gregexpr(re_func_tabla, linea, perl = TRUE)[[1]]
        if (m_tab[1] != -1 && !es_pander_texto) {
          for (k in seq_along(m_tab)) {
            seq_tipo     <- c(seq_tipo, "tabla")
            seq_cap      <- c(seq_cap,  NA_character_)
            seq_posicion <- c(seq_posicion, i)
          }
          en_kable <- TRUE
        }
        m_fig <- gregexpr(re_func_figura, linea, perl = TRUE)[[1]]
        if (m_fig[1] != -1) {
          for (k in seq_along(m_fig)) {
            seq_tipo     <- c(seq_tipo, "figura")
            seq_cap      <- c(seq_cap,  NA_character_)
            seq_posicion <- c(seq_posicion, i)
          }
        }
      }

      if (en_kable && grepl(re_cap_lit, linea, perl = TRUE)) {
        m_c <- regmatches(linea, regexec(re_cap_lit, linea, perl = TRUE))[[1]]
        if (length(m_c) >= 2) {
          idx <- tail(which(seq_tipo == "tabla" & is.na(seq_cap)), 1L)
          if (length(idx) > 0L) seq_cap[idx] <- m_c[2]
        }
      }
    }

    # Si hay terminal (patchwork) en el chunk, los bucles dejan de
    # descalificar el chunk.
    if (length(grep(re_terminal, paste(buffer, collapse = "\n"),
                    perl = TRUE)) > 0) {
      tiene_bucle <- FALSE
    }
    # Si hubo detección de elementos usando contenedores, el `for` solo
    # iteraba sobre la lista para imprimirla.
    if (length(seq_tipo) > 0L && length(contenedores) > 0L) {
      tiene_bucle <- FALSE
    }

    # Colapsar duplicados consecutivos del mismo tipo con MISMO caption
    if (length(seq_tipo) >= 2L) {
      mantener <- rep(TRUE, length(seq_tipo))
      for (i in 2:length(seq_tipo)) {
        if (seq_tipo[i] == seq_tipo[i - 1L]) {
          ci   <- seq_cap[i]
          cim1 <- seq_cap[i - 1L]
          if (!is.na(ci) && !is.na(cim1) && identical(ci, cim1)) {
            mantener[i] <- FALSE
          }
        }
      }
      seq_tipo     <- seq_tipo[mantener]
      seq_cap      <- seq_cap[mantener]
      seq_posicion <- seq_posicion[mantener]
    }

    list(
      secuencia            = seq_tipo,
      captions             = seq_cap,
      posiciones           = seq_posicion,
      tiene_bucle          = tiene_bucle,
      contenedores         = contenedores,
      diferidos_tentativos = diferidos_tentativos
    )
  }

  # -------------------------------------------------------------------
  # 3.2. Quitar TODOS los caption (literales y paste) de las llamadas
  #      kable/kbl del chunk para evitar la doble autonumeración bookdown
  # -------------------------------------------------------------------
  quitar_todos_captions <- function(buffer) {
    re_kable_open <- "\\b(?:kable|kbl)\\s*\\("

    # Ya no saltamos definiciones de función: queremos limpiar también
    # los captions de los kables que viven dentro del cuerpo de funciones
    # custom (presenta_modelo, generar_solucion, predict_from_excel_*).
    # Sin esa limpieza, bookdown autonumeraría las tablas cuando se llame
    # a la función, produciendo "Table X.Y:" antes de nuestra etiqueta.
    dentro_kable <- FALSE
    nivel_paren  <- 0L

    nuevo <- buffer
    for (i in seq_along(nuevo)) {
      linea <- nuevo[i]
      if (is.na(linea)) next

      if (!dentro_kable && grepl(re_kable_open, linea, perl = TRUE)) {
        dentro_kable <- TRUE
        nivel_paren  <- 0L
      }

      if (dentro_kable) {
        # 1) caption literal
        if (grepl(re_cap_lit, linea, perl = TRUE)) {
          linea <- sub(re_cap_lit, "", linea, perl = TRUE)
        }
        # 2) caption = paste(...) o paste0(...) con balanceo
        m_paste <- regexpr("caption\\s*=\\s*paste0?\\s*\\(",
                          linea, perl = TRUE)
        if (m_paste > 0) {
          ini  <- as.integer(m_paste)
          pos_paren <- regexpr("\\(", substr(linea, ini, nchar(linea)),
                              perl = TRUE)
          if (pos_paren > 0) {
            par_ini_abs <- ini + as.integer(pos_paren) - 1L
            nivel <- 1L
            j <- par_ini_abs + 1L
            ll <- linea
            line_idx <- i
            cierre_col <- NA_integer_
            while (TRUE) {
              if (j > nchar(ll)) {
                line_idx <- line_idx + 1L
                if (line_idx > length(nuevo)) break
                ll <- nuevo[line_idx]
                if (is.na(ll)) ll <- ""
                j <- 1L
                if (nchar(ll) == 0L) next
              }
              ch <- substr(ll, j, j)
              if (ch == "(") nivel <- nivel + 1L
              else if (ch == ")") {
                nivel <- nivel - 1L
                if (nivel == 0L) {
                  cierre_col <- j
                  break
                }
              }
              j <- j + 1L
            }
            if (!is.na(cierre_col)) {
              if (line_idx == i) {
                resto <- substr(linea, cierre_col + 1L, nchar(linea))
                tm <- regexpr("^\\s*,\\s*", resto, perl = TRUE)
                if (tm > 0) {
                  cierre_col <- cierre_col + attr(tm, "match.length")
                }
                linea <- paste0(substr(linea, 1L, ini - 1L),
                                substr(linea, cierre_col + 1L, nchar(linea)))
              } else {
                nuevo[i] <- substr(linea, 1L, ini - 1L)
                if (line_idx > i + 1L) {
                  nuevo[(i + 1L):(line_idx - 1L)] <- NA_character_
                }
                ult <- nuevo[line_idx]
                if (is.na(ult)) ult <- ""
                resto <- substr(ult, cierre_col + 1L, nchar(ult))
                tm <- regexpr("^\\s*,\\s*", resto, perl = TRUE)
                if (tm > 0) {
                  cierre_col <- cierre_col + attr(tm, "match.length")
                }
                nuevo[line_idx] <- substr(ult, cierre_col + 1L, nchar(ult))
                linea <- nuevo[i]
              }
            }
          }
        }

        abiertos <- nchar(gsub("[^(]", "", linea))
        cerrados <- nchar(gsub("[^)]", "", linea))
        nivel_paren <- nivel_paren + abiertos - cerrados
        nuevo[i] <- linea
        if (nivel_paren <= 0L) {
          dentro_kable <- FALSE
          nivel_paren  <- 0L
        }
      }
    }

    nuevo <- nuevo[!is.na(nuevo)]
    nuevo
  }

  # -------------------------------------------------------------------
  # 3.3. Imágenes markdown
  # -------------------------------------------------------------------
  procesar_linea_imagen <- function(linea) {
    if (es_titular(linea)) return(list(linea = linea, contado = FALSE))
    if (grepl("\\.hicon", linea, perl = TRUE)) {
      return(list(linea = linea, contado = FALSE))
    }
    re_pandoc <- "!\\[\\[([^\\]]*)\\]\\{([^}]*)\\}\\]\\(([^)]+)\\)"
    if (grepl(re_pandoc, linea, perl = TRUE)) {
      m <- regmatches(linea, regexec(re_pandoc, linea, perl = TRUE))[[1]]
      alt <- if (length(m) >= 3) paste0("[", m[2], "]{", m[3], "}") else ""
      linea_modif <- sub(re_pandoc, "![](\\3)", linea, perl = TRUE)
      return(list(linea = linea_modif, contado = TRUE, alt = alt))
    }
    re_simple <- "!\\[([^\\]]*)\\]\\(([^)]+)\\)"
    if (grepl(re_simple, linea, perl = TRUE)) {
      m <- regmatches(linea, regexec(re_simple, linea, perl = TRUE))[[1]]
      alt <- if (length(m) >= 2) m[2] else ""
      if (envolver_alt_simple && nzchar(alt) &&
          !grepl("\\]\\{", alt, perl = TRUE)) {
        alt <- paste0("[", alt, "]{.smallcaps}")
      }
      linea_modif <- sub(re_simple, "![](\\2)", linea, perl = TRUE)
      return(list(linea = linea_modif, contado = TRUE, alt = alt))
    }
    list(linea = linea, contado = FALSE)
  }

  # -------------------------------------------------------------------
  # 3.4. Detectar tabla markdown manual
  # -------------------------------------------------------------------
  detectar_tabla_md <- function(lineas, i) {
    if (i > length(lineas)) return(list(es_tabla = FALSE))
    if (!grepl(re_tabla_md_row, lineas[i], perl = TRUE)) {
      return(list(es_tabla = FALSE))
    }
    j <- i
    tiene_sep <- FALSE
    while (j <= length(lineas) &&
           grepl(re_tabla_md_row, lineas[j], perl = TRUE)) {
      if (grepl(re_tabla_md_sep, lineas[j], perl = TRUE)) tiene_sep <- TRUE
      j <- j + 1L
    }
    fin <- j - 1L
    if (!tiene_sep) return(list(es_tabla = FALSE))

    caption <- NA_character_
    k <- fin + 1L
    while (k <= length(lineas) && grepl("^\\s*$", lineas[k])) k <- k + 1L
    if (k <= length(lineas)) {
      m_post <- regexec("^\\s*:\\s*(.+?)\\.?\\s*$", lineas[k], perl = TRUE)
      r <- regmatches(lineas[k], m_post)[[1]]
      if (length(r) >= 2) {
        caption <- r[2]
        fin <- k
      } else {
        m_post2 <- regexec("^\\s*Table:\\s*(.+?)\\s*$",
                          lineas[k], perl = TRUE)
        r2 <- regmatches(lineas[k], m_post2)[[1]]
        if (length(r2) >= 2) {
          caption <- r2[2]
          fin <- k
        }
      }
    }
    list(es_tabla = TRUE, fin = fin, caption = caption)
  }

  # -------------------------------------------------------------------
  # 3.5. Dividir un chunk mixto en sub-chunks
  # -------------------------------------------------------------------
  # Para chunks con varios elementos (p.ej. una tabla + una figura),
  # buscamos un punto de corte (línea en blanco) inmediatamente después
  # de cada elemento. Si lo encontramos, dividimos el chunk en
  # sub-chunks de forma que cada elemento quede en su propio sub-chunk
  # y su etiqueta pueda aparecer pegada justo debajo.
  #
  # Devuelve NULL si no se puede dividir (no hay puntos de corte
  # suficientes); en ese caso el bucle principal cae al modo "todas las
  # etiquetas al final".
  #
  # Si devuelve una lista, es una secuencia de bloques de tipo:
  #   list(tipo = "sub_chunk_elem", buffer = ..., elem_tipo, elem_cap)
  #   list(tipo = "sub_chunk_resto", buffer = ...)   # sin etiqueta
  dividir_chunk_mixto <- function(buffer, posiciones, secuencia, captions) {
    n_elem <- length(posiciones)
    if (n_elem == 0L) return(NULL)

    # Para cada elemento k determinamos:
    #   fines[k]   = última línea del sub-chunk k
    #   inicios[k] = primera línea del sub-chunk k
    # Heurísticas para el corte:
    #   - Si hay línea en blanco entre pos_k y pos_{k+1}: corte ahí
    #     y se salta la línea blanca.
    #   - Si pos_{k+1} == pos_k + 1 (elementos en líneas adyacentes):
    #     corte forzado, cada sub-chunk = una línea con su elemento.
    #   - Si hay código intermedio sin línea en blanco: NO se puede
    #     dividir limpiamente (NULL).
    fines    <- integer(n_elem)
    inicios  <- integer(n_elem)
    inicios[1L] <- 1L
    resto_inicio <- length(buffer) + 1L  # por defecto, sin resto

    for (k in seq_len(n_elem)) {
      pos_k <- posiciones[k]

      if (k < n_elem) {
        siguiente <- posiciones[k + 1L]
        corte_blanco <- 0L
        if ((pos_k + 1L) <= (siguiente - 1L)) {
          for (j in (pos_k + 1L):(siguiente - 1L)) {
            if (grepl("^\\s*$", buffer[j])) {
              corte_blanco <- j
              break
            }
          }
        }
        if (corte_blanco > 0L) {
          fines[k]            <- corte_blanco - 1L
          inicios[k + 1L]     <- corte_blanco + 1L
        } else if (siguiente == pos_k + 1L) {
          # Corte forzado entre líneas adyacentes
          fines[k]        <- pos_k
          inicios[k + 1L] <- siguiente
        } else {
          # Código entre elementos sin línea en blanco: no dividir
          return(NULL)
        }
      } else {
        # Último elemento: buscar línea en blanco después
        corte_blanco <- 0L
        if ((pos_k + 1L) <= length(buffer)) {
          for (j in (pos_k + 1L):length(buffer)) {
            if (grepl("^\\s*$", buffer[j])) {
              corte_blanco <- j
              break
            }
          }
        }
        if (corte_blanco > 0L) {
          fines[k]     <- corte_blanco - 1L
          resto_inicio <- corte_blanco + 1L
        } else {
          fines[k]     <- length(buffer)
          resto_inicio <- length(buffer) + 1L
        }
      }
    }

    # Si solo hay un elemento y el resto no existe (etiqueta al final
    # equivaldría al comportamiento sin dividir), no merece la pena.
    if (n_elem == 1L && resto_inicio > length(buffer)) {
      return(NULL)
    }

    # Construir bloques
    bloques <- list()
    for (k in seq_len(n_elem)) {
      sub_b <- if (fines[k] >= inicios[k])
                 buffer[inicios[k]:fines[k]] else character(0)
      bloques[[length(bloques) + 1L]] <- list(
        tipo         = "sub_chunk_elem",
        buffer       = sub_b,
        elem_tipo    = secuencia[k],
        elem_caption = captions[k]
      )
    }
    if (resto_inicio <= length(buffer)) {
      resto <- buffer[resto_inicio:length(buffer)]
      if (any(!grepl("^\\s*$", resto))) {
        bloques[[length(bloques) + 1L]] <- list(
          tipo   = "sub_chunk_resto",
          buffer = resto
        )
      }
    }

    # Verificación de balance de delimitadores
    contar_char <- function(txt, ch) {
      sum(strsplit(txt, "", fixed = TRUE)[[1]] == ch)
    }
    for (b in bloques) {
      txt <- paste(b$buffer, collapse = "\n")
      if (contar_char(txt, "{") != contar_char(txt, "}") ||
          contar_char(txt, "(") != contar_char(txt, ")") ||
          contar_char(txt, "[") != contar_char(txt, "]")) {
        return(NULL)
      }
    }

    bloques
  }

  # ===================================================================
  # 4. Recorrido principal
  # ===================================================================
  cont_tabla  <- 0L
  cont_figura <- 0L
  estado <- "texto"
  chunk_header <- ""
  chunk_buffer <- character(0)
  salida <- character(0)
  registro <- character(0)
  # Estado global entre chunks: contenedores y diferidos tentativos
  # creados en chunks previos pueden ser consumidos en chunks
  # posteriores (p.ej. `var <- f(...)` en chunk N y `var[[expr]]` en
  # chunk N+1).
  contenedores_globales <- list()
  tentativos_globales   <- list()

  push <- function(...) salida <<- c(salida, ...)

  i <- 1L
  n_lineas <- length(lineas)
  while (i <= n_lineas) {
    linea <- lineas[i]

    if (estado == "texto") {
      if (es_chunk_inicio(linea)) {
        estado <- "chunk"
        chunk_header <- linea
        chunk_buffer <- character(0)
        i <- i + 1L
        next
      }
      td <- detectar_tabla_md(lineas, i)
      if (isTRUE(td$es_tabla)) {
        cont_tabla <- cont_tabla + 1L
        for (k in i:td$fin) push(lineas[k])
        desc <- if (incluir_caption_tabla && !is.na(td$caption)) td$caption else NULL
        push(bloque_numeracion("tabla", num_capitulo, cont_tabla, desc))
        registro <- c(registro, sprintf(
          "  Tabla  %d.%d  (markdown manual, linea %d)%s",
          num_capitulo, cont_tabla, i,
          if (!is.na(td$caption))
            sprintf(" - caption: \"%s\"", td$caption) else ""))
        i <- td$fin + 1L
        next
      }
      if (grepl("!\\[", linea, perl = TRUE)) {
        res <- procesar_linea_imagen(linea)
        if (isTRUE(res$contado)) {
          cont_figura <- cont_figura + 1L
          push(res$linea)
          desc <- if (incluir_alt_figura) res$alt else NULL
          push(bloque_numeracion("figura", num_capitulo, cont_figura, desc))
          registro <- c(registro, sprintf(
            "  Figura %d.%d  (imagen markdown, linea %d)",
            num_capitulo, cont_figura, i))
          i <- i + 1L
          next
        }
      }
      push(linea)
      i <- i + 1L
    } else {
      if (es_chunk_fin(linea)) {
        skip <- es_eval_false(chunk_header)
        info <- analizar_chunk(chunk_buffer,
                              contenedores_iniciales = contenedores_globales,
                              diferidos_tentativos_iniciales = tentativos_globales)
        # Fusionar estado global con lo devuelto (no sobrescribir).
        # Así los tentativos/contenedores creados en chunks previos
        # persisten para los chunks siguientes.
        if (!skip && !is.null(info$contenedores)) {
          contenedores_globales <- modifyList(contenedores_globales,
                                              info$contenedores)
        }
        if (!skip && !is.null(info$diferidos_tentativos)) {
          tentativos_globales <- modifyList(tentativos_globales,
                                            info$diferidos_tentativos)
        }
        # Si un tentativo ha sido promovido a contenedor en este chunk,
        # ya no necesita estar en tentativos.
        if (length(contenedores_globales) > 0 &&
            length(tentativos_globales) > 0) {
          for (k in names(contenedores_globales)) {
            tentativos_globales[[k]] <- NULL
          }
        }

        if (skip) {
          # Chunk eval=FALSE: dejar tal cual, no se renderiza.
          push(chunk_header); push(chunk_buffer); push(linea)
        } else if (length(info$secuencia) == 0L) {
          # Chunk eval=TRUE pero sin tablas/figuras directas detectadas.
          # PUEDE contener definiciones de funciones cuyos kables se
          # ejecutarán al llamar la función — limpiamos sus captions
          # para evitar autonumeración bookdown.
          chunk_limpio <- quitar_todos_captions(chunk_buffer)
          push(chunk_header); push(chunk_limpio); push(linea)
        } else if (info$tiene_bucle) {
          chunk_limpio <- quitar_todos_captions(chunk_buffer)
          push(chunk_header); push(chunk_limpio); push(linea)
          n_tab <- sum(info$secuencia == "tabla")
          n_fig <- sum(info$secuencia == "figura")
          registro <- c(registro, sprintf(
            "  Bucle  (chunk linea %d) - sin numerar (tab=%d fig=%d)",
            i - length(chunk_buffer) - 1L, n_tab, n_fig))
        } else {
          chunk_limpio <- quitar_todos_captions(chunk_buffer)
          # Las posiciones e info del análisis ORIGINAL siguen siendo
          # válidas para el limpio (las líneas se vacían pero no se
          # eliminan, los números de línea no cambian).

          if (length(info$secuencia) >= 1L) {
            bloques <- dividir_chunk_mixto(chunk_limpio,
                                          info$posiciones,
                                          info$secuencia,
                                          info$captions)
          } else {
            bloques <- NULL
          }

          if (!is.null(bloques)) {
            # Emitir sub-chunks intercalados con etiquetas
            for (b in bloques) {
              push(chunk_header)
              push(b$buffer)
              push("```")
              if (b$tipo == "sub_chunk_elem") {
                if (b$elem_tipo == "tabla") {
                  cont_tabla <- cont_tabla + 1L
                  desc <- if (incluir_caption_tabla && !is.na(b$elem_caption))
                            b$elem_caption else NULL
                  push(bloque_numeracion("tabla", num_capitulo,
                                        cont_tabla, desc))
                  registro <- c(registro, sprintf(
                    "  Tabla  %d.%d  (sub-chunk de mixto)%s",
                    num_capitulo, cont_tabla,
                    if (!is.na(b$elem_caption))
                      sprintf(" - caption: \"%s\"", b$elem_caption) else ""))
                } else {
                  cont_figura <- cont_figura + 1L
                  push(bloque_numeracion("figura", num_capitulo, cont_figura))
                  registro <- c(registro, sprintf(
                    "  Figura %d.%d  (sub-chunk de mixto)",
                    num_capitulo, cont_figura))
                }
              }
            }
          } else {
            # No se puede dividir: comportamiento clásico (etiquetas al
            # final del chunk completo)
            push(chunk_header); push(chunk_limpio); push(linea)
            for (k in seq_along(info$secuencia)) {
              tipo <- info$secuencia[k]
              cap  <- info$captions[k]
              if (tipo == "tabla") {
                cont_tabla <- cont_tabla + 1L
                desc <- if (incluir_caption_tabla && !is.na(cap)) cap else NULL
                push(bloque_numeracion("tabla", num_capitulo, cont_tabla, desc))
                registro <- c(registro, sprintf(
                  "  Tabla  %d.%d  (chunk linea %d)%s",
                  num_capitulo, cont_tabla,
                  i - length(chunk_buffer) - 1L,
                  if (!is.na(cap)) sprintf(" - caption: \"%s\"", cap) else ""))
              } else {
                cont_figura <- cont_figura + 1L
                push(bloque_numeracion("figura", num_capitulo, cont_figura))
                registro <- c(registro, sprintf(
                  "  Figura %d.%d  (chunk linea %d)",
                  num_capitulo, cont_figura,
                  i - length(chunk_buffer) - 1L))
              }
            }
          }
        }

        estado <- "texto"
        chunk_header <- ""
        chunk_buffer <- character(0)
      } else {
        chunk_buffer <- c(chunk_buffer, linea)
      }
      i <- i + 1L
    }
  }

  # ===================================================================
  # 5. Volcado
  # ===================================================================
  con <- file(ruta_salida, "wb")
  writeLines(salida, con, sep = eol_original, useBytes = TRUE)
  close(con)

  if (verbose) {
    cat(sprintf("[OK] %s -> %s\n",
                basename(ruta_entrada), basename(ruta_salida)))
    cat(sprintf("     Capítulo %d. Tablas: %d. Figuras: %d.\n",
                num_capitulo, cont_tabla, cont_figura))
    if (length(registro) > 0) {
      cat("     Detalle:\n")
      cat(paste0("    ", registro, "\n"), sep = "")
    }
  }

  invisible(list(
    entrada   = ruta_entrada,
    salida    = ruta_salida,
    capitulo  = num_capitulo,
    n_tablas  = cont_tabla,
    n_figuras = cont_figura,
    registro  = registro
  ))
}

if (sys.nframe() == 0 &&
    length(commandArgs(trailingOnly = TRUE)) >= 1) {
  args <- commandArgs(trailingOnly = TRUE)
  entrada <- args[1]
  salida  <- if (length(args) >= 2 && nzchar(args[2])) args[2] else NULL
  cap     <- if (length(args) >= 3) as.integer(args[3]) else NULL
  procesar_rmd(entrada, salida, cap)
}
