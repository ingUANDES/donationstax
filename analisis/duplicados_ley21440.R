# Duplicados exactos de la nómina de donaciones de la Ley 21.440 (https://github.com/ingUANDES/donationstax/issues/16).
# Contraparte en R de duplicados_ley21440.py: mismas reglas (descritas en su cabecera), lector distinto
# (readxl en vez de calamine). contraste_datos.py exige que las tres salidas coincidan. La comprobación
# contra el perfil versionado (--perfil) solo está en Python. Nada se excluye.
#
# Uso: Rscript duplicados_ley21440.R <carpeta_xlsx> <por_anio.csv> <multiplicidad.csv> <concentracion.csv>
#      Rscript duplicados_ley21440.R --csv <prueba.csv> <por_anio.csv> <multiplicidad.csv> <concentracion.csv>
# Conviene correrlo con LC_ALL=C.UTF-8. En el modo --csv no hace falta readxl.

aqui_dup <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(aqui_dup, "esquema_ley21440.R"))

CAMPOS_ANIO <- c(
  "anio", "n_filas_datos", "n_filas_distintas", "n_duplicados_exactos", "n_grupos_repetidos",
  "n_filas_repetidas", "n_filas_sin_repetir", "multiplicidad_max", "multiplicidad_mediana_filas_repetidas",
  "suma_valor", "suma_valor_sin_duplicados", "suma_valor_copias_extra", "suma_valor_filas_repetidas",
  "suma_valor_filas_sin_repetir", "prop_filas_copias_extra", "prop_valor_copias_extra"
)
CAMPOS_MULT <- c("clave", "anio", "multiplicidad", "n_filas_distintas", "n_filas", "suma_valor")
CAMPOS_CONC <- c(
  "clave", "anio", "dimension", "n_categorias", "n_categorias_con_copias", "n_copias_extra",
  "prop_copias_top1", "prop_copias_top5", "prop_copias_top10", "n_categorias_mitad_copias",
  "n_categorias_90_copias", "categoria_top1_copias", "prop_filas_top1", "prop_filas_top5", "prop_filas_top10",
  "n_categorias_mitad_filas", "n_categorias_90_filas"
)
CON_TOP1 <- c("fecha", "valor")
DIMENSIONES <- c("donataria", "fecha", "donataria_fecha", "valor")
TOPS <- c(1L, 5L, 10L)
TOL_REL <- 1e-9

prop <- function(a, b) if (b == 0) "" else g17(as.numeric(a) / as.numeric(b))

# Filas de datos (matriz de texto) e índices de columnas de cada año, desde <carpeta>/<año>.xlsx.
filas_xlsx <- function(carpeta) {
  anios <- sort(as.integer(sub("\\.xlsx$", "", list.files(carpeta, pattern = "^[0-9]{4}\\.xlsx$"))))
  stopifnot(length(anios) > 0)
  setNames(lapply(anios, function(a) {
    tb <- leer21440(file.path(carpeta, paste0(a, ".xlsx")))
    m <- tb$m
    cuerpo <- if (tb$fila_enc < nrow(m)) m[(tb$fila_enc + 1):nrow(m), , drop = FALSE] else m[0, , drop = FALSE]
    cuerpo <- cuerpo[rowSums(cuerpo != "") > 0, , drop = FALSE]
    list(m = cuerpo[es_fila_datos21440(cuerpo, tb$idx), , drop = FALSE], idx = tb$idx)
  }), anios)
}

# Lo mismo desde un CSV de prueba con una columna `anio` y los encabezados de la nómina.
filas_csv <- function(ruta) {
  df <- read.csv(ruta, colClasses = "character", check.names = FALSE, na.strings = character(0),
                 fileEncoding = "UTF-8")
  anio <- as.integer(df[["anio"]])
  m <- as.matrix(df[, names(df) != "anio", drop = FALSE])
  idx <- roles(norm(colnames(m)), ROLES21440)
  stopifnot(all(idx[c("rut", "donataria", "valor", "fecha")] > 0))
  anios <- sort(unique(anio))
  setNames(lapply(anios, function(a) {
    x <- m[anio == a, , drop = FALSE]
    x <- x[rowSums(x != "") > 0, , drop = FALSE]
    list(m = x[es_fila_datos21440(x, idx), , drop = FALSE], idx = idx)
  }), anios)
}

# Multiplicidad de cada fila, marca de copia extra (duplicated: todas menos la primera de cada grupo)
# y multiplicidades de las filas distintas.
grupos <- function(m) {
  llave <- if (nrow(m) == 0) character(0) else
    do.call(paste, c(lapply(seq_len(ncol(m)), function(j) m[, j]), sep = "\x1f"))
  u <- unique(llave)
  g <- match(llave, u)
  tab <- tabulate(g, nbins = length(u))
  list(k = tab[g], extra = duplicated(llave), tab = tab)
}

fila_anio <- function(etiqueta, k, extra, tab, v) {
  n <- length(k); d <- length(tab)
  rep_ <- k > 1L
  n_rep <- sum(rep_); n_grupos <- sum(tab > 1L)
  stopifnot(n_rep == (n - d) + n_grupos)
  s <- sum(v); s_extra <- sum(v[extra]); s_unicas <- sum(v[!extra]); s_rep <- sum(v[rep_]); s_sin <- sum(v[!rep_])
  stopifnot(abs(s - s_extra - s_unicas) <= TOL_REL * max(1, abs(s)), abs(s - s_rep - s_sin) <= TOL_REL * max(1, abs(s)))
  data.frame(
    anio = as.character(etiqueta), n_filas_datos = n, n_filas_distintas = d, n_duplicados_exactos = n - d,
    n_grupos_repetidos = n_grupos, n_filas_repetidas = n_rep, n_filas_sin_repetir = n - n_rep,
    multiplicidad_max = if (d > 0) max(tab) else 0L,
    multiplicidad_mediana_filas_repetidas = if (n_rep > 0) g17(as.numeric(median(k[rep_]))) else "",
    suma_valor = g17(s), suma_valor_sin_duplicados = g17(s_unicas), suma_valor_copias_extra = g17(s_extra),
    suma_valor_filas_repetidas = g17(s_rep), suma_valor_filas_sin_repetir = g17(s_sin),
    prop_filas_copias_extra = prop(n - d, n), prop_valor_copias_extra = prop(s_extra, s),
    stringsAsFactors = FALSE
  )[, CAMPOS_ANIO]
}

filas_mult <- function(anio, k, tab, v) {
  ks <- sort(unique(tab))
  out <- data.frame(clave = paste0(anio, "|", ks), anio = as.integer(anio), multiplicidad = ks,
                    n_filas_distintas = vapply(ks, function(kk) sum(tab == kk), integer(1)),
                    stringsAsFactors = FALSE)
  out$n_filas <- out$multiplicidad * out$n_filas_distintas
  out$suma_valor <- vapply(ks, function(kk) g17(sum(v[k == kk])), "")
  stopifnot(sum(out$n_filas) == length(k), sum(out$n_filas_distintas) == length(tab))
  out[, CAMPOS_MULT]
}

# Proporción en las categorías con más conteo (TOPS) y categorías para la mitad y el 90 %.
resumen_conc <- function(conteos, total) {
  if (total == 0) return(list(top = rep("", length(TOPS)), mitad = "", noventa = ""))
  s <- sort(conteos, decreasing = TRUE)
  acum <- cumsum(as.numeric(s))
  list(top = vapply(TOPS, function(t) prop(sum(s[seq_len(min(t, length(s)))]), total), ""),
       mitad = as.character(which(2 * acum >= total)[1]),
       noventa = as.character(which(10 * acum >= 9 * total)[1]))
}

# Fecha (ISO) o Valor con más copias extra; entre empates, el de menor valor numérico.
top1 <- function(u, copias, dim) {
  if (!(dim %in% CON_TOP1) || !any(copias > 0L)) return("")
  cand <- u[copias == max(copias)]
  c1 <- cand[which.min(as.numeric(cand))]
  if (dim == "fecha") format(as.Date(floor(as.numeric(c1)), origin = "1899-12-30")) else c1
}

filas_conc <- function(anio, m, idx, extra) {
  rut <- m[, idx[["rut"]]]; fecha <- m[, idx[["fecha"]]]
  cats <- list(donataria = rut, fecha = fecha, donataria_fecha = paste(rut, fecha, sep = "\x1f"),
               valor = m[, idx[["valor"]]])
  do.call(rbind, lapply(DIMENSIONES, function(dim) {
    x <- cats[[dim]]; u <- unique(x); g <- match(x, u)
    por_filas <- tabulate(g, nbins = length(u)); por_copias <- tabulate(g[extra], nbins = length(u))
    rc <- resumen_conc(por_copias, sum(extra)); rf <- resumen_conc(por_filas, length(x))
    data.frame(
      clave = paste0(anio, "|", dim), anio = as.integer(anio), dimension = dim, n_categorias = length(u),
      n_categorias_con_copias = sum(por_copias > 0L), n_copias_extra = sum(extra),
      prop_copias_top1 = rc$top[1], prop_copias_top5 = rc$top[2], prop_copias_top10 = rc$top[3],
      n_categorias_mitad_copias = rc$mitad, n_categorias_90_copias = rc$noventa,
      categoria_top1_copias = top1(u, por_copias, dim),
      prop_filas_top1 = rf$top[1], prop_filas_top5 = rf$top[2], prop_filas_top10 = rf$top[3],
      n_categorias_mitad_filas = rf$mitad, n_categorias_90_filas = rf$noventa,
      stringsAsFactors = FALSE
    )
  }))[, CAMPOS_CONC]
}

args <- commandArgs(trailingOnly = TRUE)
modo_csv <- "--csv" %in% args
args <- args[args != "--csv"]
stopifnot(length(args) == 4)
datos <- if (modo_csv) filas_csv(args[1]) else filas_xlsx(args[1])
partes <- lapply(names(datos), function(a) {
  message("duplicados ", a)
  x <- datos[[a]]
  gr <- grupos(x$m)
  v <- a_num(x$m[, x$idx[["valor"]]])
  list(k = gr$k, extra = gr$extra, tab = gr$tab, v = v,
       fila = fila_anio(a, gr$k, gr$extra, gr$tab, v),
       mult = filas_mult(a, gr$k, gr$tab, v),
       conc = filas_conc(a, x$m, x$idx, gr$extra))
})
todos <- function(campo) do.call(c, lapply(partes, `[[`, campo))
anios <- names(datos)
suma_anios <- fila_anio(paste0(anios[1], "-", anios[length(anios)]), todos("k"), todos("extra"), todos("tab"), todos("v"))
write.csv(do.call(rbind, c(lapply(partes, `[[`, "fila"), list(suma_anios))), args[2], row.names = FALSE, quote = TRUE)
write.csv(do.call(rbind, lapply(partes, `[[`, "mult")), args[3], row.names = FALSE, quote = TRUE)
write.csv(do.call(rbind, lapply(partes, `[[`, "conc")), args[4], row.names = FALSE, quote = TRUE)
