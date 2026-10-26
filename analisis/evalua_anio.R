# Evalúa el año de la donación en las planillas armonizadas de la Ley 19.885 (D-13, punto 1, y D-14).
# Contraparte en R de evalua_anio.py: mismas tablas y definiciones (descritas en su cabecera).
# contraste_datos.py compara las dos salidas.
#
# Uso: Rscript evalua_anio.R <carpeta_armonizado> <por_anio.csv> <conflictos.csv>

NUM <- "^-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?$"
CONTEOS <- c(
  "n_incluidas", "n_sin_fecha_valida", "n_sin_fecha_anio_columna_igual_archivo", "n_con_fecha_valida",
  "n_con_fecha_en_anio", "n_con_fecha_anio_columna_igual_archivo", "n_fecha_mismo_anio", "n_fecha_otro_anio",
  "n_otro_anio_columna_igual_archivo", "n_otro_anio_columna_igual_fecha", "n_otro_anio_columna_otro",
  "n_anio_columna_no_valido", "n_filas_anio_archivo", "n_filas_anio_fecha"
)
g17 <- function(x) sprintf("%.17g", x)

args <- commandArgs(trailingOnly = TRUE)
arch <- sort(list.files(args[1], pattern = "^[0-9]{4}\\.csv$"))
enc <- names(read.csv(file.path(args[1], arch[1]), nrows = 1, check.names = FALSE))
quiero <- c("anio_archivo", "incluida", "anio_columna", "fecha_donacion", "anio_fecha", "monto_total")
clases <- ifelse(enc %in% quiero, "character", "NULL")
d <- do.call(rbind, lapply(arch, function(f) {
  x <- read.csv(file.path(args[1], f), colClasses = clases, encoding = "UTF-8", na.strings = character(0), check.names = FALSE)
  message("leído ", f)
  x[x$incluida == "1", ]
}))
a <- as.integer(d$anio_archivo)
v <- d$anio_columna
cn <- ifelse(grepl(NUM, v), suppressWarnings(as.numeric(v)), NA_real_)
col <- ifelse(!is.na(cn) & cn == floor(cn) & cn >= 2008 & cn <= 2025, as.integer(cn), NA_integer_)
m <- as.numeric(d$monto_total)
fa <- ifelse(d$anio_fecha == "", NA_integer_, as.integer(d$anio_fecha))
alt <- ifelse(is.na(fa), a, fa)
igual <- function(x, y) !is.na(x) & !is.na(y) & x == y
sin <- is.na(fa); otro <- !sin & fa != a

anios <- sort(unique(c(a, alt)))
por <- function(cond, base = a) vapply(anios, function(y) sum(cond & base == y, na.rm = TRUE), 0L)
t <- data.frame(anio = as.character(anios), stringsAsFactors = FALSE)
t$n_incluidas <- por(TRUE)
t$n_sin_fecha_valida <- por(sin)
t$n_sin_fecha_anio_columna_igual_archivo <- por(sin & igual(col, a))
t$n_con_fecha_valida <- por(!sin)
t$n_con_fecha_en_anio <- por(!sin, fa)
t$n_con_fecha_anio_columna_igual_archivo <- por(!sin & igual(col, a))
t$n_fecha_mismo_anio <- por(!sin & !otro)
t$n_fecha_otro_anio <- por(otro)
t$n_otro_anio_columna_igual_archivo <- por(otro & igual(col, a))
t$n_otro_anio_columna_igual_fecha <- por(otro & igual(col, fa))
t$n_otro_anio_columna_otro <- por(otro & !igual(col, a) & !igual(col, fa))
t$n_anio_columna_no_valido <- por(is.na(col))
t$n_filas_anio_archivo <- por(TRUE)
t$n_filas_anio_fecha <- por(TRUE, alt)
t$suma_monto_anio_archivo <- vapply(anios, function(y) g17(sum(m[a == y])), "")
t$suma_monto_anio_fecha <- vapply(anios, function(y) g17(sum(m[alt == y])), "")
total <- data.frame(anio = "total", stringsAsFactors = FALSE)
for (k in CONTEOS) total[[k]] <- sum(t[[k]])
total$suma_monto_anio_archivo <- g17(sum(m)); total$suma_monto_anio_fecha <- g17(sum(m))
t <- rbind(t, total)
write.csv(t, args[2], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")

o <- which(otro)
dif <- fa[o] - a[o]; mes <- as.integer(substr(d$fecha_donacion[o], 6, 7))
clave <- sprintf("%d|%+d|%02d", a[o], dif, mes)
orden <- order(a[o], dif, mes)
u <- unique(clave[orden])
pos <- match(u, clave)
conf <- data.frame(
  clave = u, anio_archivo = a[o][pos], diferencia = dif[pos], mes_fecha = mes[pos],
  n_filas = vapply(u, function(k) sum(clave == k), 0L),
  n_anio_columna_igual_archivo = vapply(u, function(k) sum(clave == k & igual(col[o], a[o])), 0L),
  n_anio_columna_igual_fecha = vapply(u, function(k) sum(clave == k & igual(col[o], fa[o])), 0L),
  suma_monto_total = vapply(u, function(k) g17(sum(m[o][clave == k])), ""), stringsAsFactors = FALSE
)
write.csv(conf, args[3], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
