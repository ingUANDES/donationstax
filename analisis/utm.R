# Lee la UTM mensual de 2008 a 2025 de las páginas del SII (https://github.com/ingUANDES/donationstax/issues/33).
# Contraparte de utm.py: mismas reglas de lectura, mismas comprobaciones y mismas salidas (ver la cabecera
# de utm.py). Diferencias: no comprueba el sha256 de los archivos (la base de R no lo trae; lo hace utm.py)
# y no tiene autoprueba. utm_mensual.csv debe ser idéntico byte a byte al de Python; utm_por_anio.csv se
# contrasta con contraste.py.
#
# Uso: Rscript utm.R <carpeta_html> <manifiesto.csv> <utm_mensual.csv> <utm_por_anio.csv>

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 4)
carpeta <- args[1]; manifiesto <- args[2]; sal_mensual <- args[3]; sal_anio <- args[4]

ANIOS <- 2008:2025
MESES <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre",
           "Octubre", "Noviembre", "Diciembre")
RE_ANIO <- "<th[^>]*rowspan=['\"]2['\"][^>]*>\\s*(?:<strong>)?\\s*([0-9]{4})\\b"
RE_FILA <- "<tr>\\s*<th[^>]*>\\s*([A-Za-z]+)\\s*</th>((?:\\s*<td[^>]*>[^<]*</td>)+)\\s*</tr>"
RE_CELDA <- "<td[^>]*>([^<]*)</td>"
RE_ENTERO <- "^[0-9]{1,3}(\\.[0-9]{3})*$"
RE_VAR <- "^-?[0-9]+([,.][0-9]+)?$"

falla <- function(...) stop(paste0("AssertionError: ", ...), call. = FALSE)

grupos <- function(texto, patron) {
  # Todas las coincidencias de `patron` con sus grupos de captura (matriz: coincidencia, grupo 1, ...).
  m <- regmatches(texto, gregexec(patron, texto, perl = TRUE))[[1]]
  if (length(m) == 0) return(matrix(character(0), nrow = 0, ncol = 0))
  t(m)
}

leer_pagina <- function(ruta, anio) {
  bytes <- readBin(ruta, "raw", file.info(ruta)$size)
  texto <- rawToChar(bytes)
  Encoding(texto) <- "UTF-8"
  if (!validUTF8(texto)) falla(ruta, ": no es UTF-8")
  m <- regmatches(texto, regexec(RE_ANIO, texto, perl = TRUE))[[1]]
  if (length(m) < 2 || as.integer(m[2]) != anio) falla(ruta, ": el encabezado no es del año ", anio)
  filas <- grupos(texto, RE_FILA)
  if (nrow(filas) != 12 || !identical(filas[, 2], MESES)) falla(ruta, ": meses ", paste(filas[, 2], collapse = " "))
  out <- vector("list", 12)
  for (mes in 1:12) {
    v <- trimws(grupos(filas[mes, 3], RE_CELDA)[, 2])
    if (length(v) != 6) falla(ruta, " mes ", mes, ": ", length(v), " celdas")
    if (!grepl(RE_ENTERO, v[1], perl = TRUE) || !grepl(RE_ENTERO, v[2], perl = TRUE)) falla(ruta, " mes ", mes, ": UTM o UTA")
    if (!grepl(RE_VAR, v[4], perl = TRUE)) falla(ruta, " mes ", mes, ": variación mensual ", v[4])
    out[[mes]] <- data.frame(anio = anio, mes = mes, utm = as.numeric(gsub(".", "", v[1], fixed = TRUE)),
                             uta = as.numeric(gsub(".", "", v[2], fixed = TRUE)),
                             ipc_variacion_mensual_pct = sub(",", ".", v[4], fixed = TRUE),
                             stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

man <- read.csv(manifiesto, colClasses = "character", encoding = "UTF-8")
if (!identical(sort(as.integer(man$anio)), ANIOS)) falla(manifiesto, ": años ", paste(man$anio, collapse = " "))
serie <- do.call(rbind, lapply(ANIOS, function(a) leer_pagina(file.path(carpeta, sprintf("utm%d.htm", a)), a)))

if (any(serie$uta != 12 * serie$utm)) falla("UTA distinta de 12 × UTM en ", sum(serie$uta != 12 * serie$utm), " meses")
v <- as.numeric(serie$ipc_variacion_mensual_pct)
for (t in 3:nrow(serie)) {
  previa <- serie$utm[t - 1]
  if (abs(serie$utm[t] - previa * (1 + v[t - 2] / 100)) > 0.0005 * previa + 1)
    falla(serie$anio[t], "-", serie$mes[t], ": UTM ", serie$utm[t], " no es ", previa, " reajustada en ", v[t - 2], " %")
}

ent <- function(x) formatC(x, format = "d", big.mark = "")
mensual <- data.frame(anio = serie$anio, mes = serie$mes, utm = ent(serie$utm), uta = ent(serie$uta),
                      ipc_variacion_mensual_pct = serie$ipc_variacion_mensual_pct, stringsAsFactors = FALSE)
write.csv(mensual, sal_mensual, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

num <- function(x) sprintf("%.17g", x)
filas <- lapply(ANIOS, function(a) {
  u <- serie$utm[serie$anio == a]
  dic <- u[12]; umin <- min(u); umax <- max(u)
  data.frame(anio = a, utm_diciembre = ent(dic), utm_min = ent(umin), mes_utm_min = which.min(u),
             utm_max = ent(umax), mes_utm_max = which.max(u), razon_dic_sobre_min = num(dic / umin),
             razon_dic_sobre_max = num(dic / umax), log_dic_sobre_min = num(log(dic / umin)),
             stringsAsFactors = FALSE)
})
write.csv(do.call(rbind, filas), sal_anio, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
cat(sprintf("UTM: %d meses de %d a %d; comprobaciones pasan\n", nrow(serie), min(ANIOS), max(ANIOS)))
