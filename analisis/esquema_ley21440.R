# Perfil del esquema de la nómina de donaciones de la Ley 21.440 (donacionesley21440.gob.cl).
# Contraparte en R de esquema_ley21440.py: mismas reglas, lector distinto (readxl en vez de
# calamine). contraste_datos.py exige que las dos salidas coincidan. Las reglas están descritas en
# la cabecera de esquema_ley21440.py; las de lectura (norm, reparar, leer_hoja) se toman de
# esquema_datos.R.
#
# Uso: Rscript esquema_ley21440.R <carpeta_xlsx> <salida.csv> [años separados por coma]
# Conviene correrlo con LC_ALL=C.UTF-8 para que chartr maneje los acentos.

aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(aqui, "esquema_datos.R"))

INICIO_BENEFICIOS <- as.Date("2022-07-01")
RUT <- "^[0-9]{1,2}\\.[0-9]{3}\\.[0-9]{3}-[0-9K]$"
ROLES21440 <- c(rut = "^RUTDONATARIA$", donataria = "^DONATARIA$", nac_ext = "^NACEXT$",
                valor = "^VALOR$", fecha = "^FECHA$")
CAMPOS21440 <- c(
  "anio", "hoja", "fila_encabezado", "columnas_normalizadas", "n_columnas", "texto_cabecera",
  "total_cabecera", "n_filas_hoja", "n_filas_datos", "n_filas_no_datos", "suma_valor", "valor_min",
  "valor_max", "n_valor_cero", "n_valor_negativo", "n_valor_no_entero", "n_fecha_serial",
  "n_fecha_texto", "n_fecha_vacia", "fecha_min", "fecha_max", "n_fecha_antes_inicio_beneficios",
  "n_anio_fecha_distinto_archivo", "n_duplicados_exactos", "n_rut_distintos", "n_filas_rut_mal_formado",
  "n_rut_dv_invalido", "n_donatarias_distintas", "n_rut_con_varios_nombres", "n_nombres_con_varios_rut",
  "tiene_nac_ext", "nac_ext_valores", "n_celdas_con_u_fffd"
)

g17 <- function(x) sprintf("%.17g", x)

dv_modulo11 <- function(cuerpo) {
  d <- rev(as.integer(strsplit(cuerpo, "")[[1]]))
  factor <- rep(2:7, length.out = length(d))
  r <- 11 - sum(d * factor) %% 11
  if (r == 11) "0" else if (r == 10) "K" else as.character(r)
}

dv_valido <- function(rut) {
  partes <- strsplit(gsub(".", "", rut, fixed = TRUE), "-", fixed = TRUE)[[1]]
  dv_modulo11(partes[1]) == partes[2]
}

# Tabla e índices de columnas; también la usa cruce_donatarias.R.
leer21440 <- function(ruta) {
  tb <- leer_tabla(ruta, "RUTDONATARIA")
  tb$idx <- roles(tb$enc, ROLES21440)
  stopifnot(all(tb$idx[c("rut", "donataria", "valor", "fecha")] > 0))
  tb
}

# Fila de datos: RUT de la donataria no vacío y Valor numérico.
es_fila_datos21440 <- function(cuerpo, idx) cuerpo[, idx[["rut"]]] != "" & !is.na(a_num(cuerpo[, idx[["valor"]]]))

perfil21440 <- function(anio, ruta) {
  tb <- leer21440(ruta)
  hoja <- tb$hoja; fila_enc <- tb$fila_enc; m <- tb$m; enc <- tb$enc; idx <- tb$idx

  arriba <- m[seq_len(fila_enc - 1), , drop = FALSE]
  celdas <- as.vector(t(arriba))
  texto <- paste(celdas[celdas != "" & !grepl(NUM, celdas)], collapse = " | ")
  tot <- arriba[, idx[["valor"]]]
  tot <- tot[grepl(NUM, tot)]
  stopifnot(length(tot) <= 1)

  cuerpo <- if (fila_enc < nrow(m)) m[(fila_enc + 1):nrow(m), , drop = FALSE] else m[0, , drop = FALSE]
  cuerpo <- cuerpo[rowSums(cuerpo != "") > 0, , drop = FALSE]
  col <- function(r, x = cuerpo) if (idx[[r]] == 0) rep("", nrow(x)) else x[, idx[[r]]]
  datos <- es_fila_datos21440(cuerpo, idx)
  cd <- cuerpo[datos, , drop = FALSE]
  v <- a_num(col("valor", cd))

  vf <- col("fecha", cd)
  f_vacia <- vf == ""; f_serial <- !f_vacia & grepl(SERIAL, vf)
  fechas <- as.Date(floor(as.numeric(vf[f_serial])), origin = "1899-12-30")

  ruts <- col("rut", cd); nombres <- col("donataria", cd)
  distintos <- unique(ruts)
  bien <- distintos[grepl(RUT, distintos)]
  pares <- unique(data.frame(rut = ruts, nombre = nombres, stringsAsFactors = FALSE))
  dup <- if (nrow(cd) == 0) 0L else sum(duplicated(do.call(paste, c(lapply(seq_len(ncol(cd)), function(j) cd[, j]), sep = "\x1f"))))
  nac <- if (idx[["nac_ext"]] > 0) {
    x <- col("nac_ext", cd); x[x == ""] <- "<vacio>"
    tb <- table(x); k <- sort(names(tb), method = "radix")
    paste(sprintf("%s:%d", k, as.integer(tb[k])), collapse = "|")
  } else ""

  data.frame(
    anio = anio, hoja = hoja, fila_encabezado = fila_enc,
    columnas_normalizadas = paste(enc[enc != ""], collapse = "|"), n_columnas = sum(enc != ""),
    texto_cabecera = texto, total_cabecera = if (length(tot)) g17(as.numeric(tot)) else "",
    n_filas_hoja = nrow(cuerpo), n_filas_datos = sum(datos), n_filas_no_datos = sum(!datos),
    suma_valor = g17(sum(v)),
    valor_min = if (length(v)) g17(min(v)) else "", valor_max = if (length(v)) g17(max(v)) else "",
    n_valor_cero = sum(v == 0), n_valor_negativo = sum(v < 0), n_valor_no_entero = sum(v != floor(v)),
    n_fecha_serial = sum(f_serial), n_fecha_texto = sum(!f_vacia & !f_serial), n_fecha_vacia = sum(f_vacia),
    fecha_min = if (length(fechas)) format(min(fechas)) else "", fecha_max = if (length(fechas)) format(max(fechas)) else "",
    n_fecha_antes_inicio_beneficios = sum(fechas < INICIO_BENEFICIOS),
    n_anio_fecha_distinto_archivo = sum(as.integer(format(fechas, "%Y")) != anio),
    n_duplicados_exactos = dup,
    n_rut_distintos = length(distintos),
    n_filas_rut_mal_formado = sum(!grepl(RUT, ruts)),
    n_rut_dv_invalido = sum(!vapply(bien, dv_valido, logical(1))),
    n_donatarias_distintas = length(unique(nombres[nombres != ""])),
    n_rut_con_varios_nombres = sum(table(pares$rut) > 1),
    n_nombres_con_varios_rut = sum(table(pares$nombre) > 1),
    tiene_nac_ext = tolower(idx[["nac_ext"]] > 0), nac_ext_valores = nac,
    n_celdas_con_u_fffd = sum(grepl("\uFFFD", cd, fixed = TRUE)),
    stringsAsFactors = FALSE
  )[, CAMPOS21440]
}

# Solo al correrlo con Rscript; con source() (cruce_donatarias.R) quedan definidas las funciones.
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  carpeta <- args[1]; salida <- args[2]
  anios <- if (length(args) >= 3) as.integer(strsplit(args[3], ",")[[1]]) else
    sort(as.integer(sub("\\.xlsx$", "", list.files(carpeta, pattern = "^[0-9]{4}\\.xlsx$"))))
  res <- do.call(rbind, lapply(anios, function(a) { message("perfil ", a); perfil21440(a, file.path(carpeta, paste0(a, ".xlsx"))) }))
  write.csv(res, salida, row.names = FALSE, quote = TRUE)
}
