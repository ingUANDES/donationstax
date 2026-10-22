# Perfil del esquema de las planillas anuales «Donaciones recibidas Ley N°19.885».
# Contraparte en R de esquema_datos.py: mismas reglas, lector distinto (readxl en vez de
# calamine). contraste_datos.py exige que las dos salidas coincidan. Las reglas están
# descritas en la cabecera de esquema_datos.py y se repiten aquí solo en código.
#
# Uso: Rscript esquema_datos.R <carpeta_xlsx> <salida.csv> [años separados por coma]
# Conviene correrlo con LC_ALL=C.UTF-8 para que chartr maneje los acentos.

suppressPackageStartupMessages(library(readxl))

NUM <- "^-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?$"
SERIAL <- "^[0-9]{5}(\\.[0-9]+)?$"
ISO <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}"
ROLES <- c(
  anio = "^ANO$", fecha = "^FECHA", proyecto = "^NOMBREDELPROYECTO",
  folio_proyecto = "^FOLIOPROYECTO", donatario = "^DONATARIO$",
  folio_institucion = "^FOLIOINSTITUCION", rut_institucion = "^RUTINSTITUCION",
  donante = "^DONANTE$", rut_donante = "^RUT$", fm = "^MONTOFONDOMIXTO",
  minst = "^MONTOINSTITUCION", mtotal = "^MONTOTOTALDONACION", region = "^REGION"
)
CAMPOS <- c(
  "anio", "hoja", "fila_encabezado", "columnas_normalizadas", "n_columnas", "n_filas_hoja",
  "n_filas_datos", "n_anio_num_monto_no_num", "n_filas_sin_anio", "anios_valores",
  "tiene_rut_donante", "tiene_rut_institucion", "tiene_folio_proyecto",
  "tiene_folio_institucion", "tiene_region", "n_fm_numerico", "n_fm_si_no", "n_fm_otro",
  "n_fm_vacio", "n_fecha_serial", "n_fecha_iso_texto", "n_fecha_otro_texto", "n_fecha_vacia",
  "fecha_min", "fecha_max", "n_anio_distinto_de_fecha", "suma_monto_total",
  "suma_monto_institucion", "suma_monto_fm", "n_duplicados_exactos", "n_donantes_distintos",
  "n_donantes_con_rut", "n_donatarios_distintos", "n_celdas_con_u_fffd"
)

norm <- function(x) gsub("[^A-Z0-9]", "", toupper(chartr("ÁÉÍÓÚÜÑáéíóúüñ", "AEIOUUNAEIOUUN", x)))
a_num <- function(x) ifelse(grepl(NUM, x), suppressWarnings(as.numeric(x)), NA_real_)

# readxl falla con las cadenas corruptas que traen los archivos de 2021 a 2025 (nombres con
# acentos dañados): caracteres fuera del plano básico y escapes OOXML `_xHHHH_` de mitades de par
# sustituto. Se lee una copia de sharedStrings.xml en la que ambos están sustituidos por U+FFFD
# (la misma regla que aplica esquema_datos.py) y los demás escapes `_xHHHH_` se protegen con
# `_x005F_` para que readxl los deje literales, como hace calamine.
reparar <- function(ruta) {
  tmp <- tempfile("xlsx_"); dir.create(tmp)
  unzip(ruta, files = "xl/sharedStrings.xml", exdir = tmp)
  f <- file.path(tmp, "xl", "sharedStrings.xml")
  x <- rawToChar(readBin(f, "raw", file.size(f)))
  cuatro <- "[\\xf0-\\xf4][\\x80-\\xbf]{3}"
  sust <- "_x(D[89A-Fa-f][0-9A-Fa-f]{2})_"
  esc <- "_x([0-9A-Fa-f]{4})_"
  if (!grepl(cuatro, x, useBytes = TRUE, perl = TRUE) && !grepl(esc, x, useBytes = TRUE, perl = TRUE)) return(ruta)
  x <- gsub(cuatro, "\xef\xbf\xbd", x, useBytes = TRUE, perl = TRUE)
  x <- gsub(sust, "\xef\xbf\xbd", x, useBytes = TRUE, perl = TRUE)
  x <- gsub(esc, "_x005F_x\\1_", x, useBytes = TRUE, perl = TRUE)
  writeBin(charToRaw(x), f)
  copia <- file.path(tmp, "copia.xlsx")
  file.copy(ruta, copia)
  owd <- setwd(tmp); on.exit(setwd(owd))
  zip(copia, "xl/sharedStrings.xml", flags = "-q")
  copia
}

leer_hoja <- function(ruta, h) {
  df <- suppressMessages(read_excel(ruta, sheet = h, col_names = FALSE, col_types = "text", trim_ws = FALSE,
                                    .name_repair = "minimal"))
  if (nrow(df) == 0 || ncol(df) == 0) return(matrix("", 0, 0))
  m <- as.matrix(df)
  m[is.na(m)] <- ""
  fil <- which(rowSums(m != "") > 0)
  if (length(fil) == 0) return(matrix("", 0, 0))
  m <- m[fil[1]:fil[length(fil)], , drop = FALSE]
  col <- which(colSums(m != "") > 0)
  m[, col[1]:col[length(col)], drop = FALSE]
}

perfil <- function(anio, ruta) {
  ruta <- reparar(ruta)
  hoja <- NULL
  for (h in excel_sheets(ruta)) {
    m <- leer_hoja(ruta, h)
    if (nrow(m) == 0) next
    for (i in seq_len(min(10, nrow(m)))) {
      if (norm(m[i, 1]) == "ANO") { hoja <- h; fila_enc <- i; break }
    }
    if (!is.null(hoja)) break
  }
  stopifnot(!is.null(hoja))
  enc <- norm(m[fila_enc, ])
  idx <- vapply(ROLES, function(p) { j <- which(grepl(p, enc))[1]; if (is.na(j)) 0L else j }, integer(1))
  cuerpo <- if (fila_enc < nrow(m)) m[(fila_enc + 1):nrow(m), , drop = FALSE] else m[0, , drop = FALSE]
  col <- function(r) if (idx[[r]] == 0) rep("", nrow(cuerpo)) else cuerpo[, idx[[r]]]

  no_vacias <- rowSums(cuerpo != "") > 0
  a <- a_num(col("anio")); mt <- a_num(col("mtotal"))
  sin_anio <- no_vacias & (is.na(a) | a != floor(a))
  anio_num_monto_no <- no_vacias & !sin_anio & is.na(mt)
  datos <- no_vacias & !sin_anio & !is.na(mt)
  cd <- cuerpo[datos, , drop = FALSE]
  ad <- as.integer(a[datos])

  tab <- table(ad)
  v <- col("fm")[datos]
  fm_vacio <- v == ""; fm_num <- !fm_vacio & grepl(NUM, v)
  fm_sino <- !fm_vacio & !fm_num & norm(v) %in% c("SI", "NO")
  vf <- col("fecha")[datos]
  f_vacia <- vf == ""; f_serial <- !f_vacia & grepl(SERIAL, vf)
  f_iso <- !f_vacia & !f_serial & grepl(ISO, vf)
  f_otro <- !f_vacia & !f_serial & !f_iso
  fechas <- as.Date(floor(as.numeric(vf[f_serial])), origin = "1899-12-30")
  suma <- function(r) { x <- a_num(col(r)[datos]); sum(x[!is.na(x)]) }
  dup <- if (nrow(cd) == 0) 0L else sum(duplicated(do.call(paste, c(lapply(seq_len(ncol(cd)), function(j) cd[, j]), sep = "\x1f"))))
  distintos <- function(r) { x <- col(r)[datos]; length(unique(x[x != ""])) }

  data.frame(
    anio = anio, hoja = hoja, fila_encabezado = fila_enc,
    columnas_normalizadas = paste(enc[enc != ""], collapse = "|"), n_columnas = sum(enc != ""),
    n_filas_hoja = sum(no_vacias), n_filas_datos = sum(datos),
    n_anio_num_monto_no_num = sum(anio_num_monto_no), n_filas_sin_anio = sum(sin_anio),
    anios_valores = paste(sprintf("%s:%d", names(tab), as.integer(tab)), collapse = "|"),
    tiene_rut_donante = tolower(idx[["rut_donante"]] > 0), tiene_rut_institucion = tolower(idx[["rut_institucion"]] > 0),
    tiene_folio_proyecto = tolower(idx[["folio_proyecto"]] > 0), tiene_folio_institucion = tolower(idx[["folio_institucion"]] > 0),
    tiene_region = tolower(idx[["region"]] > 0),
    n_fm_numerico = sum(fm_num), n_fm_si_no = sum(fm_sino), n_fm_otro = sum(!fm_vacio & !fm_num & !fm_sino),
    n_fm_vacio = sum(fm_vacio),
    n_fecha_serial = sum(f_serial), n_fecha_iso_texto = sum(f_iso), n_fecha_otro_texto = sum(f_otro),
    n_fecha_vacia = sum(f_vacia),
    fecha_min = if (length(fechas)) format(min(fechas)) else "", fecha_max = if (length(fechas)) format(max(fechas)) else "",
    n_anio_distinto_de_fecha = sum(as.integer(format(fechas, "%Y")) != ad[f_serial]),
    suma_monto_total = sprintf("%.17g", suma("mtotal")), suma_monto_institucion = sprintf("%.17g", suma("minst")),
    suma_monto_fm = sprintf("%.17g", suma("fm")),
    n_duplicados_exactos = dup, n_donantes_distintos = distintos("donante"),
    n_donantes_con_rut = sum(col("rut_donante")[datos] != ""), n_donatarios_distintos = distintos("donatario"),
    n_celdas_con_u_fffd = sum(grepl("\uFFFD", cd, fixed = TRUE)),
    stringsAsFactors = FALSE
  )[, CAMPOS]
}

args <- commandArgs(trailingOnly = TRUE)
carpeta <- args[1]; salida <- args[2]
anios <- if (length(args) >= 3) as.integer(strsplit(args[3], ",")[[1]]) else
  sort(as.integer(sub("\\.xlsx$", "", list.files(carpeta, pattern = "^[0-9]{4}\\.xlsx$"))))
res <- do.call(rbind, lapply(anios, function(a) { message("perfil ", a); perfil(a, file.path(carpeta, paste0(a, ".xlsx"))) }))
write.csv(res, salida, row.names = FALSE, quote = TRUE)
