# Armoniza las planillas «Donaciones recibidas Ley N°19.885» según D-10 (reglas 1 a 6 y 8).
# Contraparte en R de armonizar.py: mismas reglas (descritas en su cabecera), lector distinto
# (readxl). contraste_datos.py compara las tablas y contraste_armonizado.py las filas.
#
# Uso: Rscript armonizar.R <carpeta_xlsx> <salida_filas> <conteos.csv> <anios_donacion.csv> <diccionario.csv>
# Conviene correrlo con LC_ALL=C.UTF-8.

aqui_arm <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(aqui_arm, "esquema_datos.R"))

DESDE <- as.Date("2008-01-01"); HASTA <- as.Date("2025-12-31")
UMBRAL_R1 <- 0.999
TOL_SUMA <- 1e-6
MESES <- c(ENERO = 1, FEBRERO = 2, MARZO = 3, ABRIL = 4, MAYO = 5, JUNIO = 6, JULIO = 7, AGOSTO = 8,
           SEPTIEMBRE = 9, SETIEMBRE = 9, OCTUBRE = 10, NOVIEMBRE = 11, DICIEMBRE = 12)
ISO <- "^([0-9]{4})-([0-9]{2})-([0-9]{2})( [0-9]{2}:[0-9]{2}:[0-9]{2})?$"
DMA <- "^([0-9]{1,2})[-/., ]([0-9]{1,2})[-/., ]([0-9]{4})$"
LARGO <- "^([0-9]{1,2})(?: DE |\\. ?)([A-Z]+)(?: DE |\\.| )([0-9]{4})$"
RUT_GUION <- "^([0-9]{1,8})-([0-9K])$"
RUT_CUERPO_DV <- "^([0-9]{1,8})([0-9K])$"
RUT_CUERPO <- "^([0-9]{1,8})$"
RUT_SIN_GUION <- "^[0-9]{2,9}$|^[0-9]{1,8}K$"
SEPARADORES_RUT <- "[., \t\r\n\f\v\u00a0]"
TIPOS <- c(anio = "entero", fecha = "fecha", proyecto = "texto", folio_proyecto = "texto", donatario = "texto",
           folio_institucion = "texto", rut_institucion = "rut", donante = "texto", rut_donante = "rut",
           fm = "monto o indicador", minst = "monto", mtotal = "monto", region = "texto")
VARIANTES <- character(0)

CAMPOS_FILA <- c(
  "anio_archivo", "fila", "incluida", "motivo_exclusion", "anio_columna", "anio_donacion", "mes_donacion",
  "fecha_donacion", "fecha_tipo", "anio_fecha", "fecha_otro_anio", "monto_total", "monto_total_origen", "monto_institucion",
  "monto_fm", "fm_tipo", "fm_si_no", "fm_33_medible", "duplicado_exacto", "donatario", "rut_institucion",
  "proyecto", "folio_proyecto", "folio_institucion", "region", "donante", "donante_con_fffd",
  "rut_donante", "rut_cuerpo", "rut_estado"
)
CAMPOS_CONTEO <- c(
  "anio", "variante", "n_filas_hoja", "n_incluidas", "n_excluidas_monto", "n_excluidas_sin_anio",
  "n_total_numerico", "n_total_recuperado", "n_validas_r1", "n_validas_r1_suma_igual", "anio_apto_r1",
  "n_total_cero", "n_total_negativo", "n_fm_monto", "n_fm_si_no", "n_fm_vacio", "n_fm_otro",
  "n_fecha_vacia", "n_fecha_serial", "n_fecha_texto_iso", "n_fecha_texto_dma", "n_fecha_texto_largo",
  "n_fecha_no_interpretable", "n_fecha_invalida", "n_fecha_fuera_de_rango", "n_fecha_valida",
  "n_mes_desconocido", "n_fecha_otro_anio", "n_anio_columna_no_valido", "n_anio_columna_distinto_archivo",
  "n_duplicados_exactos", "n_donante_con_fffd", "formato_rut_sin_guion",
  "n_rut_sin_guion", "n_rut_sin_guion_dv_ok", "n_rut_vacio", "n_rut_dv_valido", "n_rut_dv_invalido",
  "n_rut_sin_dv", "n_rut_mal_formado", "n_rut_sin_columna", "suma_monto_total",
  "suma_monto_total_sin_duplicados"
)

g17 <- function(x) ifelse(is.na(x), "", sprintf("%.17g", x))
mayus <- function(x) chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)

# Módulo 11, vectorizado para cuerpos de 1 a 8 dígitos.
dv_modulo11 <- function(cuerpo) {
  p <- gsub(" ", "0", sprintf("%8s", cuerpo))
  pesos <- c(3, 2, 7, 6, 5, 4, 3, 2)
  s <- rep(0, length(cuerpo))
  for (k in 1:8) s <- s + as.integer(substr(p, k, k)) * pesos[k]
  r <- 11 - s %% 11
  ifelse(r == 11, "0", ifelse(r == 10, "K", as.character(r)))
}

leer_fechas <- function(v) {
  n <- length(v)
  tipo <- rep("no_interpretable", n); fecha <- rep(as.Date(NA), n)
  vacia <- v == ""
  tipo[vacia] <- "vacia"
  ser <- !vacia & grepl(SERIAL, v)
  tipo[ser] <- "serial"
  fecha[ser] <- as.Date(floor(as.numeric(v[ser])), origin = "1899-12-30")
  t <- mayus(gsub("^[ \t\r\n]+|[ \t\r\n]+$", "", v, perl = TRUE))
  resto <- !vacia & !ser
  iso <- resto & grepl(ISO, t, perl = TRUE)
  dma <- resto & !iso & grepl(DMA, t, perl = TRUE)
  lar <- resto & !iso & !dma & grepl(LARGO, t, perl = TRUE)
  y <- m <- d <- rep(NA_integer_, n)
  y[iso] <- as.integer(sub(ISO, "\\1", t[iso], perl = TRUE)); m[iso] <- as.integer(sub(ISO, "\\2", t[iso], perl = TRUE))
  d[iso] <- as.integer(sub(ISO, "\\3", t[iso], perl = TRUE))
  d[dma] <- as.integer(sub(DMA, "\\1", t[dma], perl = TRUE)); m[dma] <- as.integer(sub(DMA, "\\2", t[dma], perl = TRUE))
  y[dma] <- as.integer(sub(DMA, "\\3", t[dma], perl = TRUE))
  mes_nombre <- sub(LARGO, "\\2", t[lar], perl = TRUE)
  conocido <- mes_nombre %in% names(MESES)
  lar[lar] <- conocido
  d[lar] <- as.integer(sub(LARGO, "\\1", t[lar], perl = TRUE)); m[lar] <- as.integer(unname(MESES[mes_nombre[conocido]]))
  y[lar] <- as.integer(sub(LARGO, "\\3", t[lar], perl = TRUE))
  tipo[iso] <- "texto_iso"; tipo[dma] <- "texto_dma"; tipo[lar] <- "texto_largo"
  txt <- iso | dma | lar
  f <- as.Date(sprintf("%04d-%02d-%02d", y[txt], m[txt], d[txt]), format = "%Y-%m-%d")
  f[y[txt] < 1] <- NA  # Python no admite el año 0
  fecha[txt] <- f
  tipo[txt & is.na(fecha)] <- "invalida"
  fuera <- !is.na(fecha) & (fecha < DESDE | fecha > HASTA)
  tipo[fuera] <- "fuera_de_rango"; fecha[fuera] <- NA
  list(tipo = tipo, fecha = fecha)
}

limpiar_rut <- function(v) gsub(SEPARADORES_RUT, "", gsub("\u2013|\u2014", "-", mayus(v), perl = TRUE), perl = TRUE)

formato_rut <- function(v) {
  s <- limpiar_rut(v)
  s <- s[!grepl("-", s, fixed = TRUE) & grepl(RUT_SIN_GUION, s, perl = TRUE)]
  ok <- sum(dv_modulo11(substr(s, 1, nchar(s) - 1)) == substr(s, nchar(s), nchar(s)))
  list(formato = if (length(s) && 2 * ok >= length(s)) "cuerpo_dv" else if (length(s)) "cuerpo" else "cuerpo_dv",
       n = length(s), ok = ok)
}

leer_ruts <- function(v, formato) {
  n <- length(v); s <- limpiar_rut(v)
  estado <- rep("mal_formado", n); cuerpo <- rep("", n)
  con_dv <- formato == "cuerpo_dv" | endsWith(s, "K") | nchar(s) == 9
  g <- grepl(RUT_GUION, s, perl = TRUE)
  cd <- !g & con_dv & grepl(RUT_CUERPO_DV, s, perl = TRUE)
  b <- rep("", n); dvv <- rep("", n)
  b[g] <- sub(RUT_GUION, "\\1", s[g], perl = TRUE); dvv[g] <- sub(RUT_GUION, "\\2", s[g], perl = TRUE)
  b[cd] <- sub(RUT_CUERPO_DV, "\\1", s[cd], perl = TRUE); dvv[cd] <- sub(RUT_CUERPO_DV, "\\2", s[cd], perl = TRUE)
  con <- g | cd
  b0 <- sub("^0+", "", b)
  ok <- con & b0 != ""
  estado[ok] <- ifelse(dv_modulo11(b[ok]) == dvv[ok], "dv_valido", "dv_invalido")
  cuerpo[ok] <- b0[ok]
  sd <- !con & formato == "cuerpo" & grepl(RUT_CUERPO, s, perl = TRUE) & sub("^0+", "", s) != ""
  estado[sd] <- "sin_dv"; cuerpo[sd] <- sub("^0+", "", s[sd])
  estado[v == ""] <- "vacio"; cuerpo[v == ""] <- ""
  list(estado = estado, cuerpo = cuerpo)
}

armonizar <- function(anio, ruta) {
  tb <- leer_tabla(ruta); m <- tb$m; enc <- tb$enc; fe <- tb$fila_enc
  idx <- roles(enc)
  clave <- paste(enc[enc != ""], collapse = "|")
  if (!clave %in% VARIANTES) VARIANTES <<- c(VARIANTES, clave)
  variante <- match(clave, VARIANTES)
  js <- which(enc != "")
  rol_de <- function(j) { r <- names(idx)[which(idx == j)[1]]; if (is.na(r)) "" else r }
  roles_j <- vapply(js, rol_de, "")
  dicc <- data.frame(anio_posicion = sprintf("%d-%02d", anio, js), anio = anio, variante = variante, posicion = js,
                     encabezado = m[fe, js], normalizado = enc[js], rol = roles_j,
                     tipo = ifelse(roles_j == "", "", unname(TIPOS[roles_j])), stringsAsFactors = FALSE)
  dicc$tipo[is.na(dicc$tipo)] <- ""

  cuerpo <- if (fe < nrow(m)) m[(fe + 1):nrow(m), , drop = FALSE] else m[0, , drop = FALSE]
  fila <- if (fe < nrow(m)) (fe + 1):nrow(m) else integer(0)
  no_vacia <- rowSums(cuerpo != "") > 0
  cuerpo <- cuerpo[no_vacia, , drop = FALSE]; fila <- fila[no_vacia]
  n <- nrow(cuerpo)
  col <- function(r) if (idx[[r]] == 0) rep("", n) else cuerpo[, idx[[r]]]

  t <- a_num(col("mtotal")); fm <- a_num(col("fm")); mi <- a_num(col("minst"))
  val <- !is.na(t) & !is.na(fm) & !is.na(mi)
  validas <- sum(val); iguales <- sum(abs(t[val] - (fm[val] + mi[val])) <= TOL_SUMA)
  apto <- validas > 0 && iguales >= UMBRAL_R1 * validas
  total <- t; origen <- ifelse(is.na(t), "", "numerico")
  rec <- is.na(t) & !is.na(fm) & !is.na(mi) & apto
  total[rec] <- fm[rec] + mi[rec]; origen[rec] <- "recuperado"

  fch <- leer_fechas(col("fecha"))
  a_col <- a_num(col("anio"))
  a_ok <- !is.na(a_col) & a_col == floor(a_col) & a_col >= 2008 & a_col <= 2025
  con_fecha <- !is.na(fch$fecha)
  anio_f <- as.integer(format(fch$fecha, "%Y"))
  otro_anio <- con_fecha & anio_f != anio
  anio_don <- rep(anio, n)
  mes <- ifelse(con_fecha & !otro_anio, as.character(as.integer(format(fch$fecha, "%m"))), "")
  motivo <- ifelse(is.na(total), "monto_no_recuperable", "")
  dup <- if (n == 0) logical(0) else duplicated(do.call(paste, c(lapply(seq_len(ncol(cuerpo)), function(j) cuerpo[, j]), sep = "\x1f")))
  fv <- col("fm")
  fm_tipo <- ifelse(fv == "", "vacio", ifelse(!is.na(fm), "monto", ifelse(norm(fv) %in% c("SI", "NO"), "si_no", "otro")))
  if (idx[["rut_donante"]] > 0) {
    fr <- formato_rut(col("rut_donante")); rut <- leer_ruts(col("rut_donante"), fr$formato)
  } else {
    fr <- list(formato = "", n = 0L, ok = 0L); rut <- list(estado = rep("sin_columna", n), cuerpo = rep("", n))
  }
  fffd <- grepl("\uFFFD", col("donante"), fixed = TRUE)

  filas <- data.frame(
    anio_archivo = rep(anio, n), fila = fila, incluida = as.integer(motivo == ""), motivo_exclusion = motivo,
    anio_columna = col("anio"), anio_donacion = as.character(anio_don), mes_donacion = mes,
    fecha_donacion = ifelse(con_fecha, format(fch$fecha, "%Y-%m-%d"), ""), fecha_tipo = fch$tipo,
    anio_fecha = ifelse(con_fecha, as.character(anio_f), ""), fecha_otro_anio = as.integer(otro_anio),
    monto_total = g17(total), monto_total_origen = origen, monto_institucion = g17(mi), monto_fm = g17(fm),
    fm_tipo = fm_tipo, fm_si_no = ifelse(fm_tipo == "si_no", norm(fv), ""), fm_33_medible = as.integer(fm_tipo == "monto"),
    duplicado_exacto = as.integer(dup), donatario = col("donatario"), rut_institucion = col("rut_institucion"),
    proyecto = col("proyecto"), folio_proyecto = col("folio_proyecto"), folio_institucion = col("folio_institucion"),
    region = col("region"), donante = col("donante"), donante_con_fffd = as.integer(fffd),
    rut_donante = col("rut_donante"), rut_cuerpo = rut$cuerpo, rut_estado = rut$estado, stringsAsFactors = FALSE
  )[, CAMPOS_FILA]

  inc <- motivo == ""
  cuenta <- function(x) sum(x, na.rm = TRUE)
  conteo <- data.frame(
    anio = anio, variante = variante, n_filas_hoja = n, n_incluidas = cuenta(inc),
    n_excluidas_monto = cuenta(motivo == "monto_no_recuperable"), n_excluidas_sin_anio = cuenta(motivo == "sin_anio"),
    n_total_numerico = cuenta(origen == "numerico"), n_total_recuperado = cuenta(origen == "recuperado"),
    n_validas_r1 = validas, n_validas_r1_suma_igual = iguales, anio_apto_r1 = tolower(apto),
    n_total_cero = cuenta(inc & total == 0), n_total_negativo = cuenta(inc & total < 0),
    n_fm_monto = cuenta(inc & fm_tipo == "monto"), n_fm_si_no = cuenta(inc & fm_tipo == "si_no"),
    n_fm_vacio = cuenta(inc & fm_tipo == "vacio"), n_fm_otro = cuenta(inc & fm_tipo == "otro"),
    n_fecha_vacia = cuenta(fch$tipo == "vacia"), n_fecha_serial = cuenta(fch$tipo == "serial"),
    n_fecha_texto_iso = cuenta(fch$tipo == "texto_iso"), n_fecha_texto_dma = cuenta(fch$tipo == "texto_dma"),
    n_fecha_texto_largo = cuenta(fch$tipo == "texto_largo"), n_fecha_no_interpretable = cuenta(fch$tipo == "no_interpretable"),
    n_fecha_invalida = cuenta(fch$tipo == "invalida"), n_fecha_fuera_de_rango = cuenta(fch$tipo == "fuera_de_rango"),
    n_fecha_valida = cuenta(con_fecha), n_mes_desconocido = cuenta(mes == ""), n_fecha_otro_anio = cuenta(otro_anio),
    n_anio_columna_no_valido = cuenta(!a_ok), n_anio_columna_distinto_archivo = cuenta(a_ok & a_col != anio),
    n_duplicados_exactos = cuenta(dup), n_donante_con_fffd = cuenta(fffd), formato_rut_sin_guion = fr$formato,
    n_rut_sin_guion = fr$n, n_rut_sin_guion_dv_ok = fr$ok, n_rut_vacio = cuenta(rut$estado == "vacio"),
    n_rut_dv_valido = cuenta(rut$estado == "dv_valido"), n_rut_dv_invalido = cuenta(rut$estado == "dv_invalido"),
    n_rut_sin_dv = cuenta(rut$estado == "sin_dv"), n_rut_mal_formado = cuenta(rut$estado == "mal_formado"),
    n_rut_sin_columna = cuenta(rut$estado == "sin_columna"),
    suma_monto_total = g17(sum(total[inc])), suma_monto_total_sin_duplicados = g17(sum(total[inc & !dup])),
    stringsAsFactors = FALSE
  )[, CAMPOS_CONTEO]
  ad <- anio_don[inc]
  au <- sort(unique(ad))
  anios <- data.frame(par_anios = sprintf("%d-%d", anio, au), anio_archivo = rep(anio, length(au)), anio_donacion = au,
                      n_filas = vapply(au, function(a) sum(ad == a), integer(1)),
                      suma_monto_total = vapply(au, function(a) g17(sum(total[inc][ad == a])), ""), stringsAsFactors = FALSE)
  list(filas = filas, conteo = conteo, anios = anios, dicc = dicc)
}

args <- commandArgs(trailingOnly = TRUE)
carpeta <- args[1]; salida_filas <- args[2]
dir.create(salida_filas, recursive = TRUE, showWarnings = FALSE)
anios <- sort(as.integer(sub("\\.xlsx$", "", list.files(carpeta, pattern = "^[0-9]{4}\\.xlsx$"))))
conteos <- list(); por_anio <- list(); diccs <- list()
for (a in anios) {
  r <- armonizar(a, file.path(carpeta, paste0(a, ".xlsx")))
  write.csv(r$filas, file.path(salida_filas, paste0(a, ".csv")), row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  conteos[[length(conteos) + 1]] <- r$conteo; por_anio[[length(por_anio) + 1]] <- r$anios; diccs[[length(diccs) + 1]] <- r$dicc
  rm(r); invisible(gc())
  message("armonizado ", a)
}
write.csv(do.call(rbind, conteos), args[3], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
write.csv(do.call(rbind, por_anio), args[4], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
write.csv(do.call(rbind, diccs), args[5], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
