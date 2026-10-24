# Cruce por RUT de las donatarias de la Ley 21.440 con las instituciones de las planillas de la
# Ley 19.885. Contraparte en R de cruce_donatarias.py: mismas reglas (descritas en su cabecera),
# lector distinto. contraste_datos.py exige que las dos salidas coincidan.
#
# Uso: Rscript cruce_donatarias.R <carpeta_19885> <carpeta_21440> <salida.csv>
# Conviene correrlo con LC_ALL=C.UTF-8.

aqui_cruce <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(aqui_cruce, "esquema_ley21440.R"))

CLAVE <- "^[0-9]{7,8}-[0-9K]$"
CAMPOS_CRUCE <- c(
  "anio", "n_rut_21440", "n_rut_19885", "n_rut_comun", "n_filas_21440_rut_no_valido",
  "n_filas_19885_rut_no_valido", "suma_valor_21440", "suma_valor_21440_rut_comun", "suma_monto_19885",
  "suma_monto_19885_rut_comun"
)

clave <- function(x) toupper(gsub("[. \t\n\r]", "", x))

cuerpo_de <- function(tb) {
  m <- tb$m
  cuerpo <- if (tb$fila_enc < nrow(m)) m[(tb$fila_enc + 1):nrow(m), , drop = FALSE] else m[0, , drop = FALSE]
  cuerpo[rowSums(cuerpo != "") > 0, , drop = FALSE]
}

montos21440 <- function(ruta) {
  tb <- leer21440(ruta); cuerpo <- cuerpo_de(tb); idx <- tb$idx
  d <- es_fila_datos21440(cuerpo, idx)
  data.frame(k = clave(cuerpo[d, idx[["rut"]]]), v = a_num(cuerpo[d, idx[["valor"]]]), stringsAsFactors = FALSE)
}

montos19885 <- function(ruta) {
  tb <- leer_tabla(ruta); cuerpo <- cuerpo_de(tb); idx <- roles(tb$enc)
  stopifnot(idx[["rut_institucion"]] > 0)
  d <- clase_fila(cuerpo, idx) == "datos"
  data.frame(k = clave(cuerpo[d, idx[["rut_institucion"]]]), v = a_num(cuerpo[d, idx[["mtotal"]]]), stringsAsFactors = FALSE)
}

fila_cruce <- function(anio, m21, m19) {
  ok21 <- grepl(CLAVE, m21$k); ok19 <- grepl(CLAVE, m19$k)
  r21 <- unique(m21$k[ok21]); r19 <- unique(m19$k[ok19])
  comun <- intersect(r21, r19)
  data.frame(
    anio = as.character(anio), n_rut_21440 = length(r21), n_rut_19885 = length(r19), n_rut_comun = length(comun),
    n_filas_21440_rut_no_valido = sum(!ok21), n_filas_19885_rut_no_valido = sum(!ok19),
    suma_valor_21440 = g17(sum(m21$v)), suma_valor_21440_rut_comun = g17(sum(m21$v[m21$k %in% comun])),
    suma_monto_19885 = g17(sum(m19$v)), suma_monto_19885_rut_comun = g17(sum(m19$v[m19$k %in% comun])),
    stringsAsFactors = FALSE
  )[, CAMPOS_CRUCE]
}

args <- commandArgs(trailingOnly = TRUE)
anios_de <- function(carpeta) as.integer(sub("\\.xlsx$", "", list.files(carpeta, pattern = "^[0-9]{4}\\.xlsx$")))
anios <- sort(intersect(anios_de(args[1]), anios_de(args[2])))
stopifnot(length(anios) > 0)
partes <- lapply(anios, function(a) {
  message("cruce ", a)
  res <- list(m21 = montos21440(file.path(args[2], paste0(a, ".xlsx"))), m19 = montos19885(file.path(args[1], paste0(a, ".xlsx"))))
  invisible(gc())  # libera la planilla del año antes de leer la siguiente (las de 2022, 2023 y 2025 pasan de 7 GB)
  res
})
res <- do.call(rbind, c(
  Map(function(a, p) fila_cruce(a, p$m21, p$m19), anios, partes),
  list(fila_cruce(paste0(anios[1], "-", anios[length(anios)]),
                  do.call(rbind, lapply(partes, `[[`, "m21")), do.call(rbind, lapply(partes, `[[`, "m19"))))
))
write.csv(res, args[3], row.names = FALSE, quote = TRUE)
