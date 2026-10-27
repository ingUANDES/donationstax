# Utilidades comunes de las simulaciones de BLUEPRINT.md#p-05 (sim_bunching, sim_eventos y sim_vi).
# Contraparte de sim_comun.py: registro de cifras (clave, valor, objetivo, regla, cumple), desviaciones de
# la media de cada unidad, mínimos cuadrados y proporción de filas sin RUT por año.

proporcion_error <- function(ruta) {
  d <- read.csv(ruta, colClasses = "character", fileEncoding = "UTF-8")
  setNames(as.numeric(d$n_error_medicion) / as.numeric(d$n_filas), d$anio)
}

dentro <- function(M, unidad) {
  g <- factor(unidad)
  i <- as.integer(g)
  cuenta <- tabulate(i, nbins = nlevels(g))
  if (is.null(dim(M))) return(M - (rowsum(M, g, reorder = TRUE)[, 1] / cuenta)[i])
  M - (rowsum(M, g, reorder = TRUE) / cuenta)[i, , drop = FALSE]
}

mco <- function(y, X) qr.coef(qr(X), y)

registro <- function() {
  r <- new.env()
  r$filas <- list()
  r
}

reg_dato <- function(r, clave, valor) {
  r$filas[[length(r$filas) + 1]] <- list(clave = clave, valor = as.numeric(valor), objetivo = NA_real_,
                                         regla = "", cumple = "")
}

reg_regla <- function(r, clave, valor, objetivo, regla, ok) {
  r$filas[[length(r$filas) + 1]] <- list(clave = clave, valor = as.numeric(valor),
                                         objetivo = if (is.null(objetivo)) NA_real_ else as.numeric(objetivo),
                                         regla = regla, cumple = if (isTRUE(ok)) "sí" else "no")
}

reg_escribir <- function(r, ruta) {
  d <- do.call(rbind, lapply(r$filas, as.data.frame, stringsAsFactors = FALSE))
  # write.csv escribe hasta 15 cifras significativas; contraste.py compara con la tolerancia de D-15
  write.csv(d, ruta, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

reg_fallas <- function(r) {
  vapply(Filter(function(f) f$cumple == "no", r$filas), function(f) f$clave, character(1))
}

# Escribe la salida y se detiene salvo que las reglas incumplidas sean exactamente las registradas como
# hallazgo para esa simulación en analisis/fixtures/sim_hallazgos.csv (como Registro.terminar de Python).
reg_terminar <- function(r, ruta, ruta_hallazgos, simulacion, decision) {
  reg_escribir(r, ruta)
  h <- read.csv(ruta_hallazgos, colClasses = "character", fileEncoding = "UTF-8")
  esperadas <- h$clave[h$simulacion == simulacion]
  fallas <- reg_fallas(r)
  nuevas <- sort(setdiff(fallas, esperadas))
  resueltas <- sort(setdiff(esperadas, fallas))
  if (length(nuevas) > 0) {
    stop(sprintf("fuera de la tolerancia de DECISIONES.md#%s y sin registrar: %s", decision,
                 paste(nuevas, collapse = ", ")), call. = FALSE)
  }
  if (length(resueltas) > 0) {
    stop(sprintf("hallazgos registrados que ya no fallan (revisar %s): %s", ruta_hallazgos,
                 paste(resueltas, collapse = ", ")), call. = FALSE)
  }
  invisible(length(fallas))
}
