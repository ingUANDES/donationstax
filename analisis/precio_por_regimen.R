# Precio fiscal de donar por régimen y año comercial (P-04 del BLUEPRINT).
# Contraparte de precio_por_regimen.py: mismas entradas, mismas reglas y mismas comprobaciones (las
# reglas para c, g y tau están descritas en la cabecera del de Python). Usa precio_fiscal() de
# precio_fiscal.R. contraste.py compara las dos salidas.
#
# Uso: Rscript precio_por_regimen.R cambios.csv casos.csv comparaciones.csv salida_casos.csv salida_cambios.csv

este <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
source(file.path(dirname(normalizePath(este)), "precio_fiscal.R"))

TOL <- 1e-12
CON_EFECTO <- c("abarata", "encarece", "ambiguo", "base")

falla <- function(...) stop(paste0(...), call. = FALSE)
afirma <- function(cond, ...) if (!isTRUE(cond)) falla(...)

leer <- function(ruta) {
  read.csv(ruta, colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8",
           na.strings = character(0))
}

clave <- function(d) paste(d$norma, d$articulo_inciso, d$vigencia_desde, sep = "|")

parametros <- function(texto) {
  out <- list()
  for (p in strsplit(texto, ";", fixed = TRUE)[[1]]) {
    if (grepl("=", p, fixed = TRUE)) {
      out[[trimws(sub("=.*$", "", p))]] <- trimws(sub("^[^=]*=", "", p))
    }
  }
  out
}

args <- commandArgs(trailingOnly = TRUE)
norma <- leer(args[1])
for (col in names(norma)) {
  afirma(!any(grepl("[\r\n]", norma[[col]])), "cambios.csv: salto de línea dentro de un campo")
}
claves <- clave(norma)
params <- lapply(norma$parametros, parametros)
usos <- replicate(nrow(norma), character(0), simplify = FALSE)

buscar <- function(ref, caso) {
  idx <- which(claves == ref)
  afirma(length(idx) > 0, caso, ": la referencia '", ref, "' no está en cambios.csv")
  afirma(all(norma$estado_verificacion[idx] == "verificada"), caso, ": '", ref, "' no está verificada")
  idx
}

citar <- function(ref, caso) {
  for (i in buscar(ref, caso)) usos[[i]] <<- union(usos[[i]], caso)
}

valor <- function(ref, regla, caso) {
  idx <- buscar(ref, caso)
  if (regla %in% c("0", "1")) {
    for (i in idx) usos[[i]] <<- union(usos[[i]], caso)
    return(as.numeric(regla))
  }
  m <- regmatches(regla, regexec("^([A-Za-z0-9_]+)(\\[([0-9]+)\\])?(%)?$", regla))[[1]]
  afirma(length(m) > 0, caso, ": regla '", regla, "' no reconocida")
  nombre <- m[2]; pos <- m[4]; pct <- m[5] == "%"
  con <- idx[vapply(idx, function(i) nombre %in% names(params[[i]]), logical(1))]
  afirma(length(con) == 1, caso, ": '", ref, "' tiene ", length(con), " filas con el parámetro '", nombre, "'")
  i <- con[1]
  usos[[i]] <<- union(usos[[i]], caso)
  v <- params[[i]][[nombre]]
  if (pos != "") v <- strsplit(v, "/", fixed = TRUE)[[1]][as.integer(pos)]
  n <- regmatches(v, regexpr("^-?[0-9]+(\\.[0-9]+)?", v))
  afirma(length(n) == 1, caso, ": el parámetro '", nombre, "' = '", v, "' no es numérico")
  x <- as.numeric(n)
  if (pct) {
    x <- x / 100
  } else {
    afirma(n == v, caso, ": '", nombre, "' = '", v, "' trae texto; use la regla con %")
  }
  x
}

casos <- leer(args[2])
afirma(!anyDuplicated(casos$caso), "caso repetido: ", casos$caso[anyDuplicated(casos$caso)])
cv <- gv <- tv <- pv <- numeric(nrow(casos))
for (j in seq_len(nrow(casos))) {
  k <- casos[j, ]
  caso <- k$caso
  cv[j] <- valor(k$c_fuente, k$c, caso)
  if (k$g == "1-c") {
    citar(k$g_fuente, caso)
    gv[j] <- 1 - cv[j]
  } else {
    gv[j] <- valor(k$g_fuente, k$g, caso)
  }
  if (k$tau == "na") {
    afirma(gv[j] == 0, caso, ": tau = na exige g = 0")
    afirma(k$tau_fuente == "", caso, ": tau = na no lleva fuente")
    tv[j] <- NA
    tau <- 0
  } else {
    tv[j] <- valor(k$tau_fuente, k$tau, caso)
    tau <- tv[j]
  }
  for (ref in strsplit(k$otras_fuentes, "||", fixed = TRUE)[[1]]) if (ref != "") citar(ref, caso)
  pv[j] <- precio_fiscal(cv[j], gv[j], tau)
  afirma(pv[j] >= -TOL && pv[j] <= 1 + TOL, caso, ": precio ", pv[j], " fuera de [0, 1]")
}
precios <- setNames(pv, casos$caso)
tabla <- data.frame(caso = casos$caso, anio_comercial = casos$anio_comercial,
                    contribuyente = casos$contribuyente, regimen = casos$regimen,
                    c = cv, g = gv, tau = tv, precio = pv, subsidio = 1 - pv,
                    stringsAsFactors = FALSE)

comp <- leer(args[3])
fila_de <- integer(nrow(comp))
for (j in seq_len(nrow(comp))) {
  partes <- strsplit(comp$fila_evaluada[j], "#", fixed = TRUE)[[1]]
  idx <- buscar(partes[1], "comparación")
  if (length(partes) > 1) idx <- idx[vapply(idx, function(i) partes[2] %in% names(params[[i]]), logical(1))]
  afirma(length(idx) == 1, "comparación: '", comp$fila_evaluada[j], "' no identifica una sola fila")
  for (caso in c(comp$caso_antes[j], comp$caso_despues[j])) {
    afirma(caso %in% names(precios), "comparación: el caso '", caso, "' no existe")
  }
  fila_de[j] <- idx
}
llaves <- paste(fila_de, comp$caso_antes, comp$caso_despues)
afirma(!anyDuplicated(llaves), "comparación repetida: ", llaves[anyDuplicated(llaves)])

efecto <- function(d) if (d < -TOL) "abarata" else if (d > TOL) "encarece" else "neutro"

salida <- list()
for (i in seq_len(nrow(norma))) {
  base <- list(fila = i + 1, norma = norma$norma[i], articulo_inciso = norma$articulo_inciso[i],
               vigencia_desde = norma$vigencia_desde[i], efecto_declarado = norma$efecto_precio[i])
  dec <- norma$efecto_precio[i]
  js <- which(fila_de == i)
  if (length(js) == 0) {
    afirma(norma$estado_verificacion[i] != "verificada" || !(dec %in% CON_EFECTO) || length(usos[[i]]) > 0,
           "cambios.csv:", i + 1, " (", claves[i], ") tiene efecto '", dec, "' y ningún caso la usa")
    salida[[length(salida) + 1]] <- c(base, list(
      comparacion = "", caso_antes = "", caso_despues = "", precio_antes = NA, precio_despues = NA,
      delta = NA, efecto_calculado = "sin comparación", coincide = "", n_casos = length(usos[[i]]), nota = ""))
    next
  }
  for (j in js) {
    pa <- precios[[comp$caso_antes[j]]]
    pd <- precios[[comp$caso_despues[j]]]
    d <- pd - pa
    e <- efecto(d)
    co <- if (dec == "ambiguo") "ambiguo" else if (dec == e) "sí" else "no"
    salida[[length(salida) + 1]] <- c(base, list(
      comparacion = paste0(i + 1, ":", comp$caso_antes[j], ">", comp$caso_despues[j]),
      caso_antes = comp$caso_antes[j], caso_despues = comp$caso_despues[j],
      precio_antes = pa, precio_despues = pd, delta = d, efecto_calculado = e, coincide = co,
      n_casos = length(usos[[i]]), nota = comp$nota[j]))
  }
}
cambios <- do.call(rbind, lapply(salida, function(x) as.data.frame(x, stringsAsFactors = FALSE)))

# write.csv escribe hasta 15 cifras significativas; contraste.py compara con tolerancia
write.csv(tabla, args[4], row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(cambios, args[5], row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(sprintf("precio por régimen (R): %d casos, %d comparaciones, %d filas de cambios.csv, %d comparaciones con signo distinto del declarado\n",
            nrow(tabla), sum(cambios$comparacion != ""), nrow(norma), sum(cambios$coincide == "no")))
