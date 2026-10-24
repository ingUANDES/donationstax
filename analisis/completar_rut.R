# Regla 7 de D-10: completa el RUT del donante por nombre en las planillas armonizadas de la Ley 19.885.
# Contraparte en R de completar_rut.py: mismas reglas (descritas en su cabecera); Levenshtein de
# stringdist en vez del de rapidfuzz. contraste_datos.py compara las tablas y contraste_armonizado.py
# las filas.
#
# Uso: Rscript completar_rut.R <carpeta_armonizado> <salida_filas> <calib_identico.csv> <calib_similitud.csv> <por_anio.csv>
# Conviene correrlo con LC_ALL=C.UTF-8.

suppressPackageStartupMessages(library(stringdist))

DE <- "ÁÀÂÄÃÅáàâäãåÉÈÊËéèêëÍÌÎÏíìîïÓÒÔÖÕóòôöõÚÙÛÜúùûüÑñÇçÝýÿ"
A <- "AAAAAAAAAAAAEEEEEEEEIIIIIIIIOOOOOOOOOOUUUUUUUUNNCCYYY"
USABLE <- c("dv_valido", "sin_dv")
UMBRALES <- 80:99
MIN_ASIGNADOS <- 100
HILOS <- max(1L, parallel::detectCores())

normalizar <- function(x) {
  x <- chartr(paste0(DE, "abcdefghijklmnopqrstuvwxyz\uFFFD"), paste0(A, "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"), x)
  gsub("^ +| +$", "", gsub("[^A-Z0-9?]+", " ", x, perl = TRUE), perl = TRUE)
}
precision <- function(c, a) ifelse(a > 0, sprintf("%.6f", c / a), "")
expresion <- function(q, simetrica) {
  ch <- strsplit(q, "", fixed = TRUE)[[1]]
  paste0("^", paste0(ifelse(ch == "?", ".", if (simetrica) paste0("[", ch, "?]") else ch), collapse = ""), "$")
}

leer <- function(carpeta) {
  arch <- sort(list.files(carpeta, pattern = "^[0-9]{4}\\.csv$"))
  enc <- names(read.csv(file.path(carpeta, arch[1]), nrows = 1, check.names = FALSE))
  clases <- ifelse(enc %in% c("fila", "donante", "rut_estado", "rut_cuerpo"), "character", "NULL")
  res <- list()
  for (f in arch) {
    d <- read.csv(file.path(carpeta, f), colClasses = clases, encoding = "UTF-8", na.strings = character(0),
                  check.names = FALSE)
    u <- unique(d$donante)
    d$n <- normalizar(u)[match(d$donante, u)]
    d$usable <- d$rut_estado %in% USABLE
    res[[sub("\\.csv$", "", f)]] <- d[, c("fila", "n", "usable", "rut_cuerpo")]
    message("leído ", f)
  }
  res
}

# Diccionario: claves (nombres), cuerpos por clave y bloques de 3 caracteres.
diccionario <- function(n, c) {
  p <- unique(data.frame(n = n, c = c, stringsAsFactors = FALSE))
  p <- p[order(p$n, method = "radix"), ]
  claves <- unique(p$n)
  list(claves = claves, cuerpos = split(p$c, factor(p$n, levels = claves)), largo = nchar(claves),
       comodin = grepl("?", claves, fixed = TRUE))
}

# Paso 1: lista de cuerpos compatibles con cada consulta.
identico <- function(dic, q) {
  pares_q <- integer(0); pares_k <- integer(0)
  k <- match(q, dic$claves)
  plana <- !grepl("?", q, fixed = TRUE)
  e <- which(plana & !is.na(k)); pares_q <- c(pares_q, e); pares_k <- c(pares_k, k[e])
  lq <- nchar(q)
  for (j in which(dic$comodin)) {  # claves con «?» contra consultas planas del mismo largo
    cand <- which(plana & lq == dic$largo[j])
    if (length(cand)) {
      h <- cand[grepl(expresion(dic$claves[j], FALSE), q[cand], perl = TRUE)]
      pares_q <- c(pares_q, h); pares_k <- c(pares_k, rep(j, length(h)))
    }
  }
  for (i in which(!plana)) {  # consultas con «?» contra todas las claves del mismo largo
    cand <- which(dic$largo == lq[i])
    h <- cand[grepl(expresion(q[i], TRUE), dic$claves[cand], perl = TRUE)]
    pares_q <- c(pares_q, rep(i, length(h))); pares_k <- c(pares_k, h)
  }
  res <- vector("list", length(q)); res[] <- list(character(0))
  if (length(pares_q)) {
    b <- dic$cuerpos[pares_k]
    u <- unique(data.frame(q = rep(pares_q, lengths(b)), b = unlist(b, use.names = FALSE), stringsAsFactors = FALSE))
    s <- split(u$b, u$q)
    res[as.integer(names(s))] <- s
  }
  res
}

# Paso 2: mejor similitud de cada consulta (sin «?») dentro de su bloque, si s >= UMBRALES[1]/100.
similares <- function(dic, q) {
  plana <- which(!dic$comodin)
  ch_todas <- dic$claves[plana]; bl_ch <- substr(ch_todas, 1, 3)
  num <- rep(NA_integer_, length(q)); den <- rep(NA_integer_, length(q)); cs <- vector("list", length(q))
  bl_q <- substr(q, 1, 3)
  for (k in unique(bl_q)) {
    ch_i <- plana[bl_ch == k]
    if (!length(ch_i)) next
    ch <- dic$claves[ch_i]; lc <- nchar(ch)
    qi <- which(bl_q == k)
    for (trozo in split(qi, ceiling(seq_along(qi) / 1000))) {
      d <- stringdistmatrix(q[trozo], ch, method = "lv", nthread = HILOS)
      for (r in seq_along(trozo)) {
        i <- trozo[r]
        dd <- pmax(lc, nchar(q[i])); nn <- dd - d[r, ]
        ok <- which(100 * nn >= UMBRALES[1] * dd)
        if (!length(ok)) next
        b <- ok[which.max(nn[ok] / dd[ok])]
        emp <- ok[nn[ok] * dd[b] == nn[b] * dd[ok]]
        num[i] <- as.integer(nn[b]); den[i] <- as.integer(dd[b])
        cs[[i]] <- unique(unlist(dic$cuerpos[ch_i[emp]], use.names = FALSE))
      }
    }
  }
  list(num = num, den = den, cuerpos = cs)
}

calibrar <- function(filas) {
  pares <- lapply(filas, function(d) unique(d[d$usable & d$n != "", c("n", "rut_cuerpo")]))
  anios <- names(pares)[vapply(pares, nrow, 0L) > 0]
  ident <- list(); sim <- list(num = integer(0), den = integer(0), ok = logical(0), uno = logical(0)); n_sim <- 0L
  for (t in anios) {
    otros <- do.call(rbind, pares[setdiff(anios, t)])
    dic <- diccionario(otros$n, otros$rut_cuerpo)
    ver <- split(pares[[t]]$rut_cuerpo, pares[[t]]$n)
    q <- sort(names(ver), method = "radix")
    cs <- identico(dic, q)
    nb <- lengths(cs)
    correcto <- nb == 1 & mapply(function(x, v) length(x) == 1 && x %in% v, cs, ver[q])
    ident[[t]] <- data.frame(anio = as.integer(t), n_consultas = length(q), n_asignados = sum(nb == 1),
                             n_correctos = sum(correcto), n_ambiguos = sum(nb > 1), n_sin_candidato = sum(nb == 0),
                             precision = precision(sum(correcto), sum(nb == 1)), stringsAsFactors = FALSE)
    sin <- q[nb == 0 & !grepl("?", q, fixed = TRUE)]
    n_sim <- n_sim + length(sin)
    s <- similares(dic, sin)
    con <- which(!is.na(s$den))
    sim$num <- c(sim$num, s$num[con]); sim$den <- c(sim$den, s$den[con])
    sim$uno <- c(sim$uno, lengths(s$cuerpos[con]) == 1)
    sim$ok <- c(sim$ok, mapply(function(x, v) length(x) == 1 && x %in% v, s$cuerpos[con], ver[sin[con]]))
    message("calibrado ", t)
  }
  list(ident = do.call(rbind, ident), sim = sim, n_sim = n_sim)
}

tabla_similitud <- function(sim, n_sim) {
  elegido <- NA_integer_; filas <- list()
  for (k in UMBRALES) {
    asig <- sim$uno & 100 * sim$num >= k * sim$den
    a <- sum(asig); c <- sum(asig & sim$ok)
    if (is.na(elegido) && a >= MIN_ASIGNADOS && 100 * c >= 99 * a) elegido <- k
    filas[[length(filas) + 1]] <- data.frame(umbral = sprintf("0.%02d", k), n_consultas = n_sim,
                                             n_con_candidato = length(sim$num), n_asignados = a, n_correctos = c,
                                             precision = precision(c, a), elegido = "false", stringsAsFactors = FALSE)
  }
  t <- do.call(rbind, filas)
  if (!is.na(elegido)) t$elegido[UMBRALES == elegido] <- "true"
  list(tabla = t, elegido = elegido)
}

completar <- function(filas, elegido) {
  todo <- do.call(rbind, filas)
  u <- todo[todo$usable & todo$n != "", ]
  dic <- diccionario(u$n, u$rut_cuerpo)
  faltan <- sort(unique(todo$n[!todo$usable & todo$n != ""]), method = "radix")
  cs <- identico(dic, faltan); nb <- lengths(cs)
  fuente <- ifelse(nb == 1, "nombre_identico", ifelse(nb > 1, "ambiguo", "sin_candidato"))
  rut <- ifelse(nb == 1, vapply(cs, function(x) if (length(x) == 1) x else "", ""), "")
  simil <- rep("", length(faltan))
  if (!is.na(elegido)) {
    i <- which(nb == 0 & !grepl("?", faltan, fixed = TRUE))
    s <- similares(dic, faltan[i])
    asig <- !is.na(s$den) & 100 * s$num >= elegido * s$den
    j <- i[asig]; uno <- lengths(s$cuerpos[asig]) == 1
    fuente[j] <- ifelse(uno, "similitud", "ambiguo")
    rut[j] <- ifelse(uno, vapply(s$cuerpos[asig], function(x) if (length(x) == 1) x else "", ""), "")
    simil[j] <- paste0(s$num[asig], "/", s$den[asig])
  }
  list(nombre = faltan, fuente = fuente, rut = rut, simil = simil)
}

args <- commandArgs(trailingOnly = TRUE)
filas <- leer(args[1])
cal <- calibrar(filas)
ts <- tabla_similitud(cal$sim, cal$n_sim)
asg <- completar(filas, ts$elegido)
dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
resumen <- list()
for (a in names(filas)) {
  d <- filas[[a]]; m <- match(d$n, asg$nombre)
  fuente <- ifelse(d$usable, "observado", ifelse(d$n == "", "sin_nombre", asg$fuente[m]))
  rut <- ifelse(d$usable, d$rut_cuerpo, ifelse(d$n == "", "", asg$rut[m]))
  simil <- ifelse(d$usable | d$n == "", "", asg$simil[m])
  id <- ifelse(rut != "", paste0("rut:", rut), ifelse(d$n != "", paste0("nombre:", d$n), ""))
  out <- data.frame(anio_archivo = as.integer(a), fila = d$fila, donante_norm = d$n, rut_fuente = fuente, rut_final = rut,
                    similitud = simil, id_donante = id, error_medicion = as.integer(rut == ""), stringsAsFactors = FALSE)
  write.csv(out, file.path(args[2], paste0(a, ".csv")), row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  cuenta <- function(x) sum(fuente == x)
  resumen[[a]] <- data.frame(anio = as.integer(a), n_filas = nrow(d), n_observado = cuenta("observado"),
                             n_nombre_identico = cuenta("nombre_identico"), n_similitud = cuenta("similitud"),
                             n_ambiguo = cuenta("ambiguo"), n_sin_candidato = cuenta("sin_candidato"),
                             n_sin_nombre = cuenta("sin_nombre"), n_error_medicion = sum(rut == ""),
                             n_donantes_rut = length(unique(id[rut != ""])),
                             n_donantes_nombre = length(unique(id[rut == "" & d$n != ""])), stringsAsFactors = FALSE)
  message("completado ", a)
}
write.csv(cal$ident, args[3], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
write.csv(ts$tabla, args[4], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
write.csv(do.call(rbind, resumen), args[5], row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
