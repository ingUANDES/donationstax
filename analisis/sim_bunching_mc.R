# Monte Carlo del bunching en el quiebre de 1.000 UTM, con regla de selección fijada antes (BLUEPRINT.md#p-05).
# Contraparte de sim_bunching_mc.py: mismos procesos, regla de selección, tolerancias (DECISIONES.md#d-24,
# método en DECISIONES.md#d-19), semillas y orden de extracciones del generador de azar.R. Las funciones de
# tramos, matriz, estimador y bootstrap repiten las de sim_bunching.R (DECISIONES.md#d-16).
#
# Uso: Rscript sim_bunching_mc.R precio_fiscal_por_caso.csv utm_mensual.csv sim_hallazgos.csv corta|larga
#      salida.csv [repeticiones.csv]

este <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
source(file.path(dirname(normalizePath(este)), "azar.R"))
source(file.path(dirname(normalizePath(este)), "sim_comun.R"))

# De sim_bunching.R (DECISIONES.md#d-16)
PROP_FONDO <- 0.20
EPSILON <- 0.5
ANCHO_AGOLPE <- 0.04
W <- 0.02
NB <- 100
CENTRO <- 50
ORDENES <- 3:6
INFERIORES <- c(-3, -4, -5)
SUPERIORES <- c(0, 1)
REPLICAS <- 200
CASO_P0 <- "e2012_hasta_1000"
CASO_P1 <- "e2012_exceso_sin_fondo"

# DECISIONES.md#d-24
SEMILLA_BASE <- 34000000
PROCESOS <- list(list(nombre = "base", n = 300000, mu = -0.5, sigma = 1.0, redondea = FALSE),
                 list(nombre = "curvatura", n = 300000, mu = -0.25, sigma = 0.5, redondea = FALSE),
                 list(nombre = "ruido", n = 75000, mu = -0.5, sigma = 1.0, redondea = FALSE),
                 list(nombre = "redondos", n = 300000, mu = -0.5, sigma = 1.0, redondea = TRUE))
PROP_REDONDEA <- 0.05
MULTIPLO <- 5000000
ANIO_UTM <- 2012
J0 <- CENTRO - 1
X_MAX <- 10
ORDENES_BIC <- 1:7
Z_BANDA <- 1.96
Z_IC <- 1.96
NIVEL <- 0.95

conteos <- function(z, k) {
  j <- floor((z - k) / W) + CENTRO
  j <- j[j >= 0 & j < NB]
  tabulate(j + 1, nbins = NB)
}

matriz <- function(q, lo, hi) {
  j <- 0:(NB - 1)
  s <- (j - CENTRO + 0.5) * W
  izq <- j < CENTRO
  cbind(sapply(0:q, function(p) ifelse(izq, s^p, 0)),
        sapply(0:q, function(p) ifelse(izq, 0, s^p)),
        sapply((CENTRO + lo):(CENTRO + hi - 1), function(jj) as.numeric(j == jj)))
}

estimar <- function(cc, q, lo, hi, ln_razon) {
  X <- matriz(q, lo, hi)
  coef <- mco(cc, X)
  b <- sum(coef[(2 * (q + 1) + 1):ncol(X)])
  h_izq <- coef[[1]]
  h_der <- coef[[q + 2]]
  ajuste <- drop(X %*% coef)
  list(B = b, epsilon = 2 * W * b / ((h_izq + h_der) * ln_razon), ajuste = ajuste, resid = cc - ajuste)
}

error_estandar <- function(cc, q, lo, hi, ln_razon) {
  r <- estimar(cc, q, lo, hi, ln_razon)
  eps <- numeric(REPLICAS)
  for (b in seq_len(REPLICAS)) {
    idx <- floor(uniformes(NB) * NB) + 1
    eps[b] <- estimar(r$ajuste + r$resid[idx], q, lo, hi, ln_razon)$epsilon
  }
  sd(eps)
}

poli <- function(q) {
  j <- 0:(NB - 1)
  s <- (j - CENTRO + 0.5) * W
  izq <- j < CENTRO
  cbind(sapply(0:q, function(p) ifelse(izq, s^p, 0)), sapply(0:q, function(p) ifelse(izq, 0, s^p)))
}
POLI <- lapply(ORDENES_BIC, poli)

elegir_q <- function(cc, filas) {
  n <- sum(filas)
  mejor <- NULL
  for (q in ORDENES_BIC) {
    X <- POLI[[q]][filas, , drop = FALSE]
    coef <- mco(cc[filas], X)
    rss <- sum((cc[filas] - drop(X %*% coef))^2)
    bic <- n * log(rss / n) + 2 * (q + 1) * log(n)
    if (is.null(mejor) || bic < mejor$bic) mejor <- list(bic = bic, q = q, coef = coef, rss = rss)
  }
  list(q = mejor$q, coef = mejor$coef, rss = mejor$rss, n = n)
}

# filas con índice de tramo 0..NB-1 fuera de [a, b]
fuera <- function(a, b) { j <- 0:(NB - 1); j < a | j > b }

ventana_region <- function(cc, xm, xp) {
  filas <- fuera(J0 - xm, J0 + xp)
  e <- elegir_q(cc, filas)
  A <- POLI[[e$q]]
  X <- A[filas, , drop = FALSE]
  s2 <- e$rss / (e$n - 2 * (e$q + 1))
  U <- chol(crossprod(X))
  w <- backsolve(U, t(A), transpose = TRUE)
  banda <- drop(A %*% e$coef) + Z_BANDA * sqrt(s2 * colSums(w * w))
  sobre <- cc > banda
  i0 <- J0 + 1
  if (!sobre[i0]) return(c(J0, J0))
  lo <- i0
  hi <- i0
  while (lo - 1 >= 1 && sobre[lo - 1]) lo <- lo - 1
  while (hi + 1 <= NB && sobre[hi + 1]) hi <- hi + 1
  c(lo - 1, hi - 1)
}

seleccionar <- function(cc) {
  ls <- integer(0)
  us <- integer(0)
  for (xm in 0:X_MAX) for (xp in 0:X_MAX) {
    v <- ventana_region(cc, xm, xp)
    ls <- c(ls, v[1])
    us <- c(us, v[2])
  }
  lo <- as.integer(median(ls))
  hi <- as.integer(median(us))
  c(elegir_q(cc, fuera(lo, hi))$q, lo, hi)
}

simular <- function(n, mu, sigma, redondea, ln_razon, utm12) {
  k <- log(1000)
  delta <- EPSILON * ln_razon
  zs <- k + mu + sigma * normales(n)
  fondo <- uniformes(n) < PROP_FONDO
  pos <- uniformes(n)
  u_mes <- uniformes(n)
  u_red <- uniformes(n)
  agolpa <- !fondo & zs > k & zs <= k + delta
  z <- ifelse(fondo | zs <= k, zs, ifelse(agolpa, k - ANCHO_AGOLPE * pos, zs - delta))
  if (redondea) {
    idx <- which(!agolpa & u_red < PROP_REDONDEA)
    utm <- utm12[floor(12 * u_mes[idx]) + 1]
    pesos <- floor(exp(z[idx]) * utm / MULTIPLO + 0.5) * MULTIPLO
    ok <- pesos > 0
    z[idx[ok]] <- log(pesos[ok] / utm[ok])
  }
  list(z = z, fondo = fondo, b_sim = sum(agolpa))
}

# Proporción de repeticiones con cada orden y con la ventana del agolpe simulado, [k - 0,04, k).
seleccion_frecuencias <- function(reg, clave, q, lo, hi) {
  for (o in ORDENES_BIC) reg_dato(reg, sprintf("%s:q%d:proporcion", clave, o), mean(q == o))
  reg_dato(reg, paste0(clave, ":ventana_simulada:proporcion"), mean(lo == J0 - 1 & hi == J0))
}

args <- commandArgs(trailingOnly = TRUE)
pr <- read.csv(args[1], colClasses = "character", fileEncoding = "UTF-8")
pr <- setNames(as.numeric(pr$precio), pr$caso)
p0 <- pr[[CASO_P0]]
p1 <- pr[[CASO_P1]]
ln_razon <- log(p1 / p0)
k <- log(1000)
u <- read.csv(args[2], colClasses = "character", fileEncoding = "UTF-8")
u <- u[as.integer(u$anio) == ANIO_UTM, ]
utm12 <- as.numeric(u$utm[order(as.integer(u$mes))])
stopifnot(length(utm12) == 12)
lista <- args[4]
sem <- semillas(SEMILLA_BASE, lista)

reg <- registro()
reg_dato(reg, "datos:P0", p0)
reg_dato(reg, "datos:P1", p1)
reg_dato(reg, "datos:repeticiones", length(sem))
reg_dato(reg, "datos:semilla_base", SEMILLA_BASE)
filas_rep <- list()
for (pc in PROCESOS) {
  nr <- length(sem)
  R <- list(B_sim = numeric(nr), q = numeric(nr), l = numeric(nr), u = numeric(nr), eps = numeric(nr),
            B = numeric(nr), ee = numeric(nr), qt = numeric(nr), lt = numeric(nr), ut = numeric(nr),
            eps_t = numeric(nr), B_t = numeric(nr))
  sens <- list()
  for (q in ORDENES) for (lo in INFERIORES) for (hi in SUPERIORES)
    sens[[sprintf("%s:sensibilidad:q%d:inf%+.2f:sup%+.2f:epsilon", pc$nombre, q, lo * W, hi * W)]] <- numeric(nr)
  for (r in seq_len(nr)) {
    azar_iniciar(sem[r])
    s <- simular(pc$n, pc$mu, pc$sigma, pc$redondea, ln_razon, utm12)
    c_sin <- conteos(s$z[!s$fondo], k)
    sel <- seleccionar(c_sin)
    est <- estimar(c_sin, sel[1], sel[2] - CENTRO, sel[3] + 1 - CENTRO, ln_razon)
    ee <- error_estandar(c_sin, sel[1], sel[2] - CENTRO, sel[3] + 1 - CENTRO, ln_razon)
    i <- 0
    for (q in ORDENES) for (lo in INFERIORES) for (hi in SUPERIORES) {
      i <- i + 1
      sens[[i]][r] <- estimar(c_sin, q, lo, hi, ln_razon)$epsilon
    }
    R$B_sim[r] <- s$b_sim; R$q[r] <- sel[1]; R$l[r] <- sel[2]; R$u[r] <- sel[3]
    R$eps[r] <- est$epsilon; R$B[r] <- est$B; R$ee[r] <- ee
    fila <- list(proceso = pc$nombre, r = r - 1, semilla = sem[r], B_simulado = s$b_sim, q = sel[1], l = sel[2],
                 u = sel[3], epsilon = est$epsilon, B = est$B, ee = ee, todos_q = NA, todos_l = NA, todos_u = NA,
                 todos_epsilon = NA, todos_B = NA)
    if (pc$nombre == "base") {
      c_todos <- conteos(s$z, k)
      st <- seleccionar(c_todos)
      t <- estimar(c_todos, st[1], st[2] - CENTRO, st[3] + 1 - CENTRO, ln_razon)
      R$qt[r] <- st[1]; R$lt[r] <- st[2]; R$ut[r] <- st[3]; R$eps_t[r] <- t$epsilon; R$B_t[r] <- t$B
      fila[c("todos_q", "todos_l", "todos_u", "todos_epsilon", "todos_B")] <- list(st[1], st[2], st[3], t$epsilon, t$B)
    }
    filas_rep[[length(filas_rep) + 1]] <- fila
  }
  nm <- pc$nombre
  reg_dato(reg, paste0(nm, ":datos:n"), pc$n)
  reg_dato(reg, paste0(nm, ":datos:B_simulado:media"), mean(R$B_sim))
  reg_dato(reg, paste0(nm, ":seleccion:q:media"), mean(R$q))
  reg_dato(reg, paste0(nm, ":seleccion:inf:media"), mean((R$l - CENTRO) * W))
  reg_dato(reg, paste0(nm, ":seleccion:sup:media"), mean((R$u + 1 - CENTRO) * W))
  seleccion_frecuencias(reg, paste0(nm, ":seleccion"), R$q, R$l, R$u)
  regla_sesgo(reg, paste0(nm, ":epsilon"), R$eps, EPSILON)
  regla_sesgo(reg, paste0(nm, ":B"), R$B, R$B_sim)
  valido <- is.finite(R$ee) & R$ee > 0
  cubre <- valido & abs(R$eps - EPSILON) <= Z_IC * ifelse(valido, R$ee, 0)
  regla_proporcion(reg, paste0(nm, ":epsilon:cobertura"), cubre, NIVEL, "igual")
  reg_dato(reg, paste0(nm, ":epsilon:ee_bootstrap:media"), if (any(valido)) mean(R$ee[valido]) else NaN)
  n_inv <- sum(!valido)
  reg_regla(reg, paste0(nm, ":epsilon:ee_bootstrap:invalidos"), n_inv, 0, "==objetivo", n_inv == 0)
  for (cl in names(sens)) {
    reg_dato(reg, paste0(cl, ":media"), mean(sens[[cl]]))
    reg_dato(reg, paste0(cl, ":ee"), sd(sens[[cl]]))
  }
  if (nm == "base") {
    reg_dato(reg, "base:todos:seleccion:q:media", mean(R$qt))
    reg_dato(reg, "base:todos:seleccion:inf:media", mean((R$lt - CENTRO) * W))
    reg_dato(reg, "base:todos:seleccion:sup:media", mean((R$ut + 1 - CENTRO) * W))
    seleccion_frecuencias(reg, "base:todos:seleccion", R$qt, R$lt, R$ut)
    regla_sesgo(reg, "base:todos:B", R$B_t, R$B_sim)
    regla_deteccion(reg, "base:todos:epsilon:deteccion", R$eps_t, (1 - PROP_FONDO / 2) * EPSILON, "bajo")
    regla_sesgo(reg, "base:todos:epsilon_atenuado", R$eps_t, (1 - PROP_FONDO) * EPSILON)
  }
}

if (length(args) >= 6) {
  d <- do.call(rbind, lapply(filas_rep, as.data.frame, stringsAsFactors = FALSE))
  write.csv(d, args[6], row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
cat(sprintf("bunching_mc (R, lista %s, %d repeticiones): %d cifras, %d reglas incumplidas, registradas como hallazgo\n",
            lista, length(sem), length(reg$filas), length(reg_fallas(reg))))
reg_terminar(reg, args[5], args[3], paste0("bunching_mc_", lista), "d-24")
