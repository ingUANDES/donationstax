# Simulación de bunching en el quiebre de 1.000 UTM, con efecto conocido (BLUEPRINT.md#p-05).
# Contraparte de sim_bunching.py: mismo caso, estimador, tolerancias (DECISIONES.md#d-16) y orden de
# extracciones del generador de azar.R.
#
# Uso: Rscript sim_bunching.R precio_fiscal_por_caso.csv sim_hallazgos.csv salida.csv

este <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
source(file.path(dirname(normalizePath(este)), "azar.R"))
source(file.path(dirname(normalizePath(este)), "sim_comun.R"))

SEMILLA <- 20261027
N <- 300000
MU_DESDE_K <- -0.5
SIGMA <- 1.0
PROP_FONDO <- 0.20
EPSILON <- 0.5
ANCHO_AGOLPE <- 0.04
W <- 0.02
NB <- 100
CENTRO <- 50
Q_PRINCIPAL <- 5
VENTANA_PRINCIPAL <- c(-4, 0)
ORDENES <- 3:6
INFERIORES <- c(-3, -4, -5)
SUPERIORES <- c(0, 1)
REPLICAS <- 200
CASO_P0 <- "e2012_hasta_1000"
CASO_P1 <- "e2012_exceso_sin_fondo"

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
  list(B = b, epsilon = 2 * W * b / ((h_izq + h_der) * ln_razon), h_izq = h_izq, h_der = h_der,
       ajuste = ajuste, resid = cc - ajuste)
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

args <- commandArgs(trailingOnly = TRUE)
azar_iniciar(SEMILLA)
pr <- read.csv(args[1], colClasses = "character", fileEncoding = "UTF-8")
pr <- setNames(as.numeric(pr$precio), pr$caso)
p0 <- pr[[CASO_P0]]
p1 <- pr[[CASO_P1]]
ln_razon <- log(p1 / p0)
k <- log(1000)
delta <- EPSILON * ln_razon
zs <- k + MU_DESDE_K + SIGMA * normales(N)
fondo <- uniformes(N) < PROP_FONDO
pos <- uniformes(N)
agolpa <- !fondo & zs > k & zs <= k + delta
z <- ifelse(fondo | zs <= k, zs, ifelse(agolpa, k - ANCHO_AGOLPE * pos, zs - delta))
b_sim <- sum(agolpa)
c_sin <- conteos(z[!fondo], k)
c_todos <- conteos(z, k)

reg <- registro()
reg_dato(reg, "datos:P0", p0)
reg_dato(reg, "datos:P1", p1)
reg_dato(reg, "datos:n", N)
reg_dato(reg, "datos:n_fondo", sum(fondo))
reg_dato(reg, "datos:B_simulado", b_sim)
reg_dato(reg, "datos:conteo_sin_fondo_tramo_49", c_sin[CENTRO])

lo <- VENTANA_PRINCIPAL[1]
hi <- VENTANA_PRINCIPAL[2]
r <- estimar(c_sin, Q_PRINCIPAL, lo, hi, ln_razon)
ee <- error_estandar(c_sin, Q_PRINCIPAL, lo, hi, ln_razon)
reg_regla(reg, "principal:sin_fondo:epsilon", r$epsilon, EPSILON, "rel<=0.10",
          abs(r$epsilon - EPSILON) <= 0.10 * EPSILON)
reg_regla(reg, "principal:sin_fondo:B", r$B, b_sim, "rel<=0.10", abs(r$B - b_sim) <= 0.10 * b_sim)
reg_dato(reg, "principal:sin_fondo:h_izq", r$h_izq)
reg_dato(reg, "principal:sin_fondo:h_der", r$h_der)
reg_regla(reg, "principal:sin_fondo:error_estandar", ee, NULL, "finito>0", is.finite(ee) && ee > 0)

for (q in ORDENES) for (lo_s in INFERIORES) for (hi_s in SUPERIORES) {
  e <- estimar(c_sin, q, lo_s, hi_s, ln_razon)$epsilon
  clave <- sprintf("sensibilidad:q%d:inf%+.2f:sup%+.2f:epsilon", q, lo_s * W, hi_s * W)
  reg_regla(reg, clave, e, EPSILON, "rel<=0.15", abs(e - EPSILON) <= 0.15 * EPSILON)
}

t <- estimar(c_todos, Q_PRINCIPAL, lo, hi, ln_razon)
atenuado <- (1 - PROP_FONDO) * EPSILON
reg_regla(reg, "principal:todos:B", t$B, b_sim, "rel<=0.10", abs(t$B - b_sim) <= 0.10 * b_sim)
reg_regla(reg, "principal:todos:epsilon_sesgo", t$epsilon, (1 - PROP_FONDO / 2) * EPSILON, "<=objetivo",
          t$epsilon <= (1 - PROP_FONDO / 2) * EPSILON)
reg_regla(reg, "principal:todos:epsilon_atenuado", t$epsilon, atenuado, "rel<=0.15",
          abs(t$epsilon - atenuado) <= 0.15 * atenuado)

cat(sprintf("bunching (R): B simulado %d, epsilon principal %.4f (ee %.4f), con todos %.4f; %d cifras, %d fuera de tolerancia, registradas como hallazgo\n",
            b_sim, r$epsilon, ee, t$epsilon, length(reg$filas), length(reg_fallas(reg))))
reg_terminar(reg, args[3], args[2], "bunching", "d-16")
