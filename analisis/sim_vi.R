# Simulación de variables instrumentales con efectos fijos de año, con efecto conocido (BLUEPRINT.md#p-05).
# Contraparte de sim_vi.py: mismo caso, estimadores, tolerancias (DECISIONES.md#d-18) y orden de
# extracciones del generador de azar.R.
#
# Uso: Rscript sim_vi.R precio_fiscal_por_caso.csv rut_completado_por_anio.csv sim_hallazgos.csv salida.csv

este <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
source(file.path(dirname(normalizePath(este)), "azar.R"))
source(file.path(dirname(normalizePath(este)), "sim_comun.R"))

SEMILLA <- 20261029
N <- 5000
ANIOS <- c(2013, 2014, 2015, 2016, 2017, 2019, 2020, 2023, 2025)
CASOS_A <- c("2013" = "e2013_hasta_1000", "2014" = "e2014_hasta_1000", "2015" = "e2015_hasta_1000",
             "2016" = "e2016_hasta_1000", "2017" = "e2017_14A_hasta_1000", "2019" = "e2019_14A_hasta_1000",
             "2020" = "e2020_14A_hasta_1000", "2023" = "e2023_14A_hasta_1000", "2025" = "e2025_14A_hasta_1000")
CASOS_B <- CASOS_A
CASOS_B[c("2017", "2019", "2020", "2023", "2025")] <- c("e2017_14B_hasta_1000", "e2019_14B_hasta_1000",
                                                        "e2020_pro_pyme", "e2023_pro_pyme", "e2025_pro_pyme")
CASO_PERDIDA <- "e2019_perdida"
CASO_TASA_27 <- "e2025_14A_hasta_1000"
EPSILON <- 1.0
PENDIENTE_LAMBDA <- 0.03
SIGMA_U <- 0.5
RHO <- 0.6
CORTE_PERDIDA <- -1.2816
TOL <- 0.15
MIN_F <- 10.0
TOL_RESIDUO <- 1e-10

mc2e <- function(y, x, z, W, n_unidades) {
  Z <- if (is.null(W)) cbind(z) else cbind(z, W)
  g <- mco(x, Z)
  xhat <- drop(Z %*% g)
  b <- mco(y, if (is.null(W)) cbind(xhat) else cbind(xhat, W))
  r1 <- x - xhat
  s2 <- sum(r1 * r1) / (nrow(Z) - ncol(Z) - n_unidades)
  v <- s2 * solve(crossprod(Z))[1, 1]
  c(b = b[[1]], F = g[[1]]^2 / v)
}

residuo_maximo <- function(z, W) {
  if (is.null(W)) return(max(abs(z)))
  max(abs(z - drop(W %*% mco(z, W))))
}

estimar <- function(reg, nombre, unidad, anio, y, ln_pef, ln_pleg, ln_pa, ln_p27) {
  yd <- dentro(y, unidad)
  xd <- dentro(ln_pef, unidad)
  W <- dentro(sapply(ANIOS[-1], function(a) as.numeric(anio == a)), unidad)
  g <- length(unique(unidad))
  e_mco <- -mco(yd, cbind(xd, W))[[1]]
  reg_regla(reg, sprintf("%s:mco_sesgo", nombre), e_mco - EPSILON, TOL, ">=objetivo", e_mco - EPSILON >= TOL)
  r <- mc2e(yd, xd, dentro(ln_pleg, unidad), W, g)
  reg_regla(reg, sprintf("%s:mc2e:epsilon", nombre), -r[["b"]], EPSILON, "abs<=0.15", abs(-r[["b"]] - EPSILON) <= TOL)
  reg_regla(reg, sprintf("%s:mc2e:F", nombre), r[["F"]], MIN_F, ">=objetivo", r[["F"]] >= MIN_F)
  za <- dentro(ln_pa, unidad)
  z27 <- dentro(ln_p27, unidad)
  ra <- residuo_maximo(za, W)
  r27 <- residuo_maximo(z27, W)
  reg_regla(reg, sprintf("%s:tasa_por_anio:residuo", nombre), ra, TOL_RESIDUO, "<=objetivo", ra <= TOL_RESIDUO)
  reg_regla(reg, sprintf("%s:tasa_27:residuo", nombre), r27, TOL_RESIDUO, "<=objetivo", r27 <= TOL_RESIDUO)
  r4 <- mc2e(yd, xd, za, NULL, g)
  reg_regla(reg, sprintf("%s:tasa_por_anio_sin_ef_anio:sesgo", nombre), abs(-r4[["b"]] - EPSILON), TOL,
            ">=objetivo", abs(-r4[["b"]] - EPSILON) >= TOL)
  c(e_mco, -r[["b"]], -r4[["b"]])
}

args <- commandArgs(trailingOnly = TRUE)
azar_iniciar(SEMILLA)
pr <- read.csv(args[1], colClasses = "character", fileEncoding = "UTF-8")
pr <- setNames(as.numeric(pr$precio), pr$caso)
e <- proporcion_error(args[2])
nt <- length(ANIOS)
alfa <- normales(N)
tipo_b <- uniformes(N) < 0.5
eta <- normales(N * nt)
ruido <- normales(N * nt)
u_marca <- uniformes(N * nt)

donante <- rep(0:(N - 1), each = nt)
anio <- rep(ANIOS, times = N)
ln_pa <- log(pr[CASOS_A[as.character(anio)]])
ln_pb <- log(pr[CASOS_B[as.character(anio)]])
ln_pleg <- unname(ifelse(tipo_b[donante + 1], ln_pb, ln_pa))
ln_pa <- unname(ln_pa)
perdida <- eta < CORTE_PERDIDA
ln_pef <- ifelse(perdida, log(pr[[CASO_PERDIDA]]), ln_pleg)
u <- SIGMA_U * (RHO * eta + sqrt(1 - RHO^2) * ruido)
y <- alfa[donante + 1] + PENDIENTE_LAMBDA * (anio - 2013) - EPSILON * ln_pef + u
ln_p27 <- rep(log(pr[[CASO_TASA_27]]), N * nt)
marca <- u_marca < e[as.character(anio)]

reg <- registro()
reg_dato(reg, "datos:proporcion_perdida", mean(perdida))
reg_dato(reg, "datos:filas_marcadas", sum(marca))
r_sin <- estimar(reg, "sin_error_rut", donante, anio, y, ln_pef, ln_pleg, ln_pa, ln_p27)
r_con <- estimar(reg, "error_rut", 2 * donante + as.integer(marca), anio, y, ln_pef, ln_pleg, ln_pa, ln_p27)

cat(sprintf("VI (R): sin error MCO %.4f, MC2E %.4f, sin EF de año %.4f; con error MCO %.4f, MC2E %.4f; %d cifras, %d fuera de tolerancia, registradas como hallazgo\n",
            r_sin[1], r_sin[2], r_sin[3], r_con[1], r_con[2], length(reg$filas), length(reg_fallas(reg))))
reg_terminar(reg, args[4], args[3], "vi", "d-18")
