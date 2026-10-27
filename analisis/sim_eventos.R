# Simulación de estudio de eventos y diferencias en diferencias, con efecto conocido (BLUEPRINT.md#p-05).
# Contraparte de sim_eventos.py: mismo caso, estimadores, tolerancias (DECISIONES.md#d-17) y orden de
# extracciones del generador de azar.R.
#
# Uso: Rscript sim_eventos.R rut_completado_por_anio.csv sim_hallazgos.csv salida.csv

este <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
source(file.path(dirname(normalizePath(este)), "azar.R"))
source(file.path(dirname(normalizePath(este)), "sim_comun.R"))

SEMILLA <- 20261028
N <- 40000
ANIOS <- 2012:2019
T0 <- 2015
REF <- 2014
MU_ALFA_DESDE_K <- -1.0
SIGMA_ALFA <- 1.2
SIGMA_V <- 0.6
PENDIENTE_LAMBDA <- 0.02
BETA <- 0.15
BETA_C <- 0.5
TOL <- 0.03
TOL_SEGUIMIENTO <- 0.02
MIN_SESGO <- 0.06

estudio_eventos <- function(unidad, anio, y, T) {
  anios_t <- setdiff(ANIOS, REF)
  X <- cbind(sapply(ANIOS[-1], function(a) as.numeric(anio == a)),
             sapply(anios_t, function(a) T * (anio == a)))
  coef <- mco(dentro(y, unidad), dentro(X, unidad))
  setNames(coef[length(ANIOS):ncol(X)], anios_t)
}

dif_en_dif <- function(unidad, anio, y, T) {
  X <- cbind(sapply(ANIOS[-1], function(a) as.numeric(anio == a)), T * (anio >= T0))
  coef <- mco(dentro(y, unidad), dentro(X, unidad))
  coef[[length(coef)]]
}

sesgo_reversion <- function(mu_alfa, lambda_ref, cc) {
  s <- sqrt(SIGMA_ALFA^2 + SIGMA_V^2)
  a <- (cc - mu_alfa - lambda_ref) / s
  phi <- exp(-a * a / 2) / sqrt(2 * pi)
  Phi <- pnorm(a)
  -(SIGMA_V^2 / s) * phi * (1 / (1 - Phi) + 1 / Phi)
}

comprobar_eventos <- function(reg, nombre, coefs, objetivo) {
  for (a in names(coefs)) {
    o <- objetivo(as.numeric(a))
    reg_regla(reg, sprintf("%s:gamma_%s", nombre, a), coefs[[a]], o, "abs<=0.03", abs(coefs[[a]] - o) <= TOL)
  }
}

args <- commandArgs(trailingOnly = TRUE)
azar_iniciar(SEMILLA)
e <- proporcion_error(args[1])
k <- log(1000)
mu_alfa <- k + MU_ALFA_DESDE_K
nt <- length(ANIOS)
alfa <- mu_alfa + SIGMA_ALFA * normales(N)
v <- SIGMA_V * normales(N * nt)
t_exo <- as.numeric(uniformes(N) < 0.5)
u_marca <- uniformes(N * nt)

donante <- rep(0:(N - 1), each = nt)
anio <- rep(ANIOS, times = N)
lam <- PENDIENTE_LAMBDA * (anio - 2012)
post <- as.numeric(anio >= T0)
marca <- u_marca < e[as.character(anio)]
y0 <- alfa[donante + 1] + lam + v
unidad_rut <- donante
unidad_err <- 2 * donante + as.integer(marca)

reg <- registro()
for (a in ANIOS) reg_dato(reg, sprintf("datos:e_%d", a), e[[as.character(a)]])
reg_dato(reg, "datos:filas_marcadas", sum(marca))

# 1. Tratamiento exógeno
ya <- y0 + BETA * t_exo[donante + 1] * post
objetivo_1 <- function(a) BETA * (a >= T0)
comprobar_eventos(reg, "exogeno", estudio_eventos(unidad_rut, anio, ya, t_exo[donante + 1]), objetivo_1)

# 2. Tratamiento por el nivel de 2014
y2014 <- y0[anio == REF]
t_nivel <- as.numeric(y2014 > k)
yb <- y0 + BETA * t_nivel[donante + 1] * post
b <- sesgo_reversion(mu_alfa, PENDIENTE_LAMBDA * (REF - 2012), k)
reg_regla(reg, "nivel_previo:sesgo_b", abs(b), MIN_SESGO, ">=objetivo", abs(b) >= MIN_SESGO)
objetivo_2 <- function(a) BETA * (a >= T0) + b
comprobar_eventos(reg, "nivel_previo", estudio_eventos(unidad_rut, anio, yb, t_nivel[donante + 1]), objetivo_2)

# 3. Grupo de control de 200 a 900 UTM, por nivel permanente
t_c <- alfa > k
g_c <- alfa >= log(200) & alfa <= log(900)
contamina <- g_c & alfa > log(700)
pi_c <- sum(contamina) / sum(g_c)
yc <- y0 + (BETA * t_c[donante + 1] + BETA_C * contamina[donante + 1]) * post
sel <- (t_c | g_c)[donante + 1]
delta <- dif_en_dif(unidad_rut[sel], anio[sel], yc[sel], as.numeric(t_c[donante + 1][sel]))
reg_dato(reg, "control_200_900:pi_c", pi_c)
reg_regla(reg, "control_200_900:sesgo", pi_c * BETA_C, MIN_SESGO, ">=objetivo", pi_c * BETA_C >= MIN_SESGO)
reg_regla(reg, "control_200_900:delta", delta, BETA - pi_c * BETA_C, "abs<=0.03",
          abs(delta - (BETA - pi_c * BETA_C)) <= TOL)

# 4. Error por RUT faltante
comprobar_eventos(reg, "exogeno_error_rut", estudio_eventos(unidad_err, anio, ya, t_exo[donante + 1]), objetivo_1)
en_ref <- anio == REF
tiene_ref <- logical(2 * N)
t_unidad <- numeric(2 * N)
tiene_ref[unidad_err[en_ref] + 1] <- TRUE
t_unidad[unidad_err[en_ref] + 1] <- as.numeric(yb[en_ref] > k)
sel <- tiene_ref[unidad_err + 1]
comprobar_eventos(reg, "nivel_previo_error_rut",
                  estudio_eventos(unidad_err[sel], anio[sel], yb[sel], t_unidad[unidad_err[sel] + 1]), objetivo_2)
mismo <- mean(unidad_err[anio == 2014] == unidad_err[anio == 2015])
obj <- (1 - e[["2014"]]) * (1 - e[["2015"]])
reg_regla(reg, "error_rut:mismo_id_2014_2015", mismo, obj, "abs<=0.02", abs(mismo - obj) <= TOL_SEGUIMIENTO)

cat(sprintf("eventos (R): sesgo b %.4f, pi_c %.4f, mismo id 2014-2015 %.4f; %d cifras, %d fuera de tolerancia, registradas como hallazgo\n",
            b, pi_c, mismo, length(reg$filas), length(reg_fallas(reg))))
reg_terminar(reg, args[3], args[2], "eventos", "d-17")
