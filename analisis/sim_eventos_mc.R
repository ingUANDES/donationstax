# Monte Carlo del estudio de eventos, con cobertura y prueba de tendencias previas (BLUEPRINT.md#p-05).
# Contraparte de sim_eventos_mc.py: mismos casos (DECISIONES.md#d-17), varianza, prueba y tolerancias
# (DECISIONES.md#d-25, método en DECISIONES.md#d-19), semillas y orden de extracciones del generador de azar.R.
# Las constantes y el sesgo analítico repiten los de sim_eventos.R.
#
# Uso: Rscript sim_eventos_mc.R rut_completado_por_anio.csv sim_hallazgos.csv corta|larga salida.csv
#      [repeticiones.csv]

este <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
source(file.path(dirname(normalizePath(este)), "azar.R"))
source(file.path(dirname(normalizePath(este)), "sim_comun.R"))

# De sim_eventos.R (DECISIONES.md#d-17)
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
MIN_SESGO <- 0.06

# DECISIONES.md#d-25
SEMILLA_BASE <- 34100000
ANIOS_T <- setdiff(ANIOS, REF)
PRE <- c(1, 2)
CRITICO_WALD <- -2 * log(0.05)
Z <- 1.96
POTENCIA <- 0.80

sesgo_reversion <- function(mu_alfa, lambda_ref, cc) {
  s <- sqrt(SIGMA_ALFA^2 + SIGMA_V^2)
  a <- (cc - mu_alfa - lambda_ref) / s
  phi <- exp(-a * a / 2) / sqrt(2 * pi)
  Phi <- pnorm(a)
  -(SIGMA_V^2 / s) * phi * (1 / (1 - Phi) + 1 / Phi)
}

mco_conglomerado <- function(unidad, y, X) {
  Xd <- dentro(X, unidad)
  yd <- dentro(y, unidad)
  b <- mco(yd, Xd)
  u <- yd - drop(Xd %*% b)
  S <- rowsum(Xd * u, unidad, reorder = TRUE)
  G <- nrow(S)
  n <- nrow(Xd)
  K <- ncol(Xd)
  XtXi <- solve(crossprod(Xd))
  V <- (G / (G - 1)) * ((n - 1) / (n - K)) * (XtXi %*% crossprod(S) %*% XtXi)
  list(b = b, V = V)
}

eventos <- function(unidad, anio, y, T) {
  X <- cbind(sapply(ANIOS[-1], function(a) as.numeric(anio == a)),
             sapply(ANIOS_T, function(a) T * (anio == a)))
  f <- mco_conglomerado(unidad, y, X)
  m <- length(ANIOS) - 1
  idx <- (m + 1):ncol(X)
  g <- f$b[idx]
  Vg <- f$V[idx, idx]
  ee <- sqrt(diag(Vg))
  gp <- g[PRE]
  wald <- drop(gp %*% solve(Vg[PRE, PRE], gp))
  list(g = unname(g), ee = unname(ee), wald = wald > CRITICO_WALD, ind = any(abs(gp) > Z * ee[PRE]))
}

dif_en_dif <- function(unidad, anio, y, T) {
  X <- cbind(sapply(ANIOS[-1], function(a) as.numeric(anio == a)), T * (anio >= T0))
  f <- mco_conglomerado(unidad, y, X)
  K <- ncol(X)
  c(f$b[[K]], sqrt(f$V[K, K]))
}

args <- commandArgs(trailingOnly = TRUE)
e <- proporcion_error(args[1])
lista <- args[3]
sem <- semillas(SEMILLA_BASE, lista)
k <- log(1000)
mu_alfa <- k + MU_ALFA_DESDE_K
nt <- length(ANIOS)
donante <- rep(0:(N - 1), each = nt)
anio <- rep(ANIOS, times = N)
lam <- PENDIENTE_LAMBDA * (anio - 2012)
post <- as.numeric(anio >= T0)
e_fila <- unname(e[as.character(anio)])
en_ref <- anio == REF
b <- sesgo_reversion(mu_alfa, PENDIENTE_LAMBDA * (REF - 2012), k)
beta_post <- BETA * (ANIOS_T >= T0)
casos <- c("exogeno", "nivel_previo", "exogeno_error_rut", "nivel_previo_error_rut")
nr <- length(sem)
res <- lapply(setNames(casos, casos), function(c)
  list(g = matrix(NA_real_, nr, 7), ee = matrix(NA_real_, nr, 7), wald = logical(nr), ind = logical(nr)))
ctl <- list(delta = numeric(nr), ee = numeric(nr), pi_c = numeric(nr))
mismo <- numeric(nr)
filas_rep <- list()

for (r in seq_len(nr)) {
  azar_iniciar(sem[r])
  alfa <- mu_alfa + SIGMA_ALFA * normales(N)
  v <- SIGMA_V * normales(N * nt)
  t_exo <- as.numeric(uniformes(N) < 0.5)
  u_marca <- uniformes(N * nt)
  marca <- u_marca < e_fila
  y0 <- alfa[donante + 1] + lam + v
  unidad_err <- 2 * donante + as.integer(marca)

  ya <- y0 + BETA * t_exo[donante + 1] * post
  t_nivel <- as.numeric(y0[en_ref] > k)
  yb <- y0 + BETA * t_nivel[donante + 1] * post
  sr <- list(exogeno = eventos(donante, anio, ya, t_exo[donante + 1]),
             nivel_previo = eventos(donante, anio, yb, t_nivel[donante + 1]),
             exogeno_error_rut = eventos(unidad_err, anio, ya, t_exo[donante + 1]))
  tiene_ref <- logical(2 * N)
  t_unidad <- numeric(2 * N)
  tiene_ref[unidad_err[en_ref] + 1] <- TRUE
  t_unidad[unidad_err[en_ref] + 1] <- as.numeric(yb[en_ref] > k)
  sel <- tiene_ref[unidad_err + 1]
  sr$nivel_previo_error_rut <- eventos(unidad_err[sel], anio[sel], yb[sel], t_unidad[unidad_err[sel] + 1])
  for (c in casos) {
    res[[c]]$g[r, ] <- sr[[c]]$g
    res[[c]]$ee[r, ] <- sr[[c]]$ee
    res[[c]]$wald[r] <- sr[[c]]$wald
    res[[c]]$ind[r] <- sr[[c]]$ind
  }

  t_c <- alfa > k
  g_c <- alfa >= log(200) & alfa <= log(900)
  contamina <- g_c & alfa > log(700)
  pi_c <- sum(contamina) / sum(g_c)
  yc <- y0 + (BETA * t_c[donante + 1] + BETA_C * contamina[donante + 1]) * post
  selc <- (t_c | g_c)[donante + 1]
  dd <- dif_en_dif(donante[selc], anio[selc], yc[selc], as.numeric(t_c[donante + 1][selc]))
  ctl$delta[r] <- dd[1]
  ctl$ee[r] <- dd[2]
  ctl$pi_c[r] <- pi_c
  mismo[r] <- mean(unidad_err[anio == 2014] == unidad_err[anio == 2015])
  filas_rep[[r]] <- c(r - 1, sem[r], unlist(lapply(casos, function(c) sr[[c]]$g)),
                      sapply(casos, function(c) as.integer(sr[[c]]$wald)), dd[1], dd[2], pi_c, mismo[r])
}

reg <- registro()
reg_dato(reg, "datos:repeticiones", nr)
reg_dato(reg, "datos:semilla_base", SEMILLA_BASE)
for (a in ANIOS) reg_dato(reg, sprintf("datos:e_%d", a), e[[as.character(a)]])
reg_regla(reg, "nivel_previo:sesgo_b", abs(b), MIN_SESGO, ">=objetivo", abs(b) >= MIN_SESGO)
for (c in casos) {
  nivel <- startsWith(c, "nivel_previo")
  obj <- beta_post + (if (nivel) b else 0)
  for (i in seq_along(ANIOS_T)) {
    a <- ANIOS_T[i]
    regla_sesgo(reg, sprintf("%s:gamma_%d", c, a), res[[c]]$g[, i], obj[i])
    cubre <- abs(res[[c]]$g[, i] - beta_post[i]) <= Z * res[[c]]$ee[, i]
    regla_proporcion(reg, sprintf("%s:gamma_%d:cobertura", c, a), cubre, 0.95, if (nivel) NULL else "igual")
    reg_dato(reg, sprintf("%s:gamma_%d:ee_conglomerado:media", c, a), mean(res[[c]]$ee[, i]))
  }
  if (nivel) {
    regla_proporcion(reg, paste0(c, ":pretendencias:wald"), res[[c]]$wald, POTENCIA, "potencia")
  } else {
    regla_proporcion(reg, paste0(c, ":pretendencias:wald"), res[[c]]$wald, 0.05, "igual")
  }
  regla_proporcion(reg, paste0(c, ":pretendencias:individual"), res[[c]]$ind, 0.05, NULL)
}
d <- ctl$delta; ed <- ctl$ee; pc <- ctl$pi_c
reg_dato(reg, "control_200_900:pi_c:media", mean(pc))
regla_sesgo(reg, "control_200_900:delta", d, BETA - pc * BETA_C)
regla_deteccion(reg, "control_200_900:pi_c_beta_c", pc * BETA_C, MIN_SESGO, "sobre")
regla_proporcion(reg, "control_200_900:delta:cobertura_beta", abs(d - BETA) <= Z * ed, 0.95, NULL)
regla_proporcion(reg, "control_200_900:delta:cobertura_contaminado", abs(d - (BETA - pc * BETA_C)) <= Z * ed, 0.95, NULL)
reg_dato(reg, "control_200_900:delta:ee_conglomerado:media", mean(ed))
regla_sesgo(reg, "error_rut:mismo_id_2014_2015", mismo, (1 - e[["2014"]]) * (1 - e[["2015"]]))

if (length(args) >= 5) {
  m <- do.call(rbind, filas_rep)
  colnames(m) <- c("r", "semilla", unlist(lapply(casos, function(c) sprintf("%s_gamma_%d", c, ANIOS_T))),
                   paste0(casos, "_wald"), "delta", "ee_delta", "pi_c", "mismo_id_2014_2015")
  write.csv(as.data.frame(m), args[5], row.names = FALSE, fileEncoding = "UTF-8")
}
cat(sprintf("eventos_mc (R, lista %s, %d repeticiones): %d cifras, %d reglas incumplidas, registradas como hallazgo\n",
            lista, nr, length(reg$filas), length(reg_fallas(reg))))
reg_terminar(reg, args[4], args[2], paste0("eventos_mc_", lista), "d-25")
