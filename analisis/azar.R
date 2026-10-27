# Generador de números aleatorios común a Python y R para las simulaciones de P-05.
# Contraparte de azar.py, con las mismas reglas (cabecera de ese archivo y DECISIONES.md#d-15, «Tolerancia
# entre Python y R para las simulaciones»):
# MT19937 iniciado con init_genrand(semilla), uniformes de runif() con ese estado en .Random.seed y
# normales por Box-Muller, sqrt(-2 log u1) cos(2 pi u2), con u1 y u2 dos bloques consecutivos.

# init_genrand de la implementación de referencia, con aritmética exacta en dobles: el producto de
# 32 por 32 bits se parte en dos de 32 por 16 para no pasar de 2^53.
init_genrand <- function(semilla) {
  m32 <- 4294967296
  mt <- numeric(624)
  mt[1] <- semilla %% m32
  for (i in 2:624) {
    x <- mt[i - 1]
    alto <- floor(x / 1073741824)  # x >> 30, entre 0 y 3: solo toca los dos bits bajos
    bajo <- x %% 4
    x <- x - bajo + bitwXor(as.integer(bajo), as.integer(alto))
    xh <- floor(x / 65536)
    xl <- x %% 65536
    a <- 1812433253
    mt[i] <- ((((a * xh) %% 65536) * 65536 + a * xl) + (i - 1)) %% m32
  }
  mt
}

azar_iniciar <- function(semilla) {
  mt <- init_genrand(semilla)
  firmado <- ifelse(mt >= 2147483648, mt - 4294967296, mt)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  assign(".Random.seed", c(10403L, 624L, as.integer(firmado)), envir = .GlobalEnv)
  invisible(NULL)
}

uniformes <- function(n) runif(n)

normales <- function(n) {
  u1 <- runif(n)
  u2 <- runif(n)
  sqrt(-2 * log(u1)) * cos(2 * pi * u2)
}
