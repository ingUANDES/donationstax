# Precio fiscal de donar: P = 1 - c - tau * g.
# c: fracción imputada como crédito; g: fracción deducida como gasto;
# tau: tasa marginal del impuesto a la renta del donante.
# Los parámetros vienen del CSV de entrada; este script no los fija.
# Contraparte en Python: precio_fiscal.py (ver contraste.py).
#
# Uso: Rscript precio_fiscal.R entrada.csv salida.csv

precio_fiscal <- function(c, g, tau) 1 - c - tau * g

args <- commandArgs(trailingOnly = TRUE)
d <- read.csv(args[1], stringsAsFactors = FALSE)
p <- precio_fiscal(d$c, d$g, d$tau)
out <- data.frame(caso = d$caso, precio = p, subsidio = 1 - p)
# write.csv escribe hasta 15 cifras significativas; contraste.py compara con tolerancia
write.csv(out, args[2], row.names = FALSE, quote = FALSE)
