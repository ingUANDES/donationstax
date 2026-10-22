"""Precio fiscal de donar: P = 1 - c - tau * g.

c: fracción de la donación que se imputa como crédito contra el impuesto.
g: fracción que se deduce de la base imponible como gasto.
tau: tasa marginal del impuesto sobre la renta del donante.

Los parámetros (c, g, tau) vienen del CSV de entrada; este módulo no los fija.
Contraparte en R: precio_fiscal.R (debe dar el mismo resultado, ver contraste.py).

Uso: python precio_fiscal.py entrada.csv salida.csv
"""
import csv
import sys


def precio_fiscal(c, g, tau):
    return 1.0 - c - tau * g


def main(entrada, salida):
    with open(entrada, newline="", encoding="utf-8") as f:
        filas = list(csv.DictReader(f))
    with open(salida, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["caso", "precio", "subsidio"])
        for r in filas:
            p = precio_fiscal(float(r["c"]), float(r["g"]), float(r["tau"]))
            w.writerow([r["caso"], repr(p), repr(1.0 - p)])


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
