"""Utilidades comunes de las simulaciones de BLUEPRINT.md#p-05 (sim_bunching, sim_eventos y sim_vi).

- Registro: una línea por cifra (clave, valor, objetivo, regla, cumple); las reglas son las de las
  decisiones de cada simulación, y fallas() lista las que no se cumplen.
- dentro: desviaciones de la media de cada unidad (efectos fijos de unidad, también en paneles no
  balanceados).
- mco: mínimos cuadrados ordinarios.
- proporcion_error: n_error_medicion / n_filas por año, de datos/rut_completado_por_anio.csv.
- Registro.terminar: escribe la salida y falla con AssertionError salvo que las reglas incumplidas sean
  exactamente las registradas como hallazgo para esa simulación en analisis/fixtures/sim_hallazgos.csv.
  Una regla incumplida que no está registrada, o una registrada que se cumple, detiene la verificación;
  las tolerancias no se cambian (BLUEPRINT.md#p-05).

Contraparte en R: sim_comun.R.
"""
import csv

import numpy as np


def proporcion_error(ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        return {int(r["anio"]): int(r["n_error_medicion"]) / int(r["n_filas"]) for r in csv.DictReader(f)}



def dentro(M, unidad):
    """Desviaciones de la media de cada unidad (efectos fijos de unidad)."""
    _, cod = np.unique(unidad, return_inverse=True)
    cuenta = np.bincount(cod).astype(np.float64)
    M = np.asarray(M, dtype=np.float64)
    if M.ndim == 1:
        return M - (np.bincount(cod, weights=M) / cuenta)[cod]
    return np.column_stack([M[:, j] - (np.bincount(cod, weights=M[:, j]) / cuenta)[cod] for j in range(M.shape[1])])



def mco(y, X):
    return np.linalg.lstsq(X, y, rcond=None)[0]



class Registro:
    def __init__(self):
        self.filas = []

    def dato(self, clave, valor):
        self.filas.append([clave, float(valor), None, "", ""])

    def regla(self, clave, valor, objetivo, regla, ok):
        self.filas.append([clave, float(valor), None if objetivo is None else float(objetivo), regla,
                           "sí" if ok else "no"])

    def escribir(self, ruta):
        with open(ruta, "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f, lineterminator="\n")
            w.writerow(["clave", "valor", "objetivo", "regla", "cumple"])
            for clave, v, o, regla, ok in self.filas:
                w.writerow([clave, repr(v), "" if o is None else repr(o), regla, ok])

    def fallas(self):
        return [f[0] for f in self.filas if f[4] == "no"]

    def terminar(self, salida, ruta_hallazgos, simulacion, decision):
        self.escribir(salida)
        with open(ruta_hallazgos, newline="", encoding="utf-8") as f:
            esperadas = {r["clave"] for r in csv.DictReader(f) if r["simulacion"] == simulacion}
        fallas = set(self.fallas())
        nuevas, resueltas = sorted(fallas - esperadas), sorted(esperadas - fallas)
        assert not nuevas, f"fuera de la tolerancia de DECISIONES.md#{decision} y sin registrar: {nuevas}"
        assert not resueltas, f"hallazgos registrados que ya no fallan (revisar {ruta_hallazgos}): {resueltas}"
        return len(fallas)
