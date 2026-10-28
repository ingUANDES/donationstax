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

Monte Carlo (DECISIONES.md#d-19, «Método de Monte Carlo para las simulaciones de bunching y de estudio de
eventos»), para sim_bunching_mc y sim_eventos_mc:

- semillas(base, lista): la lista larga es base + r, r = 0..R_L - 1; la corta, sus primeras R_C.
- regla_sesgo: tipo (a), |sesgo| <= 0,25 EE + 3 EMC, con d_r = estimación - objetivo, EE la desviación de
  d_r (con R - 1) y EMC = EE / sqrt(R).
- regla_proporcion: tipos (b) y (c), |p - p0| <= 3 sqrt(p0 (1 - p0) / R), y (d), p >= p0 - ese margen.
- regla_deteccion: tipo (e), media + 3 EMC <= u («bajo») o media - 3 EMC >= u («sobre»).

Contraparte en R: sim_comun.R.
"""
import csv
import math

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



REPETICIONES = {"corta": 10, "larga": 200}  # R_C y R_L de DECISIONES.md#d-19, punto 2
MARGEN_SESGO = 0.25  # fracción del EE en la regla de tipo (a)
Z_EMC = 3.0          # errores de Monte Carlo de margen


def semillas(base, lista):
    return [base + r for r in range(REPETICIONES[lista])]


def media_ee_emc(x):
    x = np.asarray(x, dtype=np.float64)
    ee = float(np.std(x, ddof=1))
    return float(x.mean()), ee, ee / math.sqrt(len(x))


def regla_sesgo(reg, clave, est, obj):
    """Tipo (a): escribe media, sesgo (regla), EE y EMC de d_r = est - obj."""
    est = np.asarray(est, dtype=np.float64)
    d = est - np.asarray(obj, dtype=np.float64)
    sesgo, ee, emc = media_ee_emc(d)
    reg.dato(f"{clave}:media", est.mean())
    reg.regla(f"{clave}:sesgo", sesgo, 0.0, "abs<=0.25ee+3emc", abs(sesgo) <= MARGEN_SESGO * ee + Z_EMC * emc)
    reg.dato(f"{clave}:ee", ee)
    reg.dato(f"{clave}:emc", emc)
    return sesgo


def regla_proporcion(reg, clave, x, p0, tipo):
    """Tipos (b) y (c) con tipo='igual'; (d) con tipo='potencia'. tipo=None escribe la proporción sin regla."""
    x = np.asarray(x, dtype=bool)
    p = float(x.mean())
    margen = Z_EMC * math.sqrt(p0 * (1 - p0) / len(x))
    if tipo is None:
        reg.dato(clave, p)
    elif tipo == "igual":
        reg.regla(clave, p, p0, "abs<=3emc_nominal", abs(p - p0) <= margen)
    else:
        reg.regla(clave, p, p0, ">=objetivo-3emc_nominal", p >= p0 - margen)
    return p


def regla_deteccion(reg, clave, est, u, lado):
    """Tipo (e): el sesgo conocido existe; lado 'bajo' (media + 3 EMC <= u) o 'sobre' (media - 3 EMC >= u)."""
    m, ee, emc = media_ee_emc(est)
    if lado == "bajo":
        reg.regla(clave, m, u, "media+3emc<=objetivo", m + Z_EMC * emc <= u)
    else:
        reg.regla(clave, m, u, "media-3emc>=objetivo", m - Z_EMC * emc >= u)
    reg.dato(f"{clave}:emc", emc)
    return m
