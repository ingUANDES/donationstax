"""Simulación de variables instrumentales con efectos fijos de año, con efecto conocido (BLUEPRINT.md#p-05).

El caso, los estimadores y las tolerancias son los de DECISIONES.md#d-18 («Tolerancias de la simulación de
variables instrumentales»); las constantes de abajo los implementan. Los precios por régimen y año salen
de datos/precio_fiscal_por_caso.csv y la proporción de filas sin RUT, de datos/rut_completado_por_anio.csv.
Escribe una línea por cifra (clave, valor, objetivo, regla, cumple) y después falla con AssertionError si
alguna regla no se cumple, salvo los hallazgos registrados (sim_comun.py, Registro.terminar). Contraparte en R: sim_vi.R.

Uso: python sim_vi.py precio_fiscal_por_caso.csv rut_completado_por_anio.csv sim_hallazgos.csv salida.csv
"""
import csv
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
from azar import Azar  # noqa: E402
from sim_comun import Registro, dentro, mco, proporcion_error  # noqa: E402

SEMILLA = 20261029
N = 5000
ANIOS = [2013, 2014, 2015, 2016, 2017, 2019, 2020, 2023, 2025]
CASOS_A = {2013: "e2013_hasta_1000", 2014: "e2014_hasta_1000", 2015: "e2015_hasta_1000",
           2016: "e2016_hasta_1000", 2017: "e2017_14A_hasta_1000", 2019: "e2019_14A_hasta_1000",
           2020: "e2020_14A_hasta_1000", 2023: "e2023_14A_hasta_1000", 2025: "e2025_14A_hasta_1000"}
CASOS_B = {**CASOS_A, 2017: "e2017_14B_hasta_1000", 2019: "e2019_14B_hasta_1000",
           2020: "e2020_pro_pyme", 2023: "e2023_pro_pyme", 2025: "e2025_pro_pyme"}
CASO_PERDIDA = "e2019_perdida"
CASO_TASA_27 = "e2025_14A_hasta_1000"
EPSILON = 1.0
PENDIENTE_LAMBDA = 0.03
SIGMA_U = 0.5
RHO = 0.6
CORTE_PERDIDA = -1.2816
TOL = 0.15
MIN_F = 10.0
TOL_RESIDUO = 1e-10


def leer_precios(ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        return {r["caso"]: float(r["precio"]) for r in csv.DictReader(f)}


def mc2e(y, x, z, W, n_unidades):
    """MC2E con un regresor endógeno x, un instrumento z y exógenas W (columnas, puede estar vacía)."""
    Z = np.column_stack([z] + W)
    g = mco(x, Z)
    xhat = Z @ g
    b = mco(y, np.column_stack([xhat] + W))
    r1 = x - xhat
    n, kz = Z.shape
    s2 = float(r1 @ r1) / (n - kz - n_unidades)
    var = s2 * float(np.linalg.inv(Z.T @ Z)[0, 0])
    return float(b[0]), float(g[0] ** 2 / var)


def residuo_maximo(z, W):
    if not W:
        return float(np.max(np.abs(z)))
    Wm = np.column_stack(W)
    return float(np.max(np.abs(z - Wm @ mco(z, Wm))))


def estimar(reg, nombre, unidad, anio, y, ln_pef, ln_pleg, ln_pa, ln_p27):
    yd, xd = dentro(y, unidad), dentro(ln_pef, unidad)
    W = [dentro((anio == a).astype(np.float64), unidad) for a in ANIOS[1:]]
    g = len(np.unique(unidad))
    e_mco = -float(mco(yd, np.column_stack([xd] + W))[0])
    reg.regla(f"{nombre}:mco_sesgo", e_mco - EPSILON, TOL, ">=objetivo", e_mco - EPSILON >= TOL)
    b, f = mc2e(yd, xd, dentro(ln_pleg, unidad), W, g)
    reg.regla(f"{nombre}:mc2e:epsilon", -b, EPSILON, "abs<=0.15", abs(-b - EPSILON) <= TOL)
    reg.regla(f"{nombre}:mc2e:F", f, MIN_F, ">=objetivo", f >= MIN_F)
    za, z27 = dentro(ln_pa, unidad), dentro(ln_p27, unidad)
    ra, r27 = residuo_maximo(za, W), residuo_maximo(z27, W)
    reg.regla(f"{nombre}:tasa_por_anio:residuo", ra, TOL_RESIDUO, "<=objetivo", ra <= TOL_RESIDUO)
    reg.regla(f"{nombre}:tasa_27:residuo", r27, TOL_RESIDUO, "<=objetivo", r27 <= TOL_RESIDUO)
    b4, _ = mc2e(yd, xd, za, [], g)
    reg.regla(f"{nombre}:tasa_por_anio_sin_ef_anio:sesgo", abs(-b4 - EPSILON), TOL, ">=objetivo",
              abs(-b4 - EPSILON) >= TOL)
    return e_mco, -b, -b4


def main(ruta_precios, ruta_error, ruta_hallazgos, salida):
    az = Azar(SEMILLA)
    pr = leer_precios(ruta_precios)
    e = proporcion_error(ruta_error)
    nt = len(ANIOS)
    alfa = az.normales(N)
    tipo_b = az.uniformes(N) < 0.5
    eta = az.normales(N * nt)
    ruido = az.normales(N * nt)
    u_marca = az.uniformes(N * nt)

    donante = np.repeat(np.arange(N), nt)
    anio = np.tile(np.array(ANIOS), N)
    ln_pa_anio = {a: math.log(pr[CASOS_A[a]]) for a in ANIOS}
    ln_pb_anio = {a: math.log(pr[CASOS_B[a]]) for a in ANIOS}
    ln_pa = np.array([ln_pa_anio[a] for a in anio.tolist()])
    ln_pb = np.array([ln_pb_anio[a] for a in anio.tolist()])
    ln_pleg = np.where(tipo_b[donante], ln_pb, ln_pa)
    perdida = eta < CORTE_PERDIDA
    ln_pef = np.where(perdida, math.log(pr[CASO_PERDIDA]), ln_pleg)
    u = SIGMA_U * (RHO * eta + math.sqrt(1 - RHO ** 2) * ruido)
    y = alfa[donante] + PENDIENTE_LAMBDA * (anio - 2013) - EPSILON * ln_pef + u
    ln_p27 = np.full(N * nt, math.log(pr[CASO_TASA_27]))
    marca = u_marca < np.array([e[a] for a in anio.tolist()])

    reg = Registro()
    reg.dato("datos:proporcion_perdida", float(perdida.mean()))
    reg.dato("datos:filas_marcadas", int(marca.sum()))
    r_sin = estimar(reg, "sin_error_rut", donante, anio, y, ln_pef, ln_pleg, ln_pa, ln_p27)
    r_con = estimar(reg, "error_rut", 2 * donante + marca.astype(np.int64), anio, y, ln_pef, ln_pleg, ln_pa, ln_p27)

    n_fallas = reg.terminar(salida, ruta_hallazgos, "vi", "d-18")
    print(f"VI (Python): sin error MCO {r_sin[0]:.4f}, MC2E {r_sin[1]:.4f}, sin EF de año {r_sin[2]:.4f}; "
          f"con error MCO {r_con[0]:.4f}, MC2E {r_con[1]:.4f}; {len(reg.filas)} cifras, {n_fallas} fuera de tolerancia, registradas como hallazgo")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
