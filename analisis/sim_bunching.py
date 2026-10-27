"""Simulación de bunching en el quiebre de 1.000 UTM, con efecto conocido (BLUEPRINT.md#p-05).

El caso, el estimador y las tolerancias son los de DECISIONES.md#d-16 («Tolerancias de la simulación de
bunching en el quiebre de 1.000 UTM»); las constantes de abajo los implementan. Los precios P0 y P1 salen
de datos/precio_fiscal_por_caso.csv. Escribe una línea por cifra (clave, valor, objetivo, regla, cumple)
y después falla con AssertionError si alguna regla no se cumple, salvo los
hallazgos registrados (sim_comun.py, Registro.terminar). Contraparte en R: sim_bunching.R; los
dos usan el generador de azar.py / azar.R y contraste.py compara las salidas.

Uso: python sim_bunching.py precio_fiscal_por_caso.csv sim_hallazgos.csv salida.csv
"""
import csv
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
from azar import Azar  # noqa: E402
from sim_comun import Registro  # noqa: E402

SEMILLA = 20261027
N = 300000
MU_DESDE_K = -0.5          # media de z* menos k = ln 1000
SIGMA = 1.0
PROP_FONDO = 0.20
EPSILON = 0.5
ANCHO_AGOLPE = 0.04        # los que se agolpan quedan uniformes en [k - 0,04, k)
W = 0.02                   # ancho del tramo
NB = 100                   # tramos en [k - 1, k + 1)
CENTRO = 50                # el tramo 50 empieza en k
Q_PRINCIPAL = 5
VENTANA_PRINCIPAL = (-4, 0)  # en tramos: [k - 0,08, k)
ORDENES = (3, 4, 5, 6)
INFERIORES = (-3, -4, -5)    # k - 0,06, k - 0,08, k - 0,10
SUPERIORES = (0, 1)          # k, k + 0,02
REPLICAS = 200
CASO_P0, CASO_P1 = "e2012_hasta_1000", "e2012_exceso_sin_fondo"


def precios(ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        d = {r["caso"]: float(r["precio"]) for r in csv.DictReader(f)}
    return d[CASO_P0], d[CASO_P1]


def conteos(z, k):
    j = np.floor((z - k) / W).astype(np.int64) + CENTRO
    j = j[(j >= 0) & (j < NB)]
    return np.bincount(j, minlength=NB).astype(np.float64)


def matriz(q, lo, hi):
    j = np.arange(NB)
    s = (j - CENTRO + 0.5) * W
    izq = j < CENTRO
    cols = [np.where(izq, s ** p, 0.0) for p in range(q + 1)]
    cols += [np.where(izq, 0.0, s ** p) for p in range(q + 1)]
    cols += [(j == jj).astype(np.float64) for jj in range(CENTRO + lo, CENTRO + hi)]
    return np.column_stack(cols)


def estimar(c, q, lo, hi, ln_razon):
    X = matriz(q, lo, hi)
    coef = np.linalg.lstsq(X, c, rcond=None)[0]
    b = float(coef[2 * (q + 1):].sum())
    h_izq, h_der = float(coef[0]), float(coef[q + 1])
    ajuste = X @ coef
    return {"B": b, "epsilon": 2 * W * b / ((h_izq + h_der) * ln_razon), "h_izq": h_izq, "h_der": h_der,
            "ajuste": ajuste, "resid": c - ajuste}


def error_estandar(az, c, q, lo, hi, ln_razon):
    r = estimar(c, q, lo, hi, ln_razon)
    eps = []
    for _ in range(REPLICAS):
        idx = np.floor(az.uniformes(NB) * NB).astype(np.int64)
        eps.append(estimar(r["ajuste"] + r["resid"][idx], q, lo, hi, ln_razon)["epsilon"])
    return float(np.std(np.array(eps), ddof=1))


def main(ruta_precios, ruta_hallazgos, salida):
    az = Azar(SEMILLA)
    p0, p1 = precios(ruta_precios)
    ln_razon = math.log(p1 / p0)
    k = math.log(1000.0)
    delta = EPSILON * ln_razon
    zs = k + MU_DESDE_K + SIGMA * az.normales(N)
    fondo = az.uniformes(N) < PROP_FONDO
    pos = az.uniformes(N)
    agolpa = (~fondo) & (zs > k) & (zs <= k + delta)
    z = np.where(fondo | (zs <= k), zs, np.where(agolpa, k - ANCHO_AGOLPE * pos, zs - delta))
    b_sim = int(agolpa.sum())
    c_sin = conteos(z[~fondo], k)
    c_todos = conteos(z, k)

    reg = Registro()
    reg.dato("datos:P0", p0)
    reg.dato("datos:P1", p1)
    reg.dato("datos:n", N)
    reg.dato("datos:n_fondo", int(fondo.sum()))
    reg.dato("datos:B_simulado", b_sim)
    reg.dato("datos:conteo_sin_fondo_tramo_49", c_sin[CENTRO - 1])

    lo, hi = VENTANA_PRINCIPAL
    r = estimar(c_sin, Q_PRINCIPAL, lo, hi, ln_razon)
    ee = error_estandar(az, c_sin, Q_PRINCIPAL, lo, hi, ln_razon)
    reg.regla("principal:sin_fondo:epsilon", r["epsilon"], EPSILON, "rel<=0.10",
              abs(r["epsilon"] - EPSILON) <= 0.10 * EPSILON)
    reg.regla("principal:sin_fondo:B", r["B"], b_sim, "rel<=0.10", abs(r["B"] - b_sim) <= 0.10 * b_sim)
    reg.dato("principal:sin_fondo:h_izq", r["h_izq"])
    reg.dato("principal:sin_fondo:h_der", r["h_der"])
    reg.regla("principal:sin_fondo:error_estandar", ee, None, "finito>0", math.isfinite(ee) and ee > 0)

    for q in ORDENES:
        for lo_s in INFERIORES:
            for hi_s in SUPERIORES:
                e = estimar(c_sin, q, lo_s, hi_s, ln_razon)["epsilon"]
                clave = f"sensibilidad:q{q}:inf{lo_s * W:+.2f}:sup{hi_s * W:+.2f}:epsilon"
                reg.regla(clave, e, EPSILON, "rel<=0.15", abs(e - EPSILON) <= 0.15 * EPSILON)

    t = estimar(c_todos, Q_PRINCIPAL, lo, hi, ln_razon)
    atenuado = (1 - PROP_FONDO) * EPSILON
    reg.regla("principal:todos:B", t["B"], b_sim, "rel<=0.10", abs(t["B"] - b_sim) <= 0.10 * b_sim)
    reg.regla("principal:todos:epsilon_sesgo", t["epsilon"], (1 - PROP_FONDO / 2) * EPSILON, "<=objetivo",
              t["epsilon"] <= (1 - PROP_FONDO / 2) * EPSILON)
    reg.regla("principal:todos:epsilon_atenuado", t["epsilon"], atenuado, "rel<=0.15",
              abs(t["epsilon"] - atenuado) <= 0.15 * atenuado)

    n_fallas = reg.terminar(salida, ruta_hallazgos, "bunching", "d-16")
    print(f"bunching (Python): B simulado {b_sim}, epsilon principal {r['epsilon']:.4f} (ee {ee:.4f}), "
          f"con todos {t['epsilon']:.4f}; {len(reg.filas)} cifras, {n_fallas} fuera de tolerancia, registradas como hallazgo")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3])
