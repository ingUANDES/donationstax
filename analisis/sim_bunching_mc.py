"""Monte Carlo del bunching en el quiebre de 1.000 UTM, con regla de selección fijada antes (BLUEPRINT.md#p-05).

Procesos generadores, regla de selección de la ventana y del orden, sensibilidad y tolerancias: los de
DECISIONES.md#d-24 («Monte Carlo del bunching con regla de selección fijada antes de correr»); método común:
DECISIONES.md#d-19. Toma de sim_bunching.py (DECISIONES.md#d-16) los tramos, la matriz del polinomio a cada
lado, el estimador y el bootstrap de residuos. Los precios salen de datos/precio_fiscal_por_caso.csv y la UTM
de 2012 de datos/utm_mensual.csv.

Cada repetición r usa la semilla SEMILLA_BASE + r y reinicia el generador; los cuatro procesos usan las mismas
semillas. Orden de extracciones en cada repetición y proceso: normales para z*, uniformes para el Fondo, para la
posición del agolpe, para el mes y para el redondeo, y después las del bootstrap.

Escribe una línea por cifra (clave, valor, objetivo, regla, cumple) y falla con AssertionError si las reglas
incumplidas no son exactamente las registradas para bunching_mc_<lista> en sim_hallazgos.csv. Con un sexto
argumento escribe además una línea por repetición. Contraparte en R: sim_bunching_mc.R.

Uso: python sim_bunching_mc.py precio_fiscal_por_caso.csv utm_mensual.csv sim_hallazgos.csv corta|larga
     salida.csv [repeticiones.csv]
"""
import csv
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sim_bunching as sb  # noqa: E402
from azar import Azar  # noqa: E402
from sim_comun import Registro, regla_deteccion, regla_proporcion, regla_sesgo, semillas  # noqa: E402

SEMILLA_BASE = 34000000
# nombre, n, media de z* menos k, desviación de z*, redondeo
PROCESOS = (("base", 300000, -0.5, 1.0, False),
            ("curvatura", 300000, -0.25, 0.5, False),
            ("ruido", 75000, -0.5, 1.0, False),
            ("redondos", 300000, -0.5, 1.0, True))
PROP_REDONDEA = 0.05
MULTIPLO = 5000000.0
ANIO_UTM = 2012
J0 = sb.CENTRO - 1          # tramo del umbral, [k - 0,02, k)
X_MAX = 10                  # regiones excluidas de j0 - x- a j0 + x+, con x-, x+ en 0..10
ORDENES_BIC = tuple(range(1, 8))
Z_BANDA = 1.96
Z_IC = 1.96
NIVEL = 0.95


def utm_anio(ruta, anio):
    with open(ruta, newline="", encoding="utf-8") as f:
        d = {int(r["mes"]): float(r["utm"]) for r in csv.DictReader(f) if int(r["anio"]) == anio}
    return np.array([d[m] for m in range(1, 13)])


def poli(q):
    """Polinomio de orden q a cada lado de k, sin dummies (columnas de sb.matriz)."""
    j = np.arange(sb.NB)
    s = (j - sb.CENTRO + 0.5) * sb.W
    izq = j < sb.CENTRO
    return np.column_stack([np.where(izq, s ** p, 0.0) for p in range(q + 1)]
                           + [np.where(izq, 0.0, s ** p) for p in range(q + 1)])


POLI = {q: poli(q) for q in ORDENES_BIC}


def elegir_q(c, filas):
    """Orden de menor BIC = n ln(RSS/n) + 2(q+1) ln n con los tramos de filas; ante empate, el menor."""
    n = int(filas.sum())
    mejor = None
    for q in ORDENES_BIC:
        X = POLI[q][filas]
        coef = np.linalg.lstsq(X, c[filas], rcond=None)[0]
        rss = float(((c[filas] - X @ coef) ** 2).sum())
        bic = n * math.log(rss / n) + 2 * (q + 1) * math.log(n)
        if mejor is None or bic < mejor[0]:
            mejor = (bic, q, coef, rss)
    return mejor[1], mejor[2], mejor[3], n


def fuera(a, b):
    filas = np.ones(sb.NB, dtype=bool)
    filas[a:b + 1] = False
    return filas


def ventana_region(c, xm, xp):
    filas = fuera(J0 - xm, J0 + xp)
    q, coef, rss, n = elegir_q(c, filas)
    A = POLI[q]
    X = A[filas]
    s2 = rss / (n - 2 * (q + 1))
    L = np.linalg.cholesky(X.T @ X)
    w = np.linalg.solve(L, A.T)
    banda = A @ coef + Z_BANDA * np.sqrt(s2 * (w * w).sum(axis=0))
    sobre = c > banda
    if not sobre[J0]:
        return J0, J0
    lo, hi = J0, J0
    while lo - 1 >= 0 and sobre[lo - 1]:
        lo -= 1
    while hi + 1 < sb.NB and sobre[hi + 1]:
        hi += 1
    return lo, hi


def seleccionar(c):
    ls, us = [], []
    for xm in range(X_MAX + 1):
        for xp in range(X_MAX + 1):
            lo, hi = ventana_region(c, xm, xp)
            ls.append(lo)
            us.append(hi)
    lo, hi = int(np.median(ls)), int(np.median(us))
    return elegir_q(c, fuera(lo, hi))[0], lo, hi


def simular(az, n, mu, sigma, redondea, ln_razon, utm12):
    k = math.log(1000.0)
    delta = sb.EPSILON * ln_razon
    zs = k + mu + sigma * az.normales(n)
    fondo = az.uniformes(n) < sb.PROP_FONDO
    pos = az.uniformes(n)
    u_mes = az.uniformes(n)
    u_red = az.uniformes(n)
    agolpa = (~fondo) & (zs > k) & (zs <= k + delta)
    z = np.where(fondo | (zs <= k), zs, np.where(agolpa, k - sb.ANCHO_AGOLPE * pos, zs - delta))
    if redondea:
        idx = np.flatnonzero((~agolpa) & (u_red < PROP_REDONDEA))
        mes = np.floor(12 * u_mes[idx]).astype(np.int64)
        for i, m in zip(idx.tolist(), mes.tolist()):
            utm = float(utm12[m])
            pesos = math.floor(math.exp(float(z[i])) * utm / MULTIPLO + 0.5) * MULTIPLO
            if pesos > 0:
                z[i] = math.log(pesos / utm)
    return z, fondo, int(agolpa.sum())


def seleccion_frecuencias(reg, clave, q, lo, hi):
    """Proporción de repeticiones con cada orden y con la ventana del agolpe simulado, [k - 0,04, k)."""
    for o in ORDENES_BIC:
        reg.dato(f"{clave}:q{o}:proporcion", float(np.mean(q == o)))
    reg.dato(f"{clave}:ventana_simulada:proporcion", float(np.mean((lo == J0 - 1) & (hi == J0))))


def main(ruta_precios, ruta_utm, ruta_hallazgos, lista, salida, ruta_rep=None):
    p0, p1 = sb.precios(ruta_precios)
    ln_razon = math.log(p1 / p0)
    k = math.log(1000.0)
    utm12 = utm_anio(ruta_utm, ANIO_UTM)
    sem = semillas(SEMILLA_BASE, lista)
    combos = [(q, lo, hi) for q in sb.ORDENES for lo in sb.INFERIORES for hi in sb.SUPERIORES]

    reg = Registro()
    reg.dato("datos:P0", p0)
    reg.dato("datos:P1", p1)
    reg.dato("datos:repeticiones", len(sem))
    reg.dato("datos:semilla_base", SEMILLA_BASE)
    filas_rep = []
    for nombre, n, mu, sigma, redondea in PROCESOS:
        R = {x: [] for x in ("B_sim", "q", "l", "u", "eps", "B", "ee", "qt", "lt", "ut", "eps_t", "B_t")}
        sens = {cb: [] for cb in combos}
        for r, s in enumerate(sem):
            az = Azar(s)
            z, fondo, b_sim = simular(az, n, mu, sigma, redondea, ln_razon, utm12)
            c_sin = sb.conteos(z[~fondo], k)
            q, lo, hi = seleccionar(c_sin)
            est = sb.estimar(c_sin, q, lo - sb.CENTRO, hi + 1 - sb.CENTRO, ln_razon)
            ee = sb.error_estandar(az, c_sin, q, lo - sb.CENTRO, hi + 1 - sb.CENTRO, ln_razon)
            for cb in combos:
                sens[cb].append(sb.estimar(c_sin, cb[0], cb[1], cb[2], ln_razon)["epsilon"])
            fila = [nombre, r, s, b_sim, q, lo, hi, est["epsilon"], est["B"], ee]
            for x, v in zip(("B_sim", "q", "l", "u", "eps", "B", "ee"), fila[3:]):
                R[x].append(v)
            if nombre == "base":
                c_todos = sb.conteos(z, k)
                qt, lt, ut = seleccionar(c_todos)
                t = sb.estimar(c_todos, qt, lt - sb.CENTRO, ut + 1 - sb.CENTRO, ln_razon)
                for x, v in zip(("qt", "lt", "ut", "eps_t", "B_t"), (qt, lt, ut, t["epsilon"], t["B"])):
                    R[x].append(v)
                fila += [qt, lt, ut, t["epsilon"], t["B"]]
            else:
                fila += [""] * 5
            filas_rep.append(fila)

        A = {x: np.array(v, dtype=np.float64) for x, v in R.items() if v}
        reg.dato(f"{nombre}:datos:n", n)
        reg.dato(f"{nombre}:datos:B_simulado:media", A["B_sim"].mean())
        reg.dato(f"{nombre}:seleccion:q:media", A["q"].mean())
        reg.dato(f"{nombre}:seleccion:inf:media", ((A["l"] - sb.CENTRO) * sb.W).mean())
        reg.dato(f"{nombre}:seleccion:sup:media", ((A["u"] + 1 - sb.CENTRO) * sb.W).mean())
        seleccion_frecuencias(reg, f"{nombre}:seleccion", A["q"], A["l"], A["u"])
        regla_sesgo(reg, f"{nombre}:epsilon", A["eps"], sb.EPSILON)
        regla_sesgo(reg, f"{nombre}:B", A["B"], A["B_sim"])
        valido = np.isfinite(A["ee"]) & (A["ee"] > 0)
        cubre = valido & (np.abs(A["eps"] - sb.EPSILON) <= Z_IC * np.where(valido, A["ee"], 0.0))
        regla_proporcion(reg, f"{nombre}:epsilon:cobertura", cubre, NIVEL, "igual")
        reg.dato(f"{nombre}:epsilon:ee_bootstrap:media", A["ee"][valido].mean() if valido.any() else float("nan"))
        n_inv = int((~valido).sum())
        reg.regla(f"{nombre}:epsilon:ee_bootstrap:invalidos", n_inv, 0, "==objetivo", n_inv == 0)
        for (q, lo, hi), v in sens.items():
            v = np.array(v)
            clave = f"{nombre}:sensibilidad:q{q}:inf{lo * sb.W:+.2f}:sup{hi * sb.W:+.2f}:epsilon"
            reg.dato(f"{clave}:media", v.mean())
            reg.dato(f"{clave}:ee", float(np.std(v, ddof=1)))
        if nombre == "base":
            reg.dato("base:todos:seleccion:q:media", A["qt"].mean())
            reg.dato("base:todos:seleccion:inf:media", ((A["lt"] - sb.CENTRO) * sb.W).mean())
            reg.dato("base:todos:seleccion:sup:media", ((A["ut"] + 1 - sb.CENTRO) * sb.W).mean())
            seleccion_frecuencias(reg, "base:todos:seleccion", A["qt"], A["lt"], A["ut"])
            regla_sesgo(reg, "base:todos:B", A["B_t"], A["B_sim"])
            regla_deteccion(reg, "base:todos:epsilon:deteccion", A["eps_t"], (1 - sb.PROP_FONDO / 2) * sb.EPSILON,
                            "bajo")
            regla_sesgo(reg, "base:todos:epsilon_atenuado", A["eps_t"], (1 - sb.PROP_FONDO) * sb.EPSILON)

    if ruta_rep:
        with open(ruta_rep, "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f, lineterminator="\n")
            w.writerow(["proceso", "r", "semilla", "B_simulado", "q", "l", "u", "epsilon", "B", "ee",
                        "todos_q", "todos_l", "todos_u", "todos_epsilon", "todos_B"])
            w.writerows([[repr(x) if isinstance(x, float) else x for x in f_] for f_ in filas_rep])
    n_fallas = reg.terminar(salida, ruta_hallazgos, f"bunching_mc_{lista}", "d-24")
    print(f"bunching_mc (Python, lista {lista}, {len(sem)} repeticiones): {len(reg.filas)} cifras, "
          f"{n_fallas} reglas incumplidas, registradas como hallazgo")


if __name__ == "__main__":
    main(*sys.argv[1:7])
