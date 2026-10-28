"""Monte Carlo del estudio de eventos, con cobertura y prueba de tendencias previas (BLUEPRINT.md#p-05).

Casos, proceso generador, orden de extracciones y estimadores: los de DECISIONES.md#d-17, casos 1 a 4, que
implementa sim_eventos.py. Varianza robusta por conglomerado, intervalos, prueba de Wald de tendencias previas y
tolerancias: DECISIONES.md#d-25 («Monte Carlo del estudio de eventos, con cobertura y prueba de tendencias
previas»); método común: DECISIONES.md#d-19. La proporción de filas sin RUT de cada año sale de
datos/rut_completado_por_anio.csv.

Cada repetición r usa la semilla SEMILLA_BASE + r y reinicia el generador. Escribe una línea por cifra y falla
con AssertionError si las reglas incumplidas no son exactamente las registradas para eventos_mc_<lista> en
sim_hallazgos.csv. Con un quinto argumento escribe además una línea por repetición. Contraparte en R:
sim_eventos_mc.R.

Uso: python sim_eventos_mc.py rut_completado_por_anio.csv sim_hallazgos.csv corta|larga salida.csv
     [repeticiones.csv]
"""
import csv
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
import sim_eventos as se  # noqa: E402
from azar import Azar  # noqa: E402
from sim_comun import (Registro, dentro, mco, proporcion_error, regla_deteccion, regla_proporcion,  # noqa: E402
                       regla_sesgo, semillas)

SEMILLA_BASE = 34100000
ANIOS_T = [a for a in se.ANIOS if a != se.REF]
PRE = [0, 1]                       # posiciones de 2012 y 2013 en ANIOS_T
CRITICO_WALD = -2 * math.log(0.05)  # cuantil 0,95 de chi cuadrado con 2 grados de libertad
Z = 1.96
POTENCIA = 0.80


def mco_conglomerado(unidad, y, X):
    """MCO con efectos fijos de unidad y varianza CR1 por unidad (DECISIONES.md#d-25, punto 1)."""
    Xd = dentro(X, unidad)
    yd = dentro(y, unidad)
    b = mco(yd, Xd)
    u = yd - Xd @ b
    _, cod = np.unique(unidad, return_inverse=True)
    G = int(cod.max()) + 1
    n, K = Xd.shape
    S = np.column_stack([np.bincount(cod, weights=Xd[:, j] * u, minlength=G) for j in range(K)])
    XtXi = np.linalg.inv(Xd.T @ Xd)
    V = (G / (G - 1)) * ((n - 1) / (n - K)) * (XtXi @ (S.T @ S) @ XtXi)
    return b, V


def eventos(unidad, anio, y, T):
    X = np.column_stack(se.dummies_anio(anio, se.ANIOS[1:]) + [T * d for d in se.dummies_anio(anio, ANIOS_T)])
    b, V = mco_conglomerado(unidad, y, X)
    m = len(se.ANIOS) - 1
    g, Vg = b[m:], V[m:, m:]
    ee = np.sqrt(np.diag(Vg))
    gp = g[PRE]
    wald = float(gp @ np.linalg.solve(Vg[np.ix_(PRE, PRE)], gp))
    individual = bool(np.any(np.abs(gp) > Z * ee[PRE]))
    return g, ee, wald > CRITICO_WALD, individual


def dif_en_dif(unidad, anio, y, T):
    X = np.column_stack(se.dummies_anio(anio, se.ANIOS[1:]) + [T * (anio >= se.T0)])
    b, V = mco_conglomerado(unidad, y, X)
    return float(b[-1]), float(math.sqrt(V[-1, -1]))


def main(ruta_error, ruta_hallazgos, lista, salida, ruta_rep=None):
    e = proporcion_error(ruta_error)
    k = math.log(1000.0)
    mu_alfa = k + se.MU_ALFA_DESDE_K
    nt = len(se.ANIOS)
    N = se.N
    sem = semillas(SEMILLA_BASE, lista)
    donante = np.repeat(np.arange(N), nt)
    anio = np.tile(np.array(se.ANIOS), N)
    lam = se.PENDIENTE_LAMBDA * (anio - 2012)
    post = (anio >= se.T0).astype(np.float64)
    e_fila = np.array([e[a] for a in anio.tolist()])
    en_ref = anio == se.REF
    b = se.sesgo_reversion(mu_alfa, se.PENDIENTE_LAMBDA * (se.REF - 2012), k)
    beta_post = np.array([se.BETA * (a >= se.T0) for a in ANIOS_T])
    casos = ("exogeno", "nivel_previo", "exogeno_error_rut", "nivel_previo_error_rut")
    res = {c: {"g": [], "ee": [], "wald": [], "ind": []} for c in casos}
    ctl = {"delta": [], "ee": [], "pi_c": []}
    mismo = []
    filas_rep = []

    for r, s in enumerate(sem):
        az = Azar(s)
        alfa = mu_alfa + se.SIGMA_ALFA * az.normales(N)
        v = se.SIGMA_V * az.normales(N * nt)
        t_exo = (az.uniformes(N) < 0.5).astype(np.float64)
        u_marca = az.uniformes(N * nt)
        marca = u_marca < e_fila
        y0 = alfa[donante] + lam + v
        unidad_err = 2 * donante + marca.astype(np.int64)

        ya = y0 + se.BETA * t_exo[donante] * post
        t_nivel = (y0[en_ref] > k).astype(np.float64)
        yb = y0 + se.BETA * t_nivel[donante] * post
        salida_r = {"exogeno": eventos(donante, anio, ya, t_exo[donante]),
                    "nivel_previo": eventos(donante, anio, yb, t_nivel[donante]),
                    "exogeno_error_rut": eventos(unidad_err, anio, ya, t_exo[donante])}
        tiene_ref = np.zeros(2 * N, dtype=bool)
        t_unidad = np.zeros(2 * N)
        tiene_ref[unidad_err[en_ref]] = True
        t_unidad[unidad_err[en_ref]] = (yb[en_ref] > k).astype(np.float64)
        sel = tiene_ref[unidad_err]
        salida_r["nivel_previo_error_rut"] = eventos(unidad_err[sel], anio[sel], yb[sel], t_unidad[unidad_err[sel]])
        for c in casos:
            g, ee, wald, ind = salida_r[c]
            res[c]["g"].append(g)
            res[c]["ee"].append(ee)
            res[c]["wald"].append(wald)
            res[c]["ind"].append(ind)

        t_c = alfa > k
        g_c = (alfa >= math.log(200.0)) & (alfa <= math.log(900.0))
        contamina = g_c & (alfa > math.log(700.0))
        pi_c = float(contamina.sum() / g_c.sum())
        yc = y0 + (se.BETA * t_c[donante] + se.BETA_C * contamina[donante]) * post
        selc = (t_c | g_c)[donante]
        delta, ee_d = dif_en_dif(donante[selc], anio[selc], yc[selc], t_c[donante][selc].astype(np.float64))
        ctl["delta"].append(delta)
        ctl["ee"].append(ee_d)
        ctl["pi_c"].append(pi_c)
        mismo.append(float(np.mean(unidad_err[anio == 2014] == unidad_err[anio == 2015])))
        filas_rep.append([r, s] + [x for c in casos for x in salida_r[c][0].tolist()]
                         + [int(salida_r[c][2]) for c in casos] + [delta, ee_d, pi_c, mismo[-1]])

    reg = Registro()
    reg.dato("datos:repeticiones", len(sem))
    reg.dato("datos:semilla_base", SEMILLA_BASE)
    for a in se.ANIOS:
        reg.dato(f"datos:e_{a}", e[a])
    reg.regla("nivel_previo:sesgo_b", abs(b), se.MIN_SESGO, ">=objetivo", abs(b) >= se.MIN_SESGO)
    for c in casos:
        G = np.array(res[c]["g"])
        EE = np.array(res[c]["ee"])
        nivel = c.startswith("nivel_previo")
        obj = beta_post + (b if nivel else 0.0)
        for i, a in enumerate(ANIOS_T):
            regla_sesgo(reg, f"{c}:gamma_{a}", G[:, i], obj[i])
            cubre = np.abs(G[:, i] - beta_post[i]) <= Z * EE[:, i]
            regla_proporcion(reg, f"{c}:gamma_{a}:cobertura", cubre, 0.95, None if nivel else "igual")
            reg.dato(f"{c}:gamma_{a}:ee_conglomerado:media", EE[:, i].mean())
        if nivel:
            regla_proporcion(reg, f"{c}:pretendencias:wald", res[c]["wald"], POTENCIA, "potencia")
        else:
            regla_proporcion(reg, f"{c}:pretendencias:wald", res[c]["wald"], 0.05, "igual")
        regla_proporcion(reg, f"{c}:pretendencias:individual", res[c]["ind"], 0.05, None)

    d, ed, pc = (np.array(ctl[x]) for x in ("delta", "ee", "pi_c"))
    reg.dato("control_200_900:pi_c:media", pc.mean())
    regla_sesgo(reg, "control_200_900:delta", d, se.BETA - pc * se.BETA_C)
    regla_deteccion(reg, "control_200_900:pi_c_beta_c", pc * se.BETA_C, se.MIN_SESGO, "sobre")
    regla_proporcion(reg, "control_200_900:delta:cobertura_beta", np.abs(d - se.BETA) <= Z * ed, 0.95, None)
    regla_proporcion(reg, "control_200_900:delta:cobertura_contaminado",
                     np.abs(d - (se.BETA - pc * se.BETA_C)) <= Z * ed, 0.95, None)
    reg.dato("control_200_900:delta:ee_conglomerado:media", ed.mean())
    regla_sesgo(reg, "error_rut:mismo_id_2014_2015", np.array(mismo), (1 - e[2014]) * (1 - e[2015]))

    if ruta_rep:
        with open(ruta_rep, "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f, lineterminator="\n")
            w.writerow(["r", "semilla"] + [f"{c}_gamma_{a}" for c in casos for a in ANIOS_T]
                       + [f"{c}_wald" for c in casos] + ["delta", "ee_delta", "pi_c", "mismo_id_2014_2015"])
            w.writerows([[repr(x) if isinstance(x, float) else x for x in f_] for f_ in filas_rep])
    n_fallas = reg.terminar(salida, ruta_hallazgos, f"eventos_mc_{lista}", "d-25")
    print(f"eventos_mc (Python, lista {lista}, {len(sem)} repeticiones): {len(reg.filas)} cifras, "
          f"{n_fallas} reglas incumplidas, registradas como hallazgo")


if __name__ == "__main__":
    main(*sys.argv[1:6])
