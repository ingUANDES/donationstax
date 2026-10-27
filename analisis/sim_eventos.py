"""Simulación de estudio de eventos y diferencias en diferencias, con efecto conocido (BLUEPRINT.md#p-05).

El caso, los estimadores y las tolerancias son los de DECISIONES.md#d-17 («Tolerancias de la simulación de
estudio de eventos y diferencias en diferencias»); las constantes de abajo los implementan. La proporción
de filas sin RUT de cada año sale de datos/rut_completado_por_anio.csv (n_error_medicion / n_filas).
Escribe una línea por cifra (clave, valor, objetivo, regla, cumple) y después falla con AssertionError si
alguna regla no se cumple, salvo los hallazgos registrados (sim_comun.py, Registro.terminar). Contraparte en R: sim_eventos.R.

Identificadores con error por RUT faltante: el donante i tiene el identificador 2i en los años con RUT y
2i + 1 (su nombre) en los años marcados; los años marcados se unen entre sí y no con los demás.

Uso: python sim_eventos.py rut_completado_por_anio.csv sim_hallazgos.csv salida.csv
"""
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
from azar import Azar  # noqa: E402
from sim_comun import Registro, dentro, mco, proporcion_error  # noqa: E402

SEMILLA = 20261028
N = 40000
ANIOS = list(range(2012, 2020))
T0 = 2015
REF = 2014
MU_ALFA_DESDE_K = -1.0     # media de alfa menos ln 1000
SIGMA_ALFA = 1.2
SIGMA_V = 0.6
PENDIENTE_LAMBDA = 0.02
BETA = 0.15
BETA_C = 0.5
TOL = 0.03
TOL_SEGUIMIENTO = 0.02
MIN_SESGO = 0.06


def dummies_anio(anio, anios):
    return [(anio == a).astype(np.float64) for a in anios]


def estudio_eventos(unidad, anio, y, T):
    anios_t = [a for a in ANIOS if a != REF]
    X = np.column_stack(dummies_anio(anio, ANIOS[1:]) + [T * d for d in dummies_anio(anio, anios_t)])
    coef = mco(dentro(y, unidad), dentro(X, unidad))
    return dict(zip(anios_t, coef[len(ANIOS) - 1:].tolist()))


def dif_en_dif(unidad, anio, y, T):
    X = np.column_stack(dummies_anio(anio, ANIOS[1:]) + [T * (anio >= T0)])
    return float(mco(dentro(y, unidad), dentro(X, unidad))[-1])


def sesgo_reversion(mu_alfa, lambda_ref, c):
    s = math.sqrt(SIGMA_ALFA ** 2 + SIGMA_V ** 2)
    a = (c - mu_alfa - lambda_ref) / s
    phi = math.exp(-a * a / 2) / math.sqrt(2 * math.pi)
    Phi = 0.5 * (1 + math.erf(a / math.sqrt(2)))
    return -(SIGMA_V ** 2 / s) * phi * (1 / (1 - Phi) + 1 / Phi)


def comprobar_eventos(reg, nombre, coefs, objetivo):
    for a, g in coefs.items():
        o = objetivo(a)
        reg.regla(f"{nombre}:gamma_{a}", g, o, "abs<=0.03", abs(g - o) <= TOL)


def main(ruta_error, ruta_hallazgos, salida):
    az = Azar(SEMILLA)
    e = proporcion_error(ruta_error)
    k = math.log(1000.0)
    mu_alfa = k + MU_ALFA_DESDE_K
    nt = len(ANIOS)
    alfa = mu_alfa + SIGMA_ALFA * az.normales(N)
    v = SIGMA_V * az.normales(N * nt)
    t_exo = (az.uniformes(N) < 0.5).astype(np.float64)
    u_marca = az.uniformes(N * nt)

    donante = np.repeat(np.arange(N), nt)
    anio = np.tile(np.array(ANIOS), N)
    lam = PENDIENTE_LAMBDA * (anio - 2012)
    post = (anio >= T0).astype(np.float64)
    marca = u_marca < np.array([e[a] for a in anio.tolist()])
    y0 = alfa[donante] + lam + v
    unidad_rut = donante
    unidad_err = 2 * donante + marca.astype(np.int64)

    reg = Registro()
    for a in ANIOS:
        reg.dato(f"datos:e_{a}", e[a])
    reg.dato("datos:filas_marcadas", int(marca.sum()))

    # 1. Tratamiento exógeno
    ya = y0 + BETA * t_exo[donante] * post
    objetivo_1 = lambda a: BETA * (a >= T0)  # noqa: E731
    comprobar_eventos(reg, "exogeno", estudio_eventos(unidad_rut, anio, ya, t_exo[donante]), objetivo_1)

    # 2. Tratamiento por el nivel de 2014
    y2014 = y0[anio == REF]
    t_nivel = (y2014 > k).astype(np.float64)
    yb = y0 + BETA * t_nivel[donante] * post
    b = sesgo_reversion(mu_alfa, PENDIENTE_LAMBDA * (REF - 2012), k)
    reg.regla("nivel_previo:sesgo_b", abs(b), MIN_SESGO, ">=objetivo", abs(b) >= MIN_SESGO)
    objetivo_2 = lambda a: BETA * (a >= T0) + b  # noqa: E731
    comprobar_eventos(reg, "nivel_previo", estudio_eventos(unidad_rut, anio, yb, t_nivel[donante]), objetivo_2)

    # 3. Grupo de control de 200 a 900 UTM, por nivel permanente
    t_c = alfa > k
    g_c = (alfa >= math.log(200.0)) & (alfa <= math.log(900.0))
    contamina = g_c & (alfa > math.log(700.0))
    pi_c = float(contamina.sum() / g_c.sum())
    yc = y0 + (BETA * t_c[donante] + BETA_C * contamina[donante]) * post
    sel = (t_c | g_c)[donante]
    delta = dif_en_dif(unidad_rut[sel], anio[sel], yc[sel], t_c[donante][sel].astype(np.float64))
    reg.dato("control_200_900:pi_c", pi_c)
    reg.regla("control_200_900:sesgo", pi_c * BETA_C, MIN_SESGO, ">=objetivo", pi_c * BETA_C >= MIN_SESGO)
    reg.regla("control_200_900:delta", delta, BETA - pi_c * BETA_C, "abs<=0.03",
              abs(delta - (BETA - pi_c * BETA_C)) <= TOL)

    # 4. Error por RUT faltante
    comprobar_eventos(reg, "exogeno_error_rut", estudio_eventos(unidad_err, anio, ya, t_exo[donante]), objetivo_1)
    tiene_ref = np.zeros(2 * N, dtype=bool)
    t_unidad = np.zeros(2 * N)
    en_ref = anio == REF
    tiene_ref[unidad_err[en_ref]] = True
    t_unidad[unidad_err[en_ref]] = (yb[en_ref] > k).astype(np.float64)
    sel = tiene_ref[unidad_err]
    comprobar_eventos(reg, "nivel_previo_error_rut",
                      estudio_eventos(unidad_err[sel], anio[sel], yb[sel], t_unidad[unidad_err[sel]]), objetivo_2)
    mismo = float(np.mean(unidad_err[anio == 2014] == unidad_err[anio == 2015]))
    obj = (1 - e[2014]) * (1 - e[2015])
    reg.regla("error_rut:mismo_id_2014_2015", mismo, obj, "abs<=0.02", abs(mismo - obj) <= TOL_SEGUIMIENTO)

    n_fallas = reg.terminar(salida, ruta_hallazgos, "eventos", "d-17")
    print(f"eventos (Python): sesgo b {b:.4f}, pi_c {pi_c:.4f}, mismo id 2014-2015 {mismo:.4f}; "
          f"{len(reg.filas)} cifras, {n_fallas} fuera de tolerancia, registradas como hallazgo")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3])
