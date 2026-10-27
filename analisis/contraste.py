"""Contrasta las salidas de Python y R y, si se entrega, un valor esperado calculado a mano.

Falla con AssertionError si difieren más que TOL. Incluye una autoprueba que
comprueba que el contraste sí falla cuando se le entrega una salida alterada.
Por omisión la clave es la columna «caso» y el valor, la columna «precio»; las
filas con el valor vacío no se comparan (en las dos salidas deben ser las mismas).
Con --iguales col1,col2 exige además que esas columnas de texto sean idénticas, fila
a fila y en el mismo orden, en las dos salidas. Con --tol-rel t la tolerancia es
relativa: |a - b| <= t * max(1, |a|) (la de las simulaciones, DECISIONES.md#d-15).

Uso: python contraste.py salida_py.csv salida_R.csv [esperado.csv] [--clave caso] [--columna precio]
     [--iguales col1,col2] [--tol-rel t]
"""
import argparse
import csv

TOL = 1e-12


def cerca(a, b, tol, relativa):
    return abs(a - b) <= (tol * max(1.0, abs(a)) if relativa else tol)


def leer(ruta, col, clave="caso"):
    with open(ruta, newline="", encoding="utf-8") as f:
        filas = list(csv.DictReader(f))
    out = {r[clave]: float(r[col]) for r in filas if r[col] != ""}
    assert len(out) == sum(1 for r in filas if r[col] != ""), f"{ruta}: clave {clave!r} repetida"
    return out


def comparar(py, r, esperado=None, tol=TOL, relativa=False):
    assert py.keys() == r.keys(), f"casos distintos: py={sorted(py)} R={sorted(r)}"
    if esperado is not None:
        assert py.keys() == esperado.keys(), f"casos distintos: py={sorted(py)} esperado={sorted(esperado)}"
    for caso in py:
        assert cerca(py[caso], r[caso], tol, relativa), (
            f"Python y R difieren en {caso}: {py[caso]!r} vs {r[caso]!r}"
        )
        if esperado is not None:
            assert cerca(py[caso], esperado[caso], tol, relativa), (
                f"{caso}: salida {py[caso]!r} vs esperado a mano {esperado[caso]!r}"
            )


def iguales(ruta_py, ruta_r, columnas):
    with open(ruta_py, newline="", encoding="utf-8") as f:
        a = list(csv.DictReader(f))
    with open(ruta_r, newline="", encoding="utf-8") as f:
        b = list(csv.DictReader(f))
    assert len(a) == len(b), f"número de filas distinto: py={len(a)} R={len(b)}"
    for i, (x, y) in enumerate(zip(a, b), start=2):
        for c in columnas:
            assert x[c] == y[c], f"línea {i}, columna {c!r}: py={x[c]!r} R={y[c]!r}"
    return len(a)


def autoprueba(py, r, esperado=None, tol=TOL, relativa=False):
    alterado = dict(r)
    caso = next(iter(alterado))
    alterado[caso] += 1e-6 * max(1.0, abs(alterado[caso]))
    try:
        comparar(py, alterado, esperado, tol, relativa)
    except AssertionError:
        return
    raise AssertionError("el contraste no detectó una alteración relativa de 1e-6")


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("salida_py")
    p.add_argument("salida_r")
    p.add_argument("esperado", nargs="?")
    p.add_argument("--clave", default="caso")
    p.add_argument("--columna", default="precio")
    p.add_argument("--iguales", default="")
    p.add_argument("--tol-rel", type=float, default=None)
    a = p.parse_args()
    relativa = a.tol_rel is not None
    tol = a.tol_rel if relativa else TOL
    py = leer(a.salida_py, a.columna, a.clave)
    r = leer(a.salida_r, a.columna, a.clave)
    esperado = leer(a.esperado, a.columna, a.clave) if a.esperado else None
    comparar(py, r, esperado, tol, relativa)
    autoprueba(py, r, esperado, tol, relativa)
    quien = "Python = R = esperado" if esperado is not None else "Python = R"
    print(f"contraste OK: {len(py)} {a.clave} en {a.columna!r}, {quien} (tol {'relativa ' if relativa else ''}{tol})")
    if a.iguales:
        cols = a.iguales.split(",")
        n = iguales(a.salida_py, a.salida_r, cols)
        print(f"contraste OK: {n} filas con {len(cols)} columnas de texto idénticas en Python y R")
