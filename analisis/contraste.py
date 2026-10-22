"""Contrasta las salidas de Python y R contra un valor esperado calculado a mano.

Falla con AssertionError si difieren más que TOL. Incluye una autoprueba que
comprueba que el contraste sí falla cuando se le entrega una salida alterada.

Uso: python contraste.py salida_py.csv salida_R.csv esperado.csv
"""
import csv
import sys

TOL = 1e-12


def leer(ruta, col):
    with open(ruta, newline="", encoding="utf-8") as f:
        return {r["caso"]: float(r[col]) for r in csv.DictReader(f)}


def comparar(py, r, esperado):
    assert py.keys() == r.keys() == esperado.keys(), (
        f"casos distintos: py={sorted(py)} R={sorted(r)} esperado={sorted(esperado)}"
    )
    for caso in esperado:
        assert abs(py[caso] - r[caso]) <= TOL, (
            f"Python y R difieren en {caso}: {py[caso]!r} vs {r[caso]!r}"
        )
        assert abs(py[caso] - esperado[caso]) <= TOL, (
            f"{caso}: salida {py[caso]!r} vs esperado a mano {esperado[caso]!r}"
        )


def autoprueba(py, r, esperado):
    alterado = dict(r)
    caso = next(iter(alterado))
    alterado[caso] += 1e-6
    try:
        comparar(py, alterado, esperado)
    except AssertionError:
        return
    raise AssertionError("el contraste no detectó una alteración de 1e-6")


if __name__ == "__main__":
    py = leer(sys.argv[1], "precio")
    r = leer(sys.argv[2], "precio")
    esperado = leer(sys.argv[3], "precio")
    comparar(py, r, esperado)
    autoprueba(py, r, esperado)
    print(f"contraste OK: {len(esperado)} casos, Python = R = esperado (tol {TOL})")
