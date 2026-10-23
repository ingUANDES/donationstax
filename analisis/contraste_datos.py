"""Contrasta los perfiles de esquema calculados en Python y en R.

Todas las columnas deben coincidir exactamente salvo las sumas de montos, que se comparan con
tolerancia relativa TOL_REL. Falla con AssertionError. Incluye una autoprueba: una alteración de un
conteo o de una suma por encima de la tolerancia debe hacer fallar el contraste.

Uso: python contraste_datos.py perfil_py.csv perfil_R.csv
"""
import csv
import sys

TOL_REL = 1e-9
SUMAS = ("suma_monto_total", "suma_monto_institucion", "suma_monto_fm")


def leer(ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        return {r["anio"]: r for r in csv.DictReader(f)}


def comparar(py, r):
    assert py.keys() == r.keys(), f"años distintos: py={sorted(py)} R={sorted(r)}"
    for anio in py:
        assert py[anio].keys() == r[anio].keys(), f"{anio}: columnas distintas"
        for c, vp in py[anio].items():
            vr = r[anio][c]
            if c in SUMAS:
                a, b = float(vp), float(vr)
                assert abs(a - b) <= TOL_REL * max(1.0, abs(a), abs(b)), f"{anio} {c}: Python {vp} vs R {vr}"
            else:
                assert vp == vr, f"{anio} {c}: Python {vp!r} vs R {vr!r}"


def autoprueba(py, r):
    anio = next(iter(r))
    for campo, nuevo in (("n_filas_datos", "-1"), ("suma_monto_total", "1e30")):
        alterado = {a: dict(f) for a, f in r.items()}
        alterado[anio][campo] = nuevo
        try:
            comparar(py, alterado)
        except AssertionError:
            continue
        raise AssertionError(f"el contraste no detectó una alteración en {campo}")


if __name__ == "__main__":
    py, r = leer(sys.argv[1]), leer(sys.argv[2])
    comparar(py, r)
    autoprueba(py, r)
    print(f"contraste de esquema OK: {len(py)} años, Python = R (sumas con tolerancia relativa {TOL_REL})")
