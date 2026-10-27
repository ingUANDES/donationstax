"""Contrasta los perfiles de esquema calculados en Python y en R.

Sirve para los perfiles de las planillas de la Ley 19.885 (esquema_datos.py y .R), de la nómina de
la Ley 21.440 (esquema_ley21440.py y .R), para su cruce (cruce_donatarias.py y .R) y para las tablas
de la armonización (armonizar.py y .R). Las filas se emparejan por la primera columna. Todas las columnas deben coincidir exactamente salvo las
sumas de montos (columnas cuyo nombre empieza por «suma_»), que se comparan con tolerancia relativa
TOL_REL. Falla con AssertionError. Incluye una autoprueba: una alteración de un conteo o de una
suma por encima de la tolerancia debe hacer fallar el contraste (si la tabla no tiene
conteos, se altera su última columna). Con un tercer archivo (valores calculados a mano sobre una
tabla de prueba, como en analisis/fixtures/duplicados_esperado_*.csv), exige además que la salida de
Python coincida con él con las mismas reglas.

Uso: python contraste_datos.py perfil_py.csv perfil_R.csv [esperado.csv]
"""
import csv
import sys

TOL_REL = 1e-9


def leer(ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        lector = csv.DictReader(f)
        return {r[lector.fieldnames[0]]: r for r in lector}


def comparar(py, r):
    assert py.keys() == r.keys(), f"filas distintas: py={sorted(py)} R={sorted(r)}"
    for anio in py:
        assert py[anio].keys() == r[anio].keys(), f"{anio}: columnas distintas"
        for c, vp in py[anio].items():
            vr = r[anio][c]
            if c.startswith("suma_"):
                a, b = float(vp), float(vr)
                assert abs(a - b) <= TOL_REL * max(1.0, abs(a), abs(b)), f"{anio} {c}: Python {vp} vs R {vr}"
            else:
                assert vp == vr, f"{anio} {c}: Python {vp!r} vs R {vr!r}"


def autoprueba(py, r):
    anio = next(iter(r))
    campos = list(r[anio])
    conteo = next((c for c in campos if c.startswith("n_")), campos[-1])
    suma = next((c for c in campos if c.startswith("suma_")), None)
    for campo, nuevo in ((conteo, "-1"), (suma, "1e30")):
        if campo is None:
            continue
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
    quien = "Python = R"
    if len(sys.argv) > 3:
        esperado = leer(sys.argv[3])
        comparar(py, esperado)
        autoprueba(py, esperado)
        quien = "Python = R = esperado a mano"
    print(f"contraste OK: {len(py)} filas (una por año o período), {quien} (sumas con tolerancia relativa {TOL_REL})")
