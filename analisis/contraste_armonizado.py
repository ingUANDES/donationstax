"""Contrasta, fila por fila, las planillas armonizadas en Python y en R (armonizar.py y armonizar.R).

Exige los mismos años, el mismo encabezado y, en cada año, las mismas filas con los mismos valores.
La comparación es exacta, como texto: los montos van con 17 cifras significativas en los dos
lenguajes. Falla con AssertionError e informa, por columna, cuántas celdas difieren y los primeros
ejemplos (año, fila). Incluye una autoprueba: alterar una celda del primer año debe hacerlo fallar.

Uso: python contraste_armonizado.py <carpeta_py> <carpeta_R>
"""
import csv
import itertools
import sys
from collections import Counter, defaultdict
from pathlib import Path

csv.field_size_limit(sys.maxsize)
FALTA = object()


def filas(ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        yield from csv.reader(f)


def comparar(py, r, anio, diferencias, ejemplos):
    """Compara dos iterables de filas; acumula las diferencias por columna."""
    py, r = iter(py), iter(r)
    enc_py, enc_r = next(py), next(r)
    assert enc_py == enc_r, f"{anio}: encabezados distintos: {enc_py} vs {enc_r}"
    for i, (a, b) in enumerate(itertools.zip_longest(py, r, fillvalue=FALTA), start=1):
        assert a is not FALTA and b is not FALTA, f"{anio}: número de filas distinto (se acaba en la fila {i})"
        if a != b:
            for c, va, vb in zip(enc_py, a, b):
                if va != vb:
                    diferencias[c] += 1
                    if len(ejemplos[c]) < 3:
                        ejemplos[c].append((anio, a[1], va, vb))


def contrastar(carpeta_py, carpeta_r):
    anios_py = sorted(p.stem for p in Path(carpeta_py).glob("*.csv"))
    anios_r = sorted(p.stem for p in Path(carpeta_r).glob("*.csv"))
    assert anios_py and anios_py == anios_r, f"años distintos: py={anios_py} R={anios_r}"
    diferencias, ejemplos, n = Counter(), defaultdict(list), 0
    for a in anios_py:
        comparar(filas(Path(carpeta_py) / f"{a}.csv"), filas(Path(carpeta_r) / f"{a}.csv"), a, diferencias, ejemplos)
        n += 1
    detalle = "; ".join(f"{c}: {k} celdas, p. ej. {ejemplos[c]}" for c, k in diferencias.most_common())
    assert not diferencias, f"Python y R difieren en {sum(diferencias.values())} celdas. {detalle}"
    return anios_py


def autoprueba(carpeta_py, carpeta_r, anio):
    py = list(filas(Path(carpeta_py) / f"{anio}.csv"))
    r = [list(f) for f in filas(Path(carpeta_r) / f"{anio}.csv")]
    assert len(r) > 1, f"{anio}: sin filas para la autoprueba"
    r[1][-1] = r[1][-1] + "x"
    try:
        d = Counter()
        comparar(py, r, anio, d, defaultdict(list))
        assert not d
    except AssertionError:
        return
    raise AssertionError("el contraste no detectó una celda alterada")


if __name__ == "__main__":
    anios = contrastar(sys.argv[1], sys.argv[2])
    autoprueba(sys.argv[1], sys.argv[2], anios[0])
    print(f"contraste fila a fila OK: {len(anios)} años, Python = R en todas las celdas")
