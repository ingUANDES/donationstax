"""Evalúa el año de la donación en las planillas armonizadas de la Ley 19.885 (D-13, punto 1, y D-14).

Lee las filas de armonizar.py (<carpeta_armonizado>/<año>.csv) y escribe dos tablas, sobre las filas
incluidas:
- <por_anio.csv>, una fila por año y una fila «total»: de dónde puede salir el año (fecha válida,
  columna `AÑO`, archivo), cuántas filas coinciden entre esas fuentes, cuántas filas con fecha válida
  tienen la fecha en ese año (de cualquier archivo) y, para cada año calendario,
  cuántas filas y qué monto le tocan con el año del archivo (D-14) y con el de la fecha válida (la
  regla 4 de D-10, que D-14 reemplazó; sin fecha válida, el del archivo).
- <conflictos.csv>, una fila por año del archivo, diferencia de años (fecha − archivo) y mes de la
  fecha, solo con las filas cuya fecha válida cae en otro año: cuántas son, en cuántas `AÑO` coincide
  con el archivo o con la fecha, y su monto.
`AÑO` es válido si es entero y está entre 2008 y 2025, como en armonizar.py. La contraparte en R es
evalua_anio.R; contraste_datos.py compara las dos salidas.

Uso: python evalua_anio.py <carpeta_armonizado> <por_anio.csv> <conflictos.csv>
"""
import csv
import math
import sys
from collections import Counter, defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from esquema_datos import num  # noqa: E402

csv.field_size_limit(sys.maxsize)
CONTEOS = [
    "n_incluidas", "n_sin_fecha_valida", "n_sin_fecha_anio_columna_igual_archivo", "n_con_fecha_valida",
    "n_con_fecha_en_anio", "n_con_fecha_anio_columna_igual_archivo", "n_fecha_mismo_anio", "n_fecha_otro_anio",
    "n_otro_anio_columna_igual_archivo", "n_otro_anio_columna_igual_fecha", "n_otro_anio_columna_otro",
    "n_anio_columna_no_valido", "n_filas_anio_archivo", "n_filas_anio_fecha",
]
CAMPOS_ANIO = ["anio"] + CONTEOS + ["suma_monto_anio_archivo", "suma_monto_anio_fecha"]
CAMPOS_CONF = ["clave", "anio_archivo", "diferencia", "mes_fecha", "n_filas", "n_anio_columna_igual_archivo",
               "n_anio_columna_igual_fecha", "suma_monto_total"]


def g17(x):
    return f"{x:.17g}"


def anio_columna(v):
    a = num(v)
    return int(a) if a is not None and a == int(a) and 2008 <= a <= 2025 else None


def main(carpeta, por_anio, conflictos):
    c = defaultdict(Counter)
    montos = defaultdict(lambda: defaultdict(list))
    conf = defaultdict(Counter)
    conf_monto = defaultdict(list)
    for p in sorted(Path(carpeta).glob("[0-9][0-9][0-9][0-9].csv")):
        with open(p, newline="", encoding="utf-8") as f:
            for r in csv.DictReader(f):
                if r["incluida"] != "1":
                    continue
                a = int(r["anio_archivo"])
                col = anio_columna(r["anio_columna"])
                m = float(r["monto_total"])
                fa = int(r["anio_fecha"]) if r["anio_fecha"] else None
                k = c[a]
                k["n_incluidas"] += 1
                k["n_anio_columna_no_valido"] += col is None
                k["n_filas_anio_archivo"] += 1
                montos[a]["archivo"].append(m)
                alt = fa if fa is not None else a
                c[alt]["n_filas_anio_fecha"] += 1
                montos[alt]["fecha"].append(m)
                if fa is None:
                    k["n_sin_fecha_valida"] += 1
                    k["n_sin_fecha_anio_columna_igual_archivo"] += col == a
                    continue
                k["n_con_fecha_valida"] += 1
                c[fa]["n_con_fecha_en_anio"] += 1
                k["n_con_fecha_anio_columna_igual_archivo"] += col == a
                if fa == a:
                    k["n_fecha_mismo_anio"] += 1
                    continue
                k["n_fecha_otro_anio"] += 1
                k["n_otro_anio_columna_igual_archivo"] += col == a
                k["n_otro_anio_columna_igual_fecha"] += col == fa
                k["n_otro_anio_columna_otro"] += col != a and col != fa
                mes = int(r["fecha_donacion"][5:7])
                clave = (a, fa - a, mes)
                conf[clave]["n_filas"] += 1
                conf[clave]["n_anio_columna_igual_archivo"] += col == a
                conf[clave]["n_anio_columna_igual_fecha"] += col == fa
                conf_monto[clave].append(m)
    filas = []
    for a in sorted(c):
        fila = {"anio": a, **{k: c[a][k] for k in CONTEOS}}
        fila["suma_monto_anio_archivo"] = g17(math.fsum(montos[a]["archivo"]))
        fila["suma_monto_anio_fecha"] = g17(math.fsum(montos[a]["fecha"]))
        filas.append(fila)
    total = {"anio": "total", **{k: sum(c[a][k] for a in c) for k in CONTEOS}}
    total["suma_monto_anio_archivo"] = g17(math.fsum(m for a in montos for m in montos[a]["archivo"]))
    total["suma_monto_anio_fecha"] = g17(math.fsum(m for a in montos for m in montos[a]["fecha"]))
    filas.append(total)
    with open(por_anio, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=CAMPOS_ANIO)
        w.writeheader()
        w.writerows(filas)
    with open(conflictos, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=CAMPOS_CONF)
        w.writeheader()
        for (a, d, mes) in sorted(conf):
            k = conf[(a, d, mes)]
            w.writerow({"clave": f"{a}|{d:+d}|{mes:02d}", "anio_archivo": a, "diferencia": d, "mes_fecha": mes,
                        "n_filas": k["n_filas"], "n_anio_columna_igual_archivo": k["n_anio_columna_igual_archivo"],
                        "n_anio_columna_igual_fecha": k["n_anio_columna_igual_fecha"],
                        "suma_monto_total": g17(math.fsum(conf_monto[(a, d, mes)]))})


if __name__ == "__main__":
    main(*sys.argv[1:4])
