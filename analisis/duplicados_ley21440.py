"""Duplicados exactos de la nómina de donaciones de la Ley 21.440 (https://github.com/ingUANDES/donationstax/issues/16).

Escribe tres tablas; la contraparte en R es duplicados_ley21440.R y contraste_datos.py exige que
coincidan. Nada se excluye: las tablas describen todas las filas de datos.

- Por año, más una fila con la suma de los años: filas de datos, filas distintas, copias extra
  (filas menos filas distintas; es `n_duplicados_exactos` de esquema_ley21440_por_anio.csv), grupos
  repetidos, filas repetidas (todas las copias de las filas que aparecen más de una vez) y sin
  repetir, multiplicidad máxima y mediana, y la suma de Valor en cada conjunto de filas.
  `suma_valor_sin_duplicados` cuenta una vez cada fila distinta: es la sensibilidad del monto sin
  duplicados.
- Multiplicidad: por año y número de copias k, cuántas filas distintas aparecen k veces, cuántas
  filas son y cuánto suman.
- Concentración: por año y dimensión (donataria, fecha, par donataria y fecha, valor), en cuántas
  categorías caen las copias extra, qué parte reúnen las 1, 5 y 10 categorías con más copias y
  cuántas categorías hacen falta para reunir la mitad y el 90 %; lo mismo con todas las filas, como
  referencia.

Reglas:
- Filas de datos y reglas de lectura: las de esquema_ley21440.py, que toma las de esquema_datos.py.
- Duplicado exacto: dos filas de datos del mismo archivo con el mismo texto en todas las columnas
  (en 2024 y 2025 incluyen Nac/Ext.). Una fila solo se compara con las de su archivo; la fila de la
  suma de los años suma los conteos y montos de cada año.
- Multiplicidad de una fila: cuántas filas de datos de su archivo son iguales a ella, ella incluida.
  Copia extra: cada fila igual a una anterior del mismo archivo (todas menos la primera de cada
  grupo); una fila distinta con multiplicidad k tiene k − 1 copias extra.
- Mediana de la multiplicidad: sobre las filas repetidas (k ≥ 2), cada una con la k de su fila.
- Categoría de una fila: el texto de la celda del RUT de la donataria, el de Fecha (número de serie
  de Excel), el par de los dos o el de Valor. Las k categorías con más copias (o filas): la suma de
  los k conteos mayores, que no depende del orden de los empates. `n_categorias_mitad_*` y
  `n_categorias_90_*`: el menor número de categorías que reúne al menos el 50 % y el 90 %, con
  comparación entera (2·acumulado ≥ total; 10·acumulado ≥ 9·total).
- `categoria_top1_copias`: la fecha (ISO) o el Valor con más copias extra; entre empates, el de menor
  valor numérico. Vacía en las dimensiones donataria y par donataria y fecha: no se publica el RUT de
  ninguna donataria.
- Proporciones: cociente de dos conteos o de dos sumas, con 17 cifras («%.17g», igual que en R);
  vacías si el denominador es 0. Los montos de Valor son enteros, así que sus sumas son exactas en los
  dos lenguajes.

Modo de prueba (--csv): lee un CSV con una columna `anio` y los encabezados de la nómina en vez de
los .xlsx. `make verificar` lo corre sobre analisis/fixtures/duplicados_prueba.csv (ficticio) y
compara con los valores calculados a mano de analisis/fixtures/duplicados_esperado_*.csv; en ese modo
no hace falta python-calamine.

Con --perfil, exige que las filas de datos, las copias extra y la suma de Valor de cada año sean las
del perfil versionado (datos/esquema_ley21440_por_anio.csv), y que el total de la fila superior de
cada archivo sea la suma con duplicados.

Uso: python duplicados_ley21440.py <carpeta_xlsx> <por_anio.csv> <multiplicidad.csv> <concentracion.csv>
         [--perfil esquema_ley21440_por_anio.csv]
     python duplicados_ley21440.py --csv <prueba.csv> <por_anio.csv> <multiplicidad.csv> <concentracion.csv>
"""
import argparse
import csv
import math
import statistics
import sys
from collections import Counter, defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import esquema_ley21440 as e21  # noqa: E402
from esquema_datos import norm, num, roles, serial_a_fecha  # noqa: E402

CAMPOS_ANIO = [
    "anio", "n_filas_datos", "n_filas_distintas", "n_duplicados_exactos", "n_grupos_repetidos",
    "n_filas_repetidas", "n_filas_sin_repetir", "multiplicidad_max", "multiplicidad_mediana_filas_repetidas",
    "suma_valor", "suma_valor_sin_duplicados", "suma_valor_copias_extra", "suma_valor_filas_repetidas",
    "suma_valor_filas_sin_repetir", "prop_filas_copias_extra", "prop_valor_copias_extra",
]
CAMPOS_MULT = ["clave", "anio", "multiplicidad", "n_filas_distintas", "n_filas", "suma_valor"]
CAMPOS_CONC = [
    "clave", "anio", "dimension", "n_categorias", "n_categorias_con_copias", "n_copias_extra",
    "prop_copias_top1", "prop_copias_top5", "prop_copias_top10", "n_categorias_mitad_copias",
    "n_categorias_90_copias", "categoria_top1_copias", "prop_filas_top1", "prop_filas_top5", "prop_filas_top10",
    "n_categorias_mitad_filas", "n_categorias_90_filas",
]
CON_TOP1 = ("fecha", "valor")
DIMENSIONES = ("donataria", "fecha", "donataria_fecha", "valor")
TOPS = (1, 5, 10)
TOL_REL = 1e-9


def g17(x):
    return f"{x:.17g}"


def prop(a, b):
    return g17(a / b) if b else ""


def filas_xlsx(carpeta):
    """{año: (filas de datos como tuplas, índices de columnas)} desde <carpeta>/<año>.xlsx."""
    out = {}
    for p in sorted(Path(carpeta).glob("[0-9][0-9][0-9][0-9].xlsx")):
        _, fila_enc, datos, _, idx = e21.leer(p)
        out[int(p.stem)] = ([tuple(f) for f in datos[fila_enc + 1:] if any(f) and e21.es_fila_datos(f, idx)], idx)
    assert out, f"{carpeta}: no hay archivos AAAA.xlsx"
    return out


def filas_csv(ruta):
    """Lo mismo desde un CSV de prueba con una columna `anio` y los encabezados de la nómina."""
    with open(ruta, newline="", encoding="utf-8") as f:
        lector = csv.reader(f)
        enc = next(lector)
        filas = list(lector)
    ia = enc.index("anio")
    resto = [j for j in range(len(enc)) if j != ia]
    idx = roles([norm(enc[j]) for j in resto], e21.ROLES)
    faltan = [r for r in ("rut", "donataria", "valor", "fecha") if idx[r] is None]
    assert not faltan, f"{ruta}: faltan las columnas {faltan}"
    por_anio = defaultdict(list)
    for i, f in enumerate(filas, start=2):
        assert len(f) == len(enc), f"{ruta}:{i}: {len(f)} columnas, el encabezado tiene {len(enc)}"
        t = tuple(f[j] for j in resto)
        if any(t) and e21.es_fila_datos(t, idx):
            por_anio[int(f[ia])].append(t)
    return {a: (por_anio[a], idx) for a in sorted(por_anio)}


def grupos(filas):
    """Multiplicidad de cada fila, marca de copia extra y multiplicidades de las filas distintas."""
    cnt = Counter(filas)
    vistas, extra = set(), []
    for t in filas:
        extra.append(t in vistas)
        vistas.add(t)
    return [cnt[t] for t in filas], extra, list(cnt.values())


def fila_anio(etiqueta, k, extra, tab, v):
    n, d = len(k), len(tab)
    rep = [x > 1 for x in k]
    n_rep, n_grupos = sum(rep), sum(1 for x in tab if x > 1)
    assert n_rep == (n - d) + n_grupos, f"{etiqueta}: filas repetidas no cuadran con copias y grupos"
    s = math.fsum(v)
    s_extra = math.fsum(x for x, e in zip(v, extra) if e)
    s_unicas = math.fsum(x for x, e in zip(v, extra) if not e)
    s_rep = math.fsum(x for x, r in zip(v, rep) if r)
    s_sin_rep = math.fsum(x for x, r in zip(v, rep) if not r)
    for a, b in ((s_extra, s_unicas), (s_rep, s_sin_rep)):
        assert abs(s - a - b) <= TOL_REL * max(1.0, abs(s)), f"{etiqueta}: las sumas parciales no dan el total"
    mult_rep = [x for x in k if x > 1]
    return {
        "anio": etiqueta, "n_filas_datos": n, "n_filas_distintas": d, "n_duplicados_exactos": n - d,
        "n_grupos_repetidos": n_grupos, "n_filas_repetidas": n_rep, "n_filas_sin_repetir": n - n_rep,
        "multiplicidad_max": max(tab) if tab else 0,
        "multiplicidad_mediana_filas_repetidas": g17(statistics.median(mult_rep)) if mult_rep else "",
        "suma_valor": g17(s), "suma_valor_sin_duplicados": g17(s_unicas), "suma_valor_copias_extra": g17(s_extra),
        "suma_valor_filas_repetidas": g17(s_rep), "suma_valor_filas_sin_repetir": g17(s_sin_rep),
        "prop_filas_copias_extra": prop(n - d, n), "prop_valor_copias_extra": prop(s_extra, s),
    }


def filas_mult(anio, k, tab, v):
    valores = defaultdict(list)
    for x, val in zip(k, v):
        valores[x].append(val)
    ng = Counter(tab)
    out = [{"clave": f"{anio}|{kk}", "anio": anio, "multiplicidad": kk, "n_filas_distintas": ng[kk],
            "n_filas": kk * ng[kk], "suma_valor": g17(math.fsum(valores[kk]))} for kk in sorted(ng)]
    assert sum(r["n_filas"] for r in out) == len(k) and sum(r["n_filas_distintas"] for r in out) == len(tab)
    return out


def resumen(conteos, total):
    """Proporción en las categorías con más conteo (TOPS) y categorías para la mitad y el 90 %."""
    if total == 0:
        return [""] * len(TOPS), "", ""
    s = sorted(conteos, reverse=True)
    acum, mitad, noventa = 0, "", ""
    for i, x in enumerate(s, start=1):
        acum += x
        if mitad == "" and 2 * acum >= total:
            mitad = i
        if noventa == "" and 10 * acum >= 9 * total:
            noventa = i
    return [prop(sum(s[:t]), total) for t in TOPS], mitad, noventa


def top1(por_copias, dim):
    """Fecha (ISO) o Valor con más copias extra; entre empates, el de menor valor numérico."""
    if dim not in CON_TOP1 or not por_copias:
        return ""
    m = max(por_copias.values())
    c = min((x for x, n in por_copias.items() if n == m), key=float)
    return serial_a_fecha(c).isoformat() if dim == "fecha" else c


def filas_conc(anio, filas, idx, extra):
    ir, ifc, iv = idx["rut"], idx["fecha"], idx["valor"]
    categoria = {"donataria": lambda t: t[ir], "fecha": lambda t: t[ifc],
                 "donataria_fecha": lambda t: (t[ir], t[ifc]), "valor": lambda t: t[iv]}
    out = []
    for dim in DIMENSIONES:
        c = categoria[dim]
        por_filas = Counter(c(t) for t in filas)
        por_copias = Counter(c(t) for t, e in zip(filas, extra) if e)
        n_copias = sum(por_copias.values())
        top_c, mitad_c, noventa_c = resumen(list(por_copias.values()), n_copias)
        top_f, mitad_f, noventa_f = resumen(list(por_filas.values()), len(filas))
        fila = {"clave": f"{anio}|{dim}", "anio": anio, "dimension": dim, "n_categorias": len(por_filas),
                "n_categorias_con_copias": len(por_copias), "n_copias_extra": n_copias,
                "n_categorias_mitad_copias": mitad_c, "n_categorias_90_copias": noventa_c,
                "categoria_top1_copias": top1(por_copias, dim),
                "n_categorias_mitad_filas": mitad_f, "n_categorias_90_filas": noventa_f}
        for t, pc, pf in zip(TOPS, top_c, top_f):
            fila[f"prop_copias_top{t}"], fila[f"prop_filas_top{t}"] = pc, pf
        out.append(fila)
    return out


def contra_perfil(por_anio, ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        perfil = {r["anio"]: r for r in csv.DictReader(f)}
    anios = {str(r["anio"]) for r in por_anio[:-1]}
    assert anios == set(perfil), f"años distintos: duplicados={sorted(anios)} perfil={sorted(perfil)}"
    for r in por_anio[:-1]:
        p = perfil[str(r["anio"])]
        for c in ("n_filas_datos", "n_duplicados_exactos"):
            assert int(p[c]) == r[c], f"{r['anio']} {c}: perfil {p[c]} vs {r[c]}"
        assert float(p["suma_valor"]) == float(r["suma_valor"]), f"{r['anio']} suma_valor: perfil {p['suma_valor']} vs {r['suma_valor']}"
        assert p["total_cabecera"] != "" and float(p["total_cabecera"]) == float(r["suma_valor"]), (
            f"{r['anio']}: el total de la fila superior ({p['total_cabecera']}) no es la suma con duplicados")


def escribir(ruta, campos, filas):
    with open(ruta, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=campos)
        w.writeheader()
        w.writerows(filas)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("entrada")
    p.add_argument("por_anio")
    p.add_argument("multiplicidad")
    p.add_argument("concentracion")
    p.add_argument("--csv", action="store_true", help="la entrada es un CSV de prueba con columna anio")
    p.add_argument("--perfil", help="perfil versionado contra el que se comprueban filas, copias y suma")
    a = p.parse_args()

    datos = filas_csv(a.entrada) if a.csv else filas_xlsx(a.entrada)
    por_anio, mult, conc = [], [], []
    todos = {"k": [], "extra": [], "tab": [], "v": []}
    for anio, (filas, idx) in datos.items():
        k, extra, tab = grupos(filas)
        v = [num(t[idx["valor"]]) for t in filas]
        por_anio.append(fila_anio(anio, k, extra, tab, v))
        mult += filas_mult(anio, k, tab, v)
        conc += filas_conc(anio, filas, idx, extra)
        for nombre, x in (("k", k), ("extra", extra), ("tab", tab), ("v", v)):
            todos[nombre] += x
        print("duplicados", anio, flush=True)
    anios = list(datos)
    por_anio.append(fila_anio(f"{anios[0]}-{anios[-1]}", todos["k"], todos["extra"], todos["tab"], todos["v"]))
    if a.perfil:
        contra_perfil(por_anio, a.perfil)
        print("perfil OK: filas, copias extra y suma de Valor iguales al perfil; el total del archivo es la suma con duplicados")
    escribir(a.por_anio, CAMPOS_ANIO, por_anio)
    escribir(a.multiplicidad, CAMPOS_MULT, mult)
    escribir(a.concentracion, CAMPOS_CONC, conc)


if __name__ == "__main__":
    main()
