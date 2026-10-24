"""Perfil del esquema de la nómina de donaciones de la Ley 21.440 (donacionesley21440.gob.cl).

Una fila por año con: hoja, fila de encabezado, columnas normalizadas, la fila superior (texto y
total que trae el archivo sobre la columna Valor), conteos y rangos de valor y fecha, duplicados
exactos, RUT de la donataria (formato y dígito verificador) y valores de «Nac/Ext.». Es la
contraparte en Python de esquema_ley21440.R; contraste_datos.py exige que coincidan.

Reglas de lectura: las de esquema_datos.py (celda como el texto de readxl con col_types = "text",
recorte a la caja de celdas no vacías, U+FFFD, espacios), que se importan de ahí. Además:
- Hoja de datos: la primera que, en sus 10 primeras filas, tiene una celda en la primera columna
  cuyo texto normalizado es «RUTDONATARIA». Encabezado: la primera fila así.
- Fila de datos: RUT de la donataria no vacío y Valor numérico.
- Fila superior: las celdas sobre el encabezado. `texto_cabecera` junta las no numéricas con « | »;
  `total_cabecera` es la celda numérica de la columna Valor (a lo más una).
- RUT bien formado: dd.ddd.ddd-D con D dígito o K; los espacios cuentan. El dígito verificador se
  comprueba con Módulo 11 solo en los bien formados.
- Fecha de beneficios: 2022-07-01, artículo primero transitorio de la Ley 21.440
  (normativa/cambios.csv).
- Números que no son sumas: con 17 cifras significativas («%.17g»), igual que en R.

Uso: python esquema_ley21440.py <carpeta_xlsx> <salida.csv> [años separados por coma]
"""
import csv
import datetime as dt
import math
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from esquema_datos import NUM, SERIAL, num, roles, serial_a_fecha, tabla  # noqa: E402

INICIO_BENEFICIOS = dt.date(2022, 7, 1)
RUT = re.compile(r"^\d{1,2}\.\d{3}\.\d{3}-[0-9K]$")
ROLES = {"rut": r"^RUTDONATARIA$", "donataria": r"^DONATARIA$", "nac_ext": r"^NACEXT$",
         "valor": r"^VALOR$", "fecha": r"^FECHA$"}
CAMPOS = [
    "anio", "hoja", "fila_encabezado", "columnas_normalizadas", "n_columnas", "texto_cabecera",
    "total_cabecera", "n_filas_hoja", "n_filas_datos", "n_filas_no_datos", "suma_valor", "valor_min",
    "valor_max", "n_valor_cero", "n_valor_negativo", "n_valor_no_entero", "n_fecha_serial",
    "n_fecha_texto", "n_fecha_vacia", "fecha_min", "fecha_max", "n_fecha_antes_inicio_beneficios",
    "n_anio_fecha_distinto_archivo", "n_duplicados_exactos", "n_rut_distintos", "n_filas_rut_mal_formado",
    "n_rut_dv_invalido", "n_donatarias_distintas", "n_rut_con_varios_nombres", "n_nombres_con_varios_rut",
    "tiene_nac_ext", "nac_ext_valores", "n_celdas_con_u_fffd",
]


def g17(x):
    return f"{x:.17g}"


def dv_modulo11(cuerpo):
    s, factor = 0, 2
    for d in reversed(cuerpo):
        s += int(d) * factor
        factor = 2 if factor == 7 else factor + 1
    r = 11 - s % 11
    return "0" if r == 11 else "K" if r == 10 else str(r)


def dv_valido(rut):
    cuerpo, dv = rut.replace(".", "").split("-")
    return dv_modulo11(cuerpo) == dv


def leer(ruta):
    """Hoja, fila de encabezado, filas recortadas, encabezado e índices; también la usa cruce_donatarias.py."""
    hoja, fila_enc, datos, enc = tabla(ruta, "RUTDONATARIA")
    idx = roles(enc, ROLES)
    faltan = [r for r in ("rut", "donataria", "valor", "fecha") if idx[r] is None]
    assert not faltan, f"{ruta}: faltan las columnas {faltan}"
    return hoja, fila_enc, datos, enc, idx


def es_fila_datos(f, idx):
    return f[idx["rut"]] != "" and num(f[idx["valor"]]) is not None


def perfil(anio, ruta):
    hoja, fila_enc, datos, enc, idx = leer(ruta)

    arriba = datos[:fila_enc]
    texto = " | ".join(c for f in arriba for c in f if c != "" and not NUM.match(c))
    totales = [f[idx["valor"]] for f in arriba if NUM.match(f[idx["valor"]])]
    assert len(totales) <= 1, f"{ruta}: más de un total sobre la columna Valor"

    def col(f, r):
        return f[idx[r]] if idx[r] is not None else ""

    no_vacias = [f for f in datos[fila_enc + 1:] if any(f)]
    filas_datos = [f for f in no_vacias if es_fila_datos(f, idx)]
    valores = [num(col(f, "valor")) for f in filas_datos]

    fch, fechas = Counter(), []
    for f in filas_datos:
        v = col(f, "fecha")
        if v == "":
            fch["vacia"] += 1
        elif SERIAL.match(v):
            fch["serial"] += 1
            fechas.append(serial_a_fecha(v))
        else:
            fch["texto"] += 1

    ruts = [col(f, "rut") for f in filas_datos]
    distintos = set(ruts)
    nombres_por_rut, ruts_por_nombre = defaultdict(set), defaultdict(set)
    for f in filas_datos:
        nombres_por_rut[col(f, "rut")].add(col(f, "donataria"))
        ruts_por_nombre[col(f, "donataria")].add(col(f, "rut"))
    nac = Counter(col(f, "nac_ext") or "<vacio>" for f in filas_datos) if idx["nac_ext"] is not None else Counter()

    return {
        "anio": anio, "hoja": hoja, "fila_encabezado": fila_enc + 1,
        "columnas_normalizadas": "|".join(h for h in enc if h), "n_columnas": sum(1 for h in enc if h),
        "texto_cabecera": texto, "total_cabecera": g17(float(totales[0])) if totales else "",
        "n_filas_hoja": len(no_vacias), "n_filas_datos": len(filas_datos),
        "n_filas_no_datos": len(no_vacias) - len(filas_datos),
        "suma_valor": g17(math.fsum(valores)),
        "valor_min": g17(min(valores)) if valores else "", "valor_max": g17(max(valores)) if valores else "",
        "n_valor_cero": sum(1 for v in valores if v == 0), "n_valor_negativo": sum(1 for v in valores if v < 0),
        "n_valor_no_entero": sum(1 for v in valores if v != math.floor(v)),
        "n_fecha_serial": fch["serial"], "n_fecha_texto": fch["texto"], "n_fecha_vacia": fch["vacia"],
        "fecha_min": min(fechas).isoformat() if fechas else "", "fecha_max": max(fechas).isoformat() if fechas else "",
        "n_fecha_antes_inicio_beneficios": sum(1 for d in fechas if d < INICIO_BENEFICIOS),
        "n_anio_fecha_distinto_archivo": sum(1 for d in fechas if d.year != anio),
        "n_duplicados_exactos": len(filas_datos) - len({tuple(f) for f in filas_datos}),
        "n_rut_distintos": len(distintos),
        "n_filas_rut_mal_formado": sum(1 for r in ruts if not RUT.match(r)),
        "n_rut_dv_invalido": sum(1 for r in distintos if RUT.match(r) and not dv_valido(r)),
        "n_donatarias_distintas": len({col(f, "donataria") for f in filas_datos if col(f, "donataria") != ""}),
        "n_rut_con_varios_nombres": sum(1 for s in nombres_por_rut.values() if len(s) > 1),
        "n_nombres_con_varios_rut": sum(1 for s in ruts_por_nombre.values() if len(s) > 1),
        "tiene_nac_ext": str(idx["nac_ext"] is not None).lower(),
        "nac_ext_valores": "|".join(f"{k}:{n}" for k, n in sorted(nac.items())),
        "n_celdas_con_u_fffd": sum(1 for f in filas_datos for c in f if "\ufffd" in c),
    }


def main(carpeta, salida, anios=None):
    carpeta = Path(carpeta)
    anios = [int(a) for a in anios.split(",")] if anios else sorted(int(p.stem) for p in carpeta.glob("[0-9][0-9][0-9][0-9].xlsx"))
    with open(salida, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=CAMPOS)
        w.writeheader()
        for a in anios:
            w.writerow(perfil(a, carpeta / f"{a}.xlsx"))
            print("perfil", a, flush=True)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else None)
