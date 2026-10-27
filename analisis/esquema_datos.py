"""Perfil del esquema de las planillas anuales «Donaciones recibidas Ley N°19.885».

Una fila por año con: hoja de datos, fila de encabezado, columnas normalizadas, conteos de tipos de
valor (fecha, Fondo Mixto), rangos, sumas y duplicados. Es la contraparte en Python de
esquema_datos.R: las dos implementaciones leen el Excel con lectores distintos (calamine y readxl)
y aplican las mismas reglas, y contraste_datos.py exige que coincidan.

Reglas comunes (idénticas en R):
- Se recorta la hoja a la caja de celdas no vacías; las filas se numeran desde la primera no vacía.
- Hoja de datos: la primera hoja que, en sus 10 primeras filas, tiene una celda en la primera
  columna cuyo texto normalizado es «ANO». Encabezado: la primera fila así.
- Texto normalizado: se sustituyen las vocales acentuadas y la Ñ, se pasa a mayúsculas y se
  quita todo lo que no sea A-Z o 0-9.
- Cada celda se trata como el texto que da readxl con col_types = "text": las fechas son su
  número de serie de Excel, los enteros no llevan «.0».
- Se sustituyen por U+FFFD los caracteres fuera del plano básico (U+10000 en adelante) y los
  escapes OOXML `_xHHHH_` que codifican una mitad de par sustituto (HHHH entre D800 y DFFF): en los
  archivos de 2021 a 2025 aparecen en nombres de personas (acentos corruptos) y hacen fallar a
  readxl. Los demás escapes `_xHHHH_` se decodifican, como hacen calamine y readxl (en los archivos de
  2008 a 2025 solo aparece `_x000D_`, una vez en 2024). `n_celdas_con_u_fffd` cuenta las
  celdas de datos que los contienen.
- Las celdas de texto con solo espacios se tratan como vacías (readxl las descarta, calamine no).
- Los fines de línea se normalizan a «\n»: «\r\n» y «\r» sueltos pasan a «\n». Los literales del XML
  los normaliza calamine, como manda la sección 2.11 de XML 1.0 (los archivos traen CRLF dentro del
  texto, por ejemplo en 2021); readxl no, y esquema_datos.R los normaliza antes de leer. Los «\r» que
  salen de decodificar `_x000D_` se normalizan en cada celda, en los dos lenguajes.
- Fila de datos: AÑO numérico entero y MONTO TOTAL DONACIÓN numérico.

Uso: python esquema_datos.py <carpeta_xlsx> <salida.csv> [años separados por coma]
"""
import csv
import datetime as dt
import math
import re
import sys
from collections import Counter
from pathlib import Path

try:
    from python_calamine import CalamineWorkbook
except ImportError:  # solo lo usa tabla(); `make verificar` importa este módulo sin leer ningún .xlsx
    CalamineWorkbook = None

ORIGEN = dt.datetime(1899, 12, 30)
FUERA_PLANO = re.compile("[\U00010000-\U0010FFFF]")
SUSTITUTO = re.compile(r"_x(D[89A-Fa-f][0-9A-Fa-f]{2})_")
ACENTOS = str.maketrans("ÁÉÍÓÚÜÑáéíóúüñ", "AEIOUUNAEIOUUN")
NUM = re.compile(r"^-?\d+(\.\d+)?([eE][-+]?\d+)?$")
SERIAL = re.compile(r"^\d{5}(\.\d+)?$")
ISO = re.compile(r"^\d{4}-\d{2}-\d{2}")
ROLES = {
    "anio": r"^ANO$", "fecha": r"^FECHA", "proyecto": r"^NOMBREDELPROYECTO",
    "folio_proyecto": r"^FOLIOPROYECTO", "donatario": r"^DONATARIO$",
    "folio_institucion": r"^FOLIOINSTITUCION", "rut_institucion": r"^RUTINSTITUCION",
    "donante": r"^DONANTE$", "rut_donante": r"^RUT$", "fm": r"^MONTOFONDOMIXTO",
    "minst": r"^MONTOINSTITUCION", "mtotal": r"^MONTOTOTALDONACION", "region": r"^REGION",
}
CAMPOS = [
    "anio", "hoja", "fila_encabezado", "columnas_normalizadas", "n_columnas", "n_filas_hoja",
    "n_filas_datos", "n_anio_num_monto_no_num", "n_filas_sin_anio", "anios_valores",
    "tiene_rut_donante", "tiene_rut_institucion", "tiene_folio_proyecto",
    "tiene_folio_institucion", "tiene_region", "n_fm_numerico", "n_fm_si_no", "n_fm_otro",
    "n_fm_vacio", "n_fecha_serial", "n_fecha_iso_texto", "n_fecha_otro_texto", "n_fecha_vacia",
    "fecha_min", "fecha_max", "n_anio_distinto_de_fecha", "suma_monto_total",
    "suma_monto_institucion", "suma_monto_fm", "n_duplicados_exactos", "n_donantes_distintos",
    "n_donantes_con_rut", "n_donatarios_distintos", "n_celdas_con_u_fffd",
]


def norm(s):
    return re.sub(r"[^A-Z0-9]", "", s.translate(ACENTOS).upper())


def canon(c):
    if c is None or c == "":
        return ""
    if isinstance(c, dt.datetime):
        d = (c - ORIGEN).total_seconds() / 86400
        return str(int(d)) if d == int(d) else repr(d)
    if isinstance(c, dt.date):
        return str((dt.datetime(c.year, c.month, c.day) - ORIGEN).days)
    if isinstance(c, bool):
        return "TRUE" if c else "FALSE"
    if isinstance(c, float):
        return str(int(c)) if c == int(c) else repr(c)
    c = str(c)
    if not c.strip():
        return ""
    return SUSTITUTO.sub("\ufffd", FUERA_PLANO.sub("\ufffd", c)).replace("\r\n", "\n").replace("\r", "\n")


def recortar(filas):
    """Quita filas y columnas vacías de los bordes, como hace readxl."""
    filas = [[canon(c) for c in f] for f in filas]
    con_datos = [i for i, f in enumerate(filas) if any(f)]
    if not con_datos:
        return []
    filas = filas[con_datos[0]: con_datos[-1] + 1]
    ancho = max(len(f) for f in filas)
    filas = [f + [""] * (ancho - len(f)) for f in filas]
    cols = [j for j in range(ancho) if any(f[j] for f in filas)]
    return [f[cols[0]: cols[-1] + 1] for f in filas]


def num(s):
    return float(s) if NUM.match(s) else None


def serial_a_fecha(s):
    return (ORIGEN + dt.timedelta(days=int(float(s)))).date()


def tabla(ruta, encabezado="ANO"):
    """Hoja, fila de encabezado (desde 0), filas recortadas, encabezado normalizado. También la usan
    esquema_ley21440.py y cruce_donatarias.py."""
    assert CalamineWorkbook is not None, "leer un .xlsx necesita python-calamine (analisis/requirements.txt)"
    wb = CalamineWorkbook.from_path(str(ruta))
    for nombre in wb.sheet_names:
        filas = recortar(wb.get_sheet_by_name(nombre).to_python(skip_empty_area=False))
        for i, f in enumerate(filas[:10]):
            if norm(f[0]) == encabezado:
                return nombre, i, filas, [norm(c) for c in filas[i]]
    raise AssertionError(f"{ruta}: ninguna hoja tiene un encabezado «{encabezado}» en sus 10 primeras filas")


def roles(enc, patrones=ROLES):
    return {r: next((j for j, h in enumerate(enc) if re.match(p, h)), None) for r, p in patrones.items()}


def clase_fila(f, idx):
    """«sin_anio», «monto_no_num» o «datos» (fila de datos: AÑO entero y MONTO TOTAL numérico)."""
    a = num(f[idx["anio"]]) if idx["anio"] is not None else None
    if a is None or a != int(a):
        return "sin_anio"
    return "monto_no_num" if num(f[idx["mtotal"]] if idx["mtotal"] is not None else "") is None else "datos"


def perfil(anio, ruta):
    hoja, fila_enc, datos, enc = tabla(ruta)
    idx = roles(enc)
    cuerpo = datos[fila_enc + 1:]

    def col(f, r):
        return f[idx[r]] if idx[r] is not None else ""

    no_vacias = [f for f in cuerpo if any(f)]
    clases = [clase_fila(f, idx) for f in no_vacias]
    filas_datos = [f for f, c in zip(no_vacias, clases) if c == "datos"]
    anio_num_monto_no, sin_anio = clases.count("monto_no_num"), clases.count("sin_anio")

    anios = Counter(int(float(col(f, "anio"))) for f in filas_datos)
    fm = Counter()
    for f in filas_datos:
        v = col(f, "fm")
        fm["vacio" if v == "" else "num" if NUM.match(v) else "si_no" if norm(v) in ("SI", "NO") else "otro"] += 1
    fch, fechas, dist = Counter(), [], 0
    for f in filas_datos:
        v = col(f, "fecha")
        if v == "":
            fch["vacia"] += 1
        elif SERIAL.match(v):
            fch["serial"] += 1
            d = serial_a_fecha(v)
            fechas.append(d)
            dist += d.year != int(float(col(f, "anio")))
        elif ISO.match(v):
            fch["iso"] += 1
        else:
            fch["otro"] += 1

    def suma(r):
        return math.fsum(x for x in (num(col(f, r)) for f in filas_datos) if x is not None)

    return {
        "anio": anio, "hoja": hoja, "fila_encabezado": fila_enc + 1,
        "columnas_normalizadas": "|".join(h for h in enc if h), "n_columnas": sum(1 for h in enc if h),
        "n_filas_hoja": len(no_vacias), "n_filas_datos": len(filas_datos),
        "n_anio_num_monto_no_num": anio_num_monto_no, "n_filas_sin_anio": sin_anio,
        "anios_valores": "|".join(f"{a}:{n}" for a, n in sorted(anios.items())),
        "tiene_rut_donante": str(idx["rut_donante"] is not None).lower(),
        "tiene_rut_institucion": str(idx["rut_institucion"] is not None).lower(),
        "tiene_folio_proyecto": str(idx["folio_proyecto"] is not None).lower(),
        "tiene_folio_institucion": str(idx["folio_institucion"] is not None).lower(),
        "tiene_region": str(idx["region"] is not None).lower(),
        "n_fm_numerico": fm["num"], "n_fm_si_no": fm["si_no"], "n_fm_otro": fm["otro"], "n_fm_vacio": fm["vacio"],
        "n_fecha_serial": fch["serial"], "n_fecha_iso_texto": fch["iso"], "n_fecha_otro_texto": fch["otro"],
        "n_fecha_vacia": fch["vacia"],
        "fecha_min": min(fechas).isoformat() if fechas else "", "fecha_max": max(fechas).isoformat() if fechas else "",
        "n_anio_distinto_de_fecha": dist,
        "suma_monto_total": repr(suma("mtotal")), "suma_monto_institucion": repr(suma("minst")),
        "suma_monto_fm": repr(suma("fm")),
        "n_duplicados_exactos": len(filas_datos) - len({tuple(f) for f in filas_datos}),
        "n_donantes_distintos": len({col(f, "donante") for f in filas_datos if col(f, "donante") != ""}),
        "n_donantes_con_rut": sum(1 for f in filas_datos if col(f, "rut_donante") != ""),
        "n_donatarios_distintos": len({col(f, "donatario") for f in filas_datos if col(f, "donatario") != ""}),
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
