"""Armoniza las planillas «Donaciones recibidas Ley N°19.885» según D-10 (reglas 1 a 6 y 8).

Lee cada planilla anual con las reglas de esquema_datos.py y escribe, por año:
- <salida_filas>/<año>.csv: una fila por fila no vacía bajo el encabezado, con las columnas armonizadas
  (CAMPOS_FILA). Trae nombres de personas: va en data_local/, nunca al repositorio.
- una fila en <conteos.csv> (CAMPOS_CONTEO) con cuántas filas toca cada regla;
- sus filas en <anios_donacion.csv> (filas y monto por año del archivo y año de la donación);
- sus columnas en <diccionario.csv> (encabezado original, normalizado, rol, variante de esquema).
La contraparte en R es armonizar.R; contraste_datos.py compara las tablas y contraste_armonizado.py
las filas. La regla 7 (completar RUT) no está aquí: solo se normaliza el RUT observado.

Reglas (D-10; lo que D-10 no dice y se fija aquí es inferencia del investigador y va marcado «[i]»):
- Universo: toda fila no vacía bajo el encabezado. Cada una queda incluida o excluida con su motivo.
- Regla 1, montos. Total numérico: se usa. Si no, y Fondo Mixto e Institución son numéricos y el año
  es apto, total = FM + Institución («recuperado»). Si no, la fila se excluye
  («monto_no_recuperable»). Año apto: entre las filas con total, FM e Institución numéricos, al
  menos UMBRAL_R1 cumplen |total − (FM + Institución)| <= TOL_SUMA [i: tolerancia]; un año sin esas
  filas no es apto [i].
- Regla 2, Fondo Mixto: «monto» si es numérico, «si_no» si su texto normalizado es SI o NO, «vacio» u
  «otro». El 33 % solo es medible con «monto» [i: también «vacio» y «otro» quedan no medibles].
- `fecha_tipo`: el formato de la fecha cuando es válida (serial, texto_iso, texto_dma, texto_largo);
  si no, vacia, no_interpretable, invalida (no existe en el calendario) o fuera_de_rango.
- Regla 3, fechas. Se quitan espacios, tabuladores y saltos de línea de los bordes y se pasan a
  mayúscula las letras a-z. Formatos interpretables: número de serie de Excel (5 dígitos, se toma la
  parte entera), AAAA-MM-DD con o sin « hh:mm:ss», D-M-AAAA con separadores - / . , o espacio
  (independientes entre sí) y «D de MES de AAAA», «D de MES AAAA», «D.MES.AAAA» o «D. MES.AAAA»
  con el nombre completo del mes. Lo demás (solo el mes, mes y año, día y mes, solo el año, año de
  2 o 5 dígitos, «N/S») no se interpreta [i]. Una fecha interpretada es válida si existe en el
  calendario y cae entre DESDE y HASTA (los años publicados, 2008 a 2025 [i]).
- Año y mes de la donación (D-14, que reemplaza la regla 4 de D-10 y la parte de la regla 3 que daba
  prioridad a la fecha; D-13, punto 1): el año es siempre el del archivo de origen, también en las
  filas sin `AÑO` ni fecha válida, que ya no se excluyen. La fecha válida da el mes solo si cae en el
  año del archivo; si cae en otro año, el mes queda desconocido, la fila se marca
  (`fecha_otro_anio`) y el año de la fecha queda en `anio_fecha` para sensibilidad. Sin fecha válida,
  el mes queda desconocido. `AÑO` se considera válido si es entero y está entre 2008 y 2025; solo se
  usa para contar si coincide con el archivo.
- Regla 5, duplicados exactos: filas con todas sus celdas iguales a una anterior del mismo archivo;
  se marcan desde la segunda copia y no se excluyen.
- Regla 6: se marca la fila cuyo donante tiene U+FFFD; no se excluye.
- RUT del donante (insumo de la regla 7): se pasa a mayúscula, la raya – o — se lee como guion y se
  quitan puntos, comas, espacios, tabuladores, saltos de línea y espacios duros (U+00A0). Con guion: cuerpo-DV. Sin guion, según el formato del año: si al
  menos la mitad de los valores sin guion de solo dígitos (o terminados en K) pasan Módulo 11 leídos
  como cuerpo + DV, todos se leen así («cuerpo_dv»); si no, son el cuerpo sin DV («cuerpo»; en ese
  caso un valor terminado en K o de 9 dígitos se lee como cuerpo + DV, porque el cuerpo tiene a lo más
  8) [i]. Estados: dv_valido, dv_invalido,
  sin_dv, mal_formado, vacio y sin_columna. El cuerpo se guarda sin ceros a la izquierda.
- Regla 8: no se imputan años ni filas.

Uso: python armonizar.py <carpeta_xlsx> <salida_filas> <conteos.csv> <anios_donacion.csv> <diccionario.csv>
"""
import csv
import datetime as dt
import math
import re
import sys
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import esquema_datos as e  # noqa: E402

DESDE, HASTA = dt.date(2008, 1, 1), dt.date(2025, 12, 31)
UMBRAL_R1 = 0.999
TOL_SUMA = 1e-6
MESES = {"ENERO": 1, "FEBRERO": 2, "MARZO": 3, "ABRIL": 4, "MAYO": 5, "JUNIO": 6, "JULIO": 7, "AGOSTO": 8,
         "SEPTIEMBRE": 9, "SETIEMBRE": 9, "OCTUBRE": 10, "NOVIEMBRE": 11, "DICIEMBRE": 12}
BORDES = re.compile(r"^[ \t\r\n]+|[ \t\r\n]+$")
MAYUS = str.maketrans("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
ISO = re.compile(r"^([0-9]{4})-([0-9]{2})-([0-9]{2})( [0-9]{2}:[0-9]{2}:[0-9]{2})?$")
DMA = re.compile(r"^([0-9]{1,2})[-/., ]([0-9]{1,2})[-/., ]([0-9]{4})$")
LARGO = re.compile(r"^([0-9]{1,2})(?: DE |\. ?)([A-Z]+)(?: DE |\.| )([0-9]{4})$")
RUT_GUION = re.compile(r"^([0-9]{1,8})-([0-9K])$")
RUT_CUERPO_DV = re.compile(r"^([0-9]{1,8})([0-9K])$")
RUT_CUERPO = re.compile(r"^([0-9]{1,8})$")
RUT_SIN_GUION = re.compile(r"^[0-9]{2,9}$|^[0-9]{1,8}K$")
SEPARADORES_RUT = re.compile("[., \t\r\n\f\v\u00a0]")
TIPOS = {"anio": "entero", "fecha": "fecha", "proyecto": "texto", "folio_proyecto": "texto", "donatario": "texto",
         "folio_institucion": "texto", "rut_institucion": "rut", "donante": "texto", "rut_donante": "rut",
         "fm": "monto o indicador", "minst": "monto", "mtotal": "monto", "region": "texto"}
VARIANTES = {}  # columnas normalizadas -> número de variante, en orden de aparición

CAMPOS_FILA = [
    "anio_archivo", "fila", "incluida", "motivo_exclusion", "anio_columna", "anio_donacion", "mes_donacion",
    "fecha_donacion", "fecha_tipo", "anio_fecha", "fecha_otro_anio", "monto_total", "monto_total_origen", "monto_institucion",
    "monto_fm", "fm_tipo", "fm_si_no", "fm_33_medible", "duplicado_exacto", "donatario", "rut_institucion",
    "proyecto", "folio_proyecto", "folio_institucion", "region", "donante", "donante_con_fffd",
    "rut_donante", "rut_cuerpo", "rut_estado",
]
CAMPOS_CONTEO = [
    "anio", "variante", "n_filas_hoja", "n_incluidas", "n_excluidas_monto", "n_excluidas_sin_anio",
    "n_total_numerico", "n_total_recuperado", "n_validas_r1", "n_validas_r1_suma_igual", "anio_apto_r1",
    "n_total_cero", "n_total_negativo", "n_fm_monto", "n_fm_si_no", "n_fm_vacio", "n_fm_otro",
    "n_fecha_vacia", "n_fecha_serial", "n_fecha_texto_iso", "n_fecha_texto_dma", "n_fecha_texto_largo",
    "n_fecha_no_interpretable", "n_fecha_invalida", "n_fecha_fuera_de_rango", "n_fecha_valida",
    "n_mes_desconocido", "n_fecha_otro_anio", "n_anio_columna_no_valido", "n_anio_columna_distinto_archivo",
    "n_duplicados_exactos", "n_donante_con_fffd", "formato_rut_sin_guion",
    "n_rut_sin_guion", "n_rut_sin_guion_dv_ok", "n_rut_vacio", "n_rut_dv_valido", "n_rut_dv_invalido",
    "n_rut_sin_dv", "n_rut_mal_formado", "n_rut_sin_columna", "suma_monto_total",
    "suma_monto_total_sin_duplicados",
]
CAMPOS_ANIOS = ["par_anios", "anio_archivo", "anio_donacion", "n_filas", "suma_monto_total"]
CAMPOS_DICC = ["anio_posicion", "anio", "variante", "posicion", "encabezado", "normalizado", "rol", "tipo"]


def g17(x):
    return f"{x:.17g}"


def dv_modulo11(cuerpo):
    s, factor = 0, 2
    for d in reversed(cuerpo):
        s += int(d) * factor
        factor = 2 if factor == 7 else factor + 1
    r = 11 - s % 11
    return "0" if r == 11 else "K" if r == 10 else str(r)


def leer_fecha(v):
    """(tipo, fecha o None). Tipos: vacia, serial, texto_iso, texto_dma, texto_largo, no_interpretable."""
    if v == "":
        return "vacia", None
    if e.SERIAL.match(v):
        return "serial", e.serial_a_fecha(v)
    t = BORDES.sub("", v).translate(MAYUS)
    for tipo, rx in (("texto_iso", ISO), ("texto_dma", DMA), ("texto_largo", LARGO)):
        m = rx.match(t)
        if m:
            if tipo == "texto_iso":
                a, mes, d = int(m.group(1)), int(m.group(2)), int(m.group(3))
            elif tipo == "texto_dma":
                d, mes, a = int(m.group(1)), int(m.group(2)), int(m.group(3))
            else:
                if m.group(2) not in MESES:
                    return "no_interpretable", None
                d, mes, a = int(m.group(1)), MESES[m.group(2)], int(m.group(3))
            try:
                return tipo, dt.date(a, mes, d)
            except ValueError:
                return tipo, "invalida"
    return "no_interpretable", None


def limpiar_rut(v):
    return SEPARADORES_RUT.sub("", v.translate(MAYUS).replace("\u2013", "-").replace("\u2014", "-"))


def formato_rut(valores):
    sin = [s for s in (limpiar_rut(v) for v in valores) if "-" not in s and RUT_SIN_GUION.match(s)]
    ok = sum(1 for s in sin if dv_modulo11(s[:-1]) == s[-1])
    return ("cuerpo_dv" if sin and 2 * ok >= len(sin) else "cuerpo" if sin else "cuerpo_dv"), len(sin), ok


def leer_rut(v, formato):
    """(estado, cuerpo sin ceros a la izquierda)."""
    if v == "":
        return "vacio", ""
    s = limpiar_rut(v)
    con_dv = formato == "cuerpo_dv" or s.endswith("K") or len(s) == 9
    m = RUT_GUION.match(s) or (RUT_CUERPO_DV.match(s) if con_dv else None)
    if m:
        cuerpo = m.group(1).lstrip("0")
        if cuerpo == "":
            return "mal_formado", ""
        return ("dv_valido" if dv_modulo11(m.group(1)) == m.group(2) else "dv_invalido"), cuerpo
    m = RUT_CUERPO.match(s) if formato == "cuerpo" else None
    if m and m.group(1).lstrip("0"):
        return "sin_dv", m.group(1).lstrip("0")
    return "mal_formado", ""


def armonizar(anio, ruta):
    hoja, fila_enc, datos, enc = e.tabla(ruta)
    idx = e.roles(enc)
    clave = "|".join(h for h in enc if h)
    variante = VARIANTES.setdefault(clave, len(VARIANTES) + 1)
    dicc = [{"anio_posicion": f"{anio}-{j + 1:02d}", "anio": anio, "variante": variante, "posicion": j + 1,
             "encabezado": datos[fila_enc][j], "normalizado": h,
             "rol": next((r for r, k in idx.items() if k == j), ""),
             "tipo": TIPOS.get(next((r for r, k in idx.items() if k == j), ""), "")}
            for j, h in enumerate(enc) if h]

    def col(f, r):
        return f[idx[r]] if idx[r] is not None else ""

    filas = [(i + 1, f) for i, f in enumerate(datos) if i > fila_enc and any(f)]

    # Regla 1: ¿el año es apto para recuperar el total como FM + Institución?
    validas = iguales = 0
    for _, f in filas:
        t, fm, mi = e.num(col(f, "mtotal")), e.num(col(f, "fm")), e.num(col(f, "minst"))
        if t is not None and fm is not None and mi is not None:
            validas += 1
            iguales += abs(t - (fm + mi)) <= TOL_SUMA
    apto = validas > 0 and iguales >= UMBRAL_R1 * validas

    formato, n_sin, n_sin_ok = formato_rut([col(f, "rut_donante") for _, f in filas]) if idx["rut_donante"] is not None else ("", 0, 0)
    vistos, salida, c = set(), [], Counter()
    for fila, f in filas:
        t, fm, mi = e.num(col(f, "mtotal")), e.num(col(f, "fm")), e.num(col(f, "minst"))
        if t is not None:
            total, origen = t, "numerico"
        elif fm is not None and mi is not None and apto:
            total, origen = fm + mi, "recuperado"
        else:
            total, origen = None, ""

        tipo, fecha = leer_fecha(col(f, "fecha"))
        if fecha == "invalida":
            tipo_final, fecha = "invalida", None
        elif fecha is not None and not (DESDE <= fecha <= HASTA):
            tipo_final, fecha = "fuera_de_rango", None
        else:
            tipo_final = tipo
        a_col = e.num(col(f, "anio"))
        a_col_ok = a_col is not None and a_col == int(a_col) and DESDE.year <= a_col <= HASTA.year
        otro_anio = fecha is not None and fecha.year != anio
        anio_don = anio
        mes = fecha.month if fecha is not None and not otro_anio else ""

        motivo = "monto_no_recuperable" if total is None else ""
        dup = tuple(f) in vistos
        vistos.add(tuple(f))
        fv = col(f, "fm")
        fm_tipo = "vacio" if fv == "" else "monto" if fm is not None else "si_no" if e.norm(fv) in ("SI", "NO") else "otro"
        rut_estado, rut_cuerpo = leer_rut(col(f, "rut_donante"), formato) if idx["rut_donante"] is not None else ("sin_columna", "")
        con_fffd = "\ufffd" in col(f, "donante")

        salida.append({
            "anio_archivo": anio, "fila": fila, "incluida": int(motivo == ""), "motivo_exclusion": motivo,
            "anio_columna": col(f, "anio"), "anio_donacion": anio_don, "mes_donacion": mes,
            "fecha_donacion": fecha.isoformat() if fecha is not None else "", "fecha_tipo": tipo_final,
            "anio_fecha": fecha.year if fecha is not None else "", "fecha_otro_anio": int(otro_anio), "monto_total": g17(total) if total is not None else "", "monto_total_origen": origen,
            "monto_institucion": g17(mi) if mi is not None else "", "monto_fm": g17(fm) if fm is not None else "",
            "fm_tipo": fm_tipo, "fm_si_no": e.norm(fv) if fm_tipo == "si_no" else "", "fm_33_medible": int(fm_tipo == "monto"),
            "duplicado_exacto": int(dup), "donatario": col(f, "donatario"), "rut_institucion": col(f, "rut_institucion"),
            "proyecto": col(f, "proyecto"), "folio_proyecto": col(f, "folio_proyecto"),
            "folio_institucion": col(f, "folio_institucion"), "region": col(f, "region"), "donante": col(f, "donante"),
            "donante_con_fffd": int(con_fffd), "rut_donante": col(f, "rut_donante"), "rut_cuerpo": rut_cuerpo,
            "rut_estado": rut_estado,
        })
        c["incluidas"] += motivo == ""
        c["excl_monto"] += motivo == "monto_no_recuperable"
        c["excl_anio"] += motivo == "sin_anio"
        c["total_" + origen] += origen != ""
        if motivo == "":
            c["fm_" + fm_tipo] += 1
            c["cero"] += total == 0
            c["neg"] += total < 0
        c["fecha_" + tipo_final] += 1
        c["fecha_valida"] += fecha is not None
        c["mes_desconocido"] += mes == ""
        c["otro_anio"] += otro_anio
        c["anio_col_no_valido"] += not a_col_ok
        c["col_distinto_archivo"] += a_col_ok and int(a_col) != anio
        c["dup"] += dup
        c["fffd"] += con_fffd
        c["rut_" + rut_estado] += 1

    incl = [s for s in salida if s["incluida"]]
    conteo = {
        "anio": anio, "variante": variante, "n_filas_hoja": len(filas), "n_incluidas": c["incluidas"],
        "n_excluidas_monto": c["excl_monto"], "n_excluidas_sin_anio": c["excl_anio"],
        "n_total_numerico": c["total_numerico"], "n_total_recuperado": c["total_recuperado"],
        "n_validas_r1": validas, "n_validas_r1_suma_igual": iguales, "anio_apto_r1": str(apto).lower(),
        "n_total_cero": c["cero"], "n_total_negativo": c["neg"],
        "n_fm_monto": c["fm_monto"], "n_fm_si_no": c["fm_si_no"], "n_fm_vacio": c["fm_vacio"], "n_fm_otro": c["fm_otro"],
        "n_fecha_vacia": c["fecha_vacia"], "n_fecha_serial": c["fecha_serial"], "n_fecha_texto_iso": c["fecha_texto_iso"],
        "n_fecha_texto_dma": c["fecha_texto_dma"], "n_fecha_texto_largo": c["fecha_texto_largo"],
        "n_fecha_no_interpretable": c["fecha_no_interpretable"], "n_fecha_invalida": c["fecha_invalida"],
        "n_fecha_fuera_de_rango": c["fecha_fuera_de_rango"], "n_fecha_valida": c["fecha_valida"],
        "n_mes_desconocido": c["mes_desconocido"], "n_fecha_otro_anio": c["otro_anio"],
        "n_anio_columna_no_valido": c["anio_col_no_valido"], "n_anio_columna_distinto_archivo": c["col_distinto_archivo"],
        "n_duplicados_exactos": c["dup"], "n_donante_con_fffd": c["fffd"], "formato_rut_sin_guion": formato,
        "n_rut_sin_guion": n_sin, "n_rut_sin_guion_dv_ok": n_sin_ok, "n_rut_vacio": c["rut_vacio"],
        "n_rut_dv_valido": c["rut_dv_valido"], "n_rut_dv_invalido": c["rut_dv_invalido"], "n_rut_sin_dv": c["rut_sin_dv"],
        "n_rut_mal_formado": c["rut_mal_formado"], "n_rut_sin_columna": c["rut_sin_columna"],
        "suma_monto_total": g17(math.fsum(float(s["monto_total"]) for s in incl)),
        "suma_monto_total_sin_duplicados": g17(math.fsum(float(s["monto_total"]) for s in incl if not s["duplicado_exacto"])),
    }
    por_anio = Counter()
    montos = {}
    for s in incl:
        por_anio[s["anio_donacion"]] += 1
        montos.setdefault(s["anio_donacion"], []).append(float(s["monto_total"]))
    anios = [{"par_anios": f"{anio}-{a}", "anio_archivo": anio, "anio_donacion": a, "n_filas": n,
              "suma_monto_total": g17(math.fsum(montos[a]))} for a, n in sorted(por_anio.items())]
    return salida, conteo, anios, dicc


def main(carpeta, salida_filas, conteos, anios_donacion, diccionario):
    carpeta, salida_filas = Path(carpeta), Path(salida_filas)
    salida_filas.mkdir(parents=True, exist_ok=True)
    anios = sorted(int(p.stem) for p in carpeta.glob("[0-9][0-9][0-9][0-9].xlsx"))
    tablas = {conteos: (CAMPOS_CONTEO, []), anios_donacion: (CAMPOS_ANIOS, []), diccionario: (CAMPOS_DICC, [])}
    for a in anios:
        filas, conteo, por_anio, dicc = armonizar(a, carpeta / f"{a}.xlsx")
        with open(salida_filas / f"{a}.csv", "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=CAMPOS_FILA)
            w.writeheader()
            w.writerows(filas)
        tablas[conteos][1].append(conteo)
        tablas[anios_donacion][1].extend(por_anio)
        tablas[diccionario][1].extend(dicc)
        print("armonizado", a, flush=True)
    for ruta, (campos, filas) in tablas.items():
        with open(ruta, "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=campos)
            w.writeheader()
            w.writerows(filas)


if __name__ == "__main__":
    main(*sys.argv[1:6])
