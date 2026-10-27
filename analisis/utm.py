"""Lee la UTM mensual de 2008 a 2025 de las páginas del SII (https://github.com/ingUANDES/donationstax/issues/33).

Entrada: la carpeta con utmAAAA.htm (datos/fuentes/sii_utm/, de descargar_utm.py) y su manifiesto
(datos/descargas_utm.csv). Salidas:

- utm_mensual.csv: anio, mes (1 a 12), utm y uta en pesos, y ipc_variacion_mensual_pct, la variación
  mensual del IPC que publica la página («Corresponde a la publicada oficialmente por el INE en cada
  mes», nota (3)), con punto decimal. Es una transformación de texto: los valores no pasan por números
  de punto flotante, y la salida de R debe ser idéntica byte a byte.
- utm_por_anio.csv: por año, la UTM de diciembre, la mínima y la máxima del año con su mes (el primero
  si se repite), utm_dic/utm_min, utm_dic/utm_max y ln(utm_dic/utm_min). Para una donación de mes
  conocido, convertirla con la UTM de diciembre en vez de la de su mes multiplica su monto en UTM por
  utm_mes/utm_dic, que queda entre 1/razon_dic_sobre_min y 1/razon_dic_sobre_max.

Reglas de lectura (las mismas en utm.R):

1. El archivo se decodifica como UTF-8 estricto.
2. El año es el número de cuatro dígitos de la primera celda de encabezado con rowspan 2 y debe ser el
   del nombre del archivo.
3. Una fila de mes es <tr><th ...>Mes</th> seguida de exactamente seis <td ...>valor</td>; los meses
   deben ser los doce, en orden, con su nombre en castellano.
4. UTM y UTA son enteros con punto de miles (^[0-9]{1,3}(\\.[0-9]{3})*$). La variación mensual es
   -?[0-9]+([,.][0-9]+)? y se escribe con punto: casi todas traen coma decimal, pero la de enero de
   2008 es «0» y la de diciembre de 2011, «0.6». Las otras tres celdas (IPC en puntos y variaciones
   acumulada y de 12 meses) no se leen: el IPC en puntos cambia de base entre años (nota (6) de las
   páginas).

Comprobaciones (AssertionError si fallan):

- El manifiesto trae los años 2008 a 2025 y el sha256 de cada archivo es el del manifiesto (solo en
  Python; utm.R no tiene sha256 en la base de R).
- UTA = 12 × UTM en cada mes (nota (2) de las páginas: «UTA = Unidad Tributaria Anual (UTM x 12)»).
- Coherencia del reajuste: desde marzo de 2008, |UTM_t − UTM_{t−1}(1 + v_{t−2}/100)| ≤ 0,0005 UTM_{t−1}
  + 1, con v la variación mensual publicada. Que la UTM se reajuste por la variación del IPC del
  segundo mes anterior es inferencia de la serie, sin verificar contra el Código Tributario; el margen
  cubre el redondeo de v a un decimal (0,05 puntos) y de la UTM al peso.
- Autoprueba: una UTM alterada en 1 % y una UTA distinta de 12 × UTM hacen fallar las comprobaciones.

Uso: python utm.py <carpeta_html> <manifiesto.csv> <utm_mensual.csv> <utm_por_anio.csv>
"""
import csv
import hashlib
import math
import re
import sys
from pathlib import Path

ANIOS = range(2008, 2026)
MESES = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre",
         "Octubre", "Noviembre", "Diciembre"]
RE_ANIO = re.compile(r"<th[^>]*rowspan=['\"]2['\"][^>]*>\s*(?:<strong>)?\s*([0-9]{4})\b")
RE_FILA = re.compile(r"<tr>\s*<th[^>]*>\s*([A-Za-z]+)\s*</th>((?:\s*<td[^>]*>[^<]*</td>)+)\s*</tr>")
RE_CELDA = re.compile(r"<td[^>]*>([^<]*)</td>")
RE_ENTERO = re.compile(r"^[0-9]{1,3}(\.[0-9]{3})*$")
RE_VAR = re.compile(r"^-?[0-9]+([,.][0-9]+)?$")
COL_MENSUAL = ["anio", "mes", "utm", "uta", "ipc_variacion_mensual_pct"]
COL_ANIO = ["anio", "utm_diciembre", "utm_min", "mes_utm_min", "utm_max", "mes_utm_max",
            "razon_dic_sobre_min", "razon_dic_sobre_max", "log_dic_sobre_min"]


def leer_pagina(ruta, anio):
    texto = Path(ruta).read_bytes().decode("utf-8")
    m = RE_ANIO.search(texto)
    assert m and int(m.group(1)) == anio, f"{ruta}: el encabezado no es del año {anio}"
    filas = RE_FILA.findall(texto)
    assert [f[0] for f in filas] == MESES, f"{ruta}: meses {[f[0] for f in filas]}"
    out = []
    for mes, (_, celdas) in enumerate(filas, start=1):
        v = [c.strip() for c in RE_CELDA.findall(celdas)]
        assert len(v) == 6, f"{ruta} mes {mes}: {len(v)} celdas"
        assert RE_ENTERO.match(v[0]) and RE_ENTERO.match(v[1]), f"{ruta} mes {mes}: UTM o UTA {v[:2]}"
        assert RE_VAR.match(v[3]), f"{ruta} mes {mes}: variación mensual {v[3]!r}"
        out.append({"anio": anio, "mes": mes, "utm": int(v[0].replace(".", "")),
                    "uta": int(v[1].replace(".", "")), "ipc_variacion_mensual_pct": v[3].replace(",", ".")})
    return out


def comprobar(serie):
    for r in serie:
        assert r["uta"] == 12 * r["utm"], f"{r['anio']}-{r['mes']}: UTA {r['uta']} != 12 × UTM {r['utm']}"
    for t in range(2, len(serie)):
        previa, actual = serie[t - 1]["utm"], serie[t]["utm"]
        v = float(serie[t - 2]["ipc_variacion_mensual_pct"])
        assert abs(actual - previa * (1 + v / 100)) <= 0.0005 * previa + 1, (
            f"{serie[t]['anio']}-{serie[t]['mes']}: UTM {actual} no es {previa} reajustada en {v} %"
        )


def autoprueba(serie):
    for campo, factor in (("utm", 1.01), ("uta", 1.01)):
        alterada = [dict(r) for r in serie]
        alterada[30][campo] = round(alterada[30][campo] * factor)
        try:
            comprobar(alterada)
        except AssertionError:
            continue
        raise AssertionError(f"la comprobación no detectó una alteración de {campo}")


def por_anio(serie):
    out = []
    for anio in ANIOS:
        u = [r["utm"] for r in serie if r["anio"] == anio]
        dic, umin, umax = u[11], min(u), max(u)
        out.append([anio, dic, umin, u.index(umin) + 1, umax, u.index(umax) + 1,
                    repr(dic / umin), repr(dic / umax), repr(math.log(dic / umin))])
    return out


def main(carpeta, manifiesto, sal_mensual, sal_anio):
    with open(manifiesto, newline="", encoding="utf-8") as f:
        man = {int(r["anio"]): r["sha256_html"] for r in csv.DictReader(f)}
    assert sorted(man) == list(ANIOS), f"{manifiesto}: años {sorted(man)}"
    serie = []
    for anio in ANIOS:
        ruta = Path(carpeta) / f"utm{anio}.htm"
        assert hashlib.sha256(ruta.read_bytes()).hexdigest() == man[anio], f"{ruta}: sha256 distinto del manifiesto"
        serie += leer_pagina(ruta, anio)
    comprobar(serie)
    autoprueba(serie)
    with open(sal_mensual, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(COL_MENSUAL)
        w.writerows([r[c] for c in COL_MENSUAL] for r in serie)
    with open(sal_anio, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(COL_ANIO)
        w.writerows(por_anio(serie))
    print(f"UTM: {len(serie)} meses de {ANIOS[0]} a {ANIOS[-1]}; comprobaciones y autoprueba pasan")


if __name__ == "__main__":
    main(*sys.argv[1:5])
