"""Descarga del SII las páginas con la UTM mensual de 2008 a 2025 (https://github.com/ingUANDES/donationstax/issues/33).

El SII publica una página por año con la UTM, la UTA, el IPC y sus variaciones de cada mes. Las de 2013
en adelante están en https://www.sii.cl/valores_y_fechas/utm/utmAAAA.htm; las de 2008 a 2012, en
https://www.sii.cl/pagina/valores/utm/utmAAAA.htm, adonde remite la opción «Años anteriores» de las
primeras. El script prueba la ruta nueva y, si responde 404 o una página sin tabla (la ruta antigua de
2013 a 2017 responde un redireccionamiento por JavaScript), la antigua. Usa el agente por omisión de
urllib.

Escribe <carpeta>/utmAAAA.htm tal como llega (son públicas y pequeñas, y se versionan en
datos/fuentes/sii_utm/ para que make verificar las lea sin red) y un manifiesto CSV (año, URL, bytes,
sha256, Last-Modified, fecha de descarga). Si el manifiesto ya existe, informa qué años cambiaron de
huella antes de reescribirlo. La lectura de las páginas está en utm.py y utm.R.

Uso: python descargar_utm.py <carpeta> <manifiesto.csv> [año_desde año_hasta]
"""
import csv
import datetime as dt
import hashlib
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

RUTAS = ("https://www.sii.cl/valores_y_fechas/utm/utm{a}.htm", "https://www.sii.cl/pagina/valores/utm/utm{a}.htm")
COLUMNAS = ["anio", "url", "bytes_html", "sha256_html", "last_modified", "fecha_descarga"]


def pedir(url):
    for intento in range(4):
        try:
            with urllib.request.urlopen(url, timeout=120) as r:
                return r.read(), r.headers.get("Last-Modified", "")
        except urllib.error.HTTPError as e:
            if e.code == 404:
                return None, ""
            raise
        except (urllib.error.URLError, TimeoutError, ConnectionError):
            if intento == 3:
                raise
            time.sleep(3 * (intento + 1))


def bajar(anio):
    for plantilla in RUTAS:
        url = plantilla.format(a=anio)
        cuerpo, lm = pedir(url)
        if cuerpo is not None and b"<table" in cuerpo:
            return url, cuerpo, lm
    raise AssertionError(f"{anio}: ninguna ruta del SII trae la tabla de la UTM")


def main(carpeta, manifiesto, desde=2008, hasta=2025):
    carpeta, manifiesto = Path(carpeta), Path(manifiesto)
    carpeta.mkdir(parents=True, exist_ok=True)
    previo = {}
    if manifiesto.is_file():
        with open(manifiesto, newline="", encoding="utf-8") as f:
            previo = {r["anio"]: r["sha256_html"] for r in csv.DictReader(f)}
    hoy = dt.date.today().isoformat()
    filas = []
    for anio in range(desde, hasta + 1):
        url, cuerpo, lm = bajar(anio)
        (carpeta / f"utm{anio}.htm").write_bytes(cuerpo)
        huella = hashlib.sha256(cuerpo).hexdigest()
        if previo and previo.get(str(anio)) != huella:
            print(f"{anio}: huella distinta de la del manifiesto anterior")
        filas.append([anio, url, len(cuerpo), huella, lm, hoy])
    with open(manifiesto, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(COLUMNAS)
        w.writerows(filas)
    print(f"{len(filas)} páginas en {carpeta}; manifiesto {manifiesto}")


if __name__ == "__main__":
    args = sys.argv[1:]
    main(args[0], args[1], *(int(a) for a in args[2:4]))
