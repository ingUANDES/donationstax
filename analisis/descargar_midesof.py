"""Descarga las planillas anuales de la página «Donaciones recibidas Ley N°19.885» del Ministerio.

La página embebe la tabla de descargas como JSON (`const raw = [...]`). El script la lee,
descarga cada archivo a <carpeta>/<año>.xlsx (descomprime si viene en .zip) y escribe un
manifiesto CSV (año, url, bytes, sha256, Last-Modified, fecha de descarga) para detectar
cuando el Ministerio cambie un archivo. Los archivos descargados NO se versionan: van en
data_local/, ignorado por git; el manifiesto sí.

Uso: python descargar_midesof.py <carpeta_descarga> <manifiesto.csv>
"""
import csv
import datetime as dt
import hashlib
import html
import io
import json
import re
import sys
import time
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

PAGINA = "https://www.midesof.gob.cl/sociedad-civil/donaciones-sociales/donaciones-recibidas-ley-n19-885/"
UA = {"User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/120 Safari/537.36"}


def leer(url):
    for intento in range(4):
        try:
            req = urllib.request.Request(urllib.parse.quote(url, safe=":/?=&%"), headers=UA)
            with urllib.request.urlopen(req, timeout=300) as r:
                return r.read(), r.headers.get("Last-Modified", "")
        except Exception:
            if intento == 3:
                raise
            time.sleep(3 * (intento + 1))


def enlaces():
    pagina, _ = leer(PAGINA)
    m = re.search(r"const raw = (\[.*?\]);", pagina.decode("utf-8", "replace"), re.S)
    assert m, "la página no trae la tabla de descargas esperada (`const raw = [...]`)"
    filas = []
    for r in json.loads(m.group(1)):
        href = re.search(r'href=\\?"([^"\\]+)', r["columna_3"])
        filas.append((int(r["columna_1"]), html.unescape(href.group(1))))
    return filas


def main(carpeta, manifiesto):
    carpeta = Path(carpeta)
    carpeta.mkdir(parents=True, exist_ok=True)
    hoy = dt.date.today().isoformat()
    filas = []
    for anio, url in sorted(enlaces()):
        datos, lm = leer(url)
        if url.lower().endswith(".zip"):
            z = zipfile.ZipFile(io.BytesIO(datos))
            nombres = [n for n in z.namelist() if n.lower().endswith(".xlsx") and not n.startswith("__MACOSX")]
            assert len(nombres) == 1, f"{anio}: se esperaba un .xlsx en el zip, hay {nombres}"
            datos = z.read(nombres[0])
        (carpeta / f"{anio}.xlsx").write_bytes(datos)
        filas.append([anio, url, len(datos), hashlib.sha256(datos).hexdigest(), lm, hoy])
        print(anio, len(datos), lm, flush=True)
    with open(manifiesto, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["anio", "url", "bytes_xlsx", "sha256_xlsx", "last_modified", "fecha_descarga"])
        w.writerows(filas)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
