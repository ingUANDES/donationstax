"""Descarga la nómina de donaciones de la Ley 21.440 publicada en donacionesley21440.gob.cl.

La portada enlaza una página por año (/donaciones/donaciones-AAAA). Cada página responde con un
redireccionamiento a un .xlsx en cdn.hacienda.cl cuya URL va firmada (_expiration, _hash) y vence a
los pocos minutos, así que el script pide la página del año sin seguir el redireccionamiento y
descarga el archivo de inmediato. La ruta del CDN trae una huella de 64 caracteres hexadecimales
(/attachment/<huella>/<id>); el script exige que sea el sha256 del archivo descargado.

Escribe <carpeta>/<año>.xlsx y un manifiesto CSV (año, página, ruta del CDN sin la firma, nombre
original, bytes, sha256, Last-Modified, fecha de descarga). Los .xlsx NO se versionan: van en
data_local/, ignorado por git; el manifiesto sí. La firma de la URL no se guarda.

Uso: python descargar_ley21440.py <carpeta_descarga> <manifiesto.csv>
"""
import csv
import datetime as dt
import hashlib
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

PORTADA = "https://donacionesley21440.gob.cl/donaciones"
CDN = "cdn.hacienda.cl"
RUTA_CDN = re.compile(r"^/asset/ley21440/attachment/([0-9a-f]{64})/[0-9]+$")


class SinRedireccion(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


def reintentar(f, *args):
    for intento in range(4):
        try:
            return f(*args)
        except (urllib.error.URLError, TimeoutError, ConnectionError):
            if intento == 3:
                raise
            time.sleep(3 * (intento + 1))


def paginas():
    with urllib.request.urlopen(PORTADA, timeout=120) as r:
        html = r.read().decode("utf-8", "replace")
    anios = sorted({int(a) for a in re.findall(r'href="/donaciones/donaciones-(\d{4})"', html)})
    assert anios, "la portada no enlaza páginas /donaciones/donaciones-AAAA"
    return [(a, f"{PORTADA}/donaciones-{a}") for a in anios]


def destino(pagina):
    """URL firmada del archivo: la página del año responde 302 y no se sigue."""
    try:
        urllib.request.build_opener(SinRedireccion).open(pagina, timeout=120)
    except urllib.error.HTTPError as e:
        if e.code in (301, 302, 303, 307, 308) and e.headers.get("Location"):
            return urllib.parse.urljoin(pagina, e.headers["Location"])
        raise
    raise AssertionError(f"{pagina}: se esperaba un redireccionamiento al archivo")


def bajar(pagina):
    url = destino(pagina)
    with urllib.request.urlopen(url, timeout=600) as r:
        datos = r.read()
        lm = r.headers.get("Last-Modified", "")
        cd = r.headers.get("Content-Disposition", "")
    return url, datos, lm, cd


def nombre_original(cd):
    m = re.search(r"filename\*=UTF-8''([^;]+)", cd)
    if m:
        return urllib.parse.unquote(m.group(1))
    m = re.search(r'filename="?([^";]+)"?', cd)
    return m.group(1) if m else ""


def main(carpeta, manifiesto):
    carpeta = Path(carpeta)
    carpeta.mkdir(parents=True, exist_ok=True)
    hoy = dt.date.today().isoformat()
    filas = []
    for anio, pagina in paginas():
        url, datos, lm, cd = reintentar(bajar, pagina)
        u = urllib.parse.urlsplit(url)
        assert u.hostname == CDN, f"{anio}: el archivo no viene de {CDN} sino de {u.hostname}"
        m = RUTA_CDN.match(u.path)
        assert m, f"{anio}: ruta del CDN inesperada {u.path}"
        sha = hashlib.sha256(datos).hexdigest()
        assert sha == m.group(1), f"{anio}: sha256 {sha} distinto de la huella de la ruta {m.group(1)}"
        assert datos[:2] == b"PK", f"{anio}: el archivo no es un .xlsx (zip)"
        (carpeta / f"{anio}.xlsx").write_bytes(datos)
        filas.append([anio, pagina, f"https://{u.hostname}{u.path}", nombre_original(cd), len(datos), sha, lm, hoy])
        print(anio, len(datos), lm, flush=True)
    with open(manifiesto, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["anio", "pagina", "url_archivo_sin_firma", "nombre_original", "bytes_xlsx", "sha256_xlsx",
                    "last_modified", "fecha_descarga"])
        w.writerows(filas)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
