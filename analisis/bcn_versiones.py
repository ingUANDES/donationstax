"""Descarga de BCN (LeyChile) las versiones vigentes de una norma y las guarda como texto plano.

Usa el XML público https://www.bcn.cl/leychile/consulta/obtxml?opt=7&idNorma=<id>&idVersion=<fecha>,
que devuelve el texto vigente en <fecha>. Recorre el rango en una grilla y busca por bisección
el primer día de cada cambio de texto; escribe una versión por archivo en la carpeta de salida:

    <nombre>_v<primer_dia_de_vigencia>.txt

con un encabezado (URL, fecha de descarga, fechaVersion del XML, huella). Sin --partes guarda la
norma completa; con --partes solo las estructuras cuyo texto empieza con alguno de los patrones
(expresiones regulares), útil para normas muy largas como la Ley sobre Impuesto a la Renta.

Uso: python bcn_versiones.py <idNorma> <nombre> <desde> <hasta> <carpeta_salida> [--partes REGEX ...]
"""
import argparse
import datetime as dt
import hashlib
import re
import sys
import time
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path

NS = "{http://www.leychile.cl/esquemas}"
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/120 Safari/537.36"
URL = "https://www.bcn.cl/leychile/consulta/obtxml?opt=7&idNorma={}&idVersion={}"
_cache = {}


PAUSA = 1.0  # segundos entre consultas: BCN responde 429 si se le pide más rápido


def descargar(id_norma, fecha):
    clave = (id_norma, fecha)
    if clave in _cache:
        return _cache[clave]
    for intento in range(8):
        try:
            time.sleep(PAUSA)
            req = urllib.request.Request(URL.format(id_norma, fecha), headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=120) as r:
                datos = r.read()
            if not datos.lstrip().startswith(b"<?xml"):
                raise ValueError("la respuesta no es XML")
            _cache[clave] = datos
            return datos
        except Exception as e:  # reintenta con espera creciente; ante 429 espera más
            if intento == 7:
                raise RuntimeError(f"{fecha}: {e}") from e
            time.sleep(min(90, (15 if "429" in str(e) else 2) * 2 ** intento))


def estructuras(raiz):
    """Recorre las EstructuraFuncional en orden de documento."""
    for e in raiz.iter(NS + "EstructuraFuncional"):
        t = e.find(NS + "Texto")
        yield e.attrib, (t.text or "") if t is not None else ""


def seleccionada(texto, partes):
    return not partes or any(re.match(p, re.sub(r"[ \t]+", " ", texto).strip()) for p in partes)


def huella(datos, partes=()):
    raiz = ET.fromstring(datos)
    h = hashlib.sha256()
    for a, t in estructuras(raiz):
        if not seleccionada(t, partes):
            continue
        h.update((a.get("idParte", "") + "|" + a.get("derogado", "") + "|" + t).encode("utf-8"))
    return h.hexdigest()


def cambios(id_norma, d0, d1, partes=(), paso=365):
    """Primeros días de cada cambio de texto entre d0 y d1 (d0 se cuenta como inicio)."""
    fechas = []
    d = d0
    while d < d1:
        fechas.append(d)
        d += dt.timedelta(days=paso)
    fechas.append(d1)
    hs = [huella(descargar(id_norma, f.isoformat()), partes) for f in fechas]
    puntos = dict(zip(fechas, hs))

    def buscar(a, ha, b, hb):
        if ha == hb:
            return []
        if (b - a).days == 1:
            return [b]
        m = a + (b - a) // 2
        if m not in puntos:
            puntos[m] = huella(descargar(id_norma, m.isoformat()), partes)
        return buscar(a, ha, m, puntos[m]) + buscar(m, puntos[m], b, hb)

    salida = [d0]
    for a, b in zip(fechas, fechas[1:]):
        salida += buscar(a, puntos[a], b, puntos[b])
    return salida


def render(datos, partes, id_norma, fecha_vig):
    raiz = ET.fromstring(datos)
    ident = raiz.find(NS + "Identificador")
    lineas = [
        f"# Fuente: {URL.format(id_norma, fecha_vig)}",
        f"# Descargado: {dt.date.today().isoformat()} (BCN, LeyChile, XML obtxml opt=7)",
        f"# fechaVersion del XML: {raiz.get('fechaVersion')} | publicación: {ident.get('fechaPublicacion')} "
        f"| promulgación: {ident.get('fechaPromulgacion')} | vigente en idVersion: {fecha_vig}",
        f"# Huella (sha256 de las estructuras incluidas): {huella(datos, partes)}",
        "",
    ]
    for a, t in estructuras(raiz):
        if not seleccionada(t, partes):
            continue
        t = re.sub(r"[ \t]+", " ", t).strip()
        lineas.append(f"### {a.get('tipoParte')} idParte={a.get('idParte')} fechaVersion={a.get('fechaVersion')} "
                      f"derogado={a.get('derogado')}")
        lineas.append(t)
        lineas.append("")
    return "\n".join(lineas)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("id_norma")
    p.add_argument("nombre")
    p.add_argument("desde")
    p.add_argument("hasta")
    p.add_argument("salida")
    p.add_argument("--partes", nargs="*", default=[])
    a = p.parse_args()
    d0, d1 = dt.date.fromisoformat(a.desde), dt.date.fromisoformat(a.hasta)
    out = Path(a.salida)
    out.mkdir(parents=True, exist_ok=True)
    fechas = cambios(a.id_norma, d0, d1, a.partes)
    for f in fechas:
        datos = descargar(a.id_norma, f.isoformat())
        ruta = out / f"{a.nombre}_v{f.isoformat()}.txt"
        ruta.write_text(render(datos, a.partes, a.id_norma, f.isoformat()), encoding="utf-8")
        raiz = ET.fromstring(datos)
        print(f"{f.isoformat()}  fechaVersion={raiz.get('fechaVersion')}  {ruta.name}")


if __name__ == "__main__":
    sys.exit(main())
