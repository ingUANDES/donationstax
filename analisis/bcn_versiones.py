"""Descarga de BCN (LeyChile) las versiones vigentes de una norma y las guarda como texto plano.

Usa el XML público https://www.bcn.cl/leychile/consulta/obtxml?opt=7&idNorma=<id>&idVersion=<fecha>,
que devuelve el texto vigente en <fecha>. Recorre el rango en una grilla y busca por bisección
el primer día de cada cambio de texto; escribe una versión por archivo en la carpeta de salida:

    <nombre>_v<primer_dia_de_vigencia>.txt

con un encabezado (URL, fecha de descarga, fechaVersion del XML, huella y estructuras cortadas). Sin
--partes guarda la norma completa; con --partes solo las estructuras cuyo texto empieza con alguno de
los patrones (expresiones regulares), útil para normas muy largas como la Ley sobre Impuesto a la Renta.

Estructuras cortadas (https://github.com/ingUANDES/donationstax/issues/28). El XML trae incompleto el texto de algunas estructuras, y el corte cae
justo después de un carácter no ASCII, que el XML escribe como referencia numérica («disminuy&#233;»
seguido de «</Texto>»). `cortada` marca las estructuras cuyo texto termina así, y la cabecera de cada
texto las lista. La regla, su contraste con el visor y lo que mostró están en
BLUEPRINT.md#textos-cortados-del-xml-de-bcn («Textos cortados del XML de BCN», en P-01 · Inventario normativo).

Texto completo. El visor de LeyChile pide la norma a servicios-leychile.bcn.cl (get_norma_json), que trae
el HTML de cada estructura. `completar` escribe <texto>_visor.txt con las estructuras cortadas de
<texto>.txt, tomadas del JSON de la misma idVersion y pasadas a texto sin las notas de BCN, solo si el
texto del XML es prefijo propio del del visor (con los espacios normalizados). `contrastar` compara con
el JSON todas las estructuras de los textos dados, sin escribir nada. `marcar` rehace, sin red, las
líneas de la cabecera sobre los cortes en textos ya descargados.

Uso:
    python bcn_versiones.py <idNorma> <nombre> <desde> <hasta> <carpeta_salida> [--partes REGEX ...]
    python bcn_versiones.py marcar <texto.txt> ...
    python bcn_versiones.py completar <texto.txt> ...
    python bcn_versiones.py contrastar <texto.txt> ...
"""
import argparse
import datetime as dt
import hashlib
import json
import re
import sys
import time
import urllib.request
import xml.etree.ElementTree as ET
from collections import Counter
from html.parser import HTMLParser
from pathlib import Path

NS = "{http://www.leychile.cl/esquemas}"
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/120 Safari/537.36"
URL = "https://www.bcn.cl/leychile/consulta/obtxml?opt=7&idNorma={}&idVersion={}"
URL_VISOR = ("https://servicios-leychile.bcn.cl/Navegar/get_norma_json?idNorma={}&idVersion={}"
             "&idLey=&tipoVersion=&cve=&agrupa_partes=1&r=")
SUFIJO_VISOR = "_visor"
MARCA_CORTES = "# Estructuras cortadas"  # prefijos con que `marcar` reconoce sus líneas, aunque cambie el resto
MARCA_VISOR = "# Texto completo de las estructuras cortadas"
CABECERA_CORTES = MARCA_CORTES + " (el texto termina en un carácter no ASCII; https://github.com/ingUANDES/donationstax/issues/28): "
_cache = {}
_cache_visor = {}


PAUSA = 1.0  # segundos entre consultas: BCN responde 429 si se le pide más rápido


def descargar(id_norma, fecha):
    """XML de la versión vigente en `fecha`. BCN devuelve la página de la aplicación en lugar del XML
    para algunas fechas sueltas (p. ej. 2006-03-20 en la Ley sobre Impuesto a la Renta); en ese caso
    se usa el día siguiente, hasta tres días."""
    for dias in range(4):
        f = (dt.date.fromisoformat(fecha) + dt.timedelta(days=dias)).isoformat()
        try:
            return _descargar(id_norma, f)
        except ValueError:
            continue
    raise RuntimeError(f"{fecha}: BCN no devolvió XML ni en los tres días siguientes")


def _descargar(id_norma, fecha):
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
        except ValueError:
            raise
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


def cortada(texto):
    """True si el texto de una estructura termina en un carácter no ASCII (sin contar los espacios
    finales): la firma del corte del XML de BCN (https://github.com/ingUANDES/donationstax/issues/28)."""
    t = texto.rstrip()
    return bool(t) and ord(t[-1]) > 127


def normalizar(texto):
    """Espacios seguidos a uno solo, como compara verifica_citas.py."""
    return re.sub(r"\s+", " ", texto).strip()


def es_prefijo(corto, largo):
    """True si `corto` (texto del XML) es prefijo propio de `largo` (texto del visor), normalizados."""
    a, b = normalizar(corto), normalizar(largo)
    return len(b) > len(a) and b.startswith(a)


def linea_cortes(incluidas):
    """Línea de la cabecera con las estructuras cortadas; `incluidas` es una lista de (idParte, texto)."""
    cortes = [(i, t) for i, t in incluidas if cortada(t)]
    if not cortes:
        return CABECERA_CORTES + "0"
    return CABECERA_CORTES + f"{len(cortes)} | " + ", ".join(
        f"idParte={i} («…{normalizar(t)[-20:]}»)" for i, t in cortes)


_ESTRUCTURA = re.compile(r"### (.*?) idParte=(\d+)(?: (.*))?$")


def leer_texto(ruta):
    """Cabecera (las líneas «# » iniciales) y estructuras de un texto escrito por este script. Cada
    estructura es un dict con tipo, idParte, resto (lo que sigue al idParte en su línea «###») y texto."""
    lineas = Path(ruta).read_text(encoding="utf-8").split("\n")
    n = 0
    while n < len(lineas) and lineas[n].startswith("# "):
        n += 1
    salida, actual = [], None
    for ln in lineas[n:]:
        m = _ESTRUCTURA.match(ln) if ln.startswith("### ") else None
        if m:
            actual = {"tipo": m.group(1), "idParte": m.group(2), "resto": m.group(3) or "", "lineas": []}
            salida.append(actual)
        elif actual is not None:
            actual["lineas"].append(ln)
    for e in salida:
        e["texto"] = "\n".join(e.pop("lineas")).strip()
    return lineas[:n], salida


def ruta_visor(ruta):
    ruta = Path(ruta)
    return ruta.with_name(ruta.stem + SUFIJO_VISOR + ruta.suffix)


def linea_visor(ruta):
    """La línea de la cabecera que nombra <texto>_visor.txt, en una lista; vacía si ese archivo no existe."""
    v = ruta_visor(ruta)
    return [MARCA_VISOR + f": {v.name} (JSON del visor de LeyChile, BCN)"] if v.is_file() else []


def marcar(ruta):
    """Rehace, sin red, la línea de cortes de la cabecera de un texto del XML ya descargado y, si existe
    <texto>_visor.txt, la que lo nombra. Devuelve el número de estructuras cortadas."""
    ruta = Path(ruta)
    if ruta.stem.endswith(SUFIJO_VISOR):
        raise ValueError(f"{ruta.name}: es un texto del visor, no del XML")
    cabecera, estr = leer_texto(ruta)
    base = [c for c in cabecera if not c.startswith((MARCA_CORTES, MARCA_VISOR))]
    nuevas = [linea_cortes([(e["idParte"], e["texto"]) for e in estr])] + linea_visor(ruta)
    cuerpo = ruta.read_text(encoding="utf-8").split("\n")[len(cabecera):]
    ruta.write_text("\n".join(base + nuevas + cuerpo), encoding="utf-8")
    return sum(cortada(e["texto"]) for e in estr)


def descargar_visor(id_norma, fecha):
    """JSON del visor de LeyChile con la versión vigente en `fecha`."""
    clave = (id_norma, fecha)
    if clave in _cache_visor:
        return _cache_visor[clave]
    url = URL_VISOR.format(id_norma, fecha)
    for intento in range(8):
        try:
            time.sleep(PAUSA)
            with urllib.request.urlopen(url, timeout=300) as r:  # responde al agente por defecto de urllib
                datos = json.loads(r.read())
            if not isinstance(datos, dict) or not datos.get("html"):
                raise ValueError(f"{url}: la respuesta no trae el html de la norma")
            _cache_visor[clave] = datos
            return datos
        except ValueError:
            raise
        except Exception as e:  # reintenta con espera creciente; ante 429 espera más
            if intento == 7:
                raise RuntimeError(f"{url}: {e}") from e
            time.sleep(min(90, (15 if "429" in str(e) else 2) * 2 ** intento))


def bloques_visor(datos):
    """{idParte: html} de todas las estructuras del JSON del visor; los hijos van en la clave «h»."""
    salida = {}

    def recorrer(lista):
        for b in lista or []:
            if b.get("i") is not None:
                salida[str(b["i"])] = b.get("t") or ""
            recorrer(b.get("h"))

    recorrer(datos.get("html"))
    return salida


class _Texto(HTMLParser):
    """HTML de una estructura del visor a texto. Omite los elementos de clase «n» (notas de modificación
    y llamadas a nota, que el visor inserta incluso a mitad de palabra) y «np» (texto de las notas de
    BCN); el XML no trae ninguno de los dos. Cada <br> y cada bloque (<div>, <p>) da un salto de línea."""

    OMITIR = {"n", "np"}
    BLOQUE = {"div", "p", "li", "tr", "table"}
    VACIAS = {"br", "img", "hr", "wbr", "input", "meta", "link", "col", "area", "base", "source"}

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.partes, self.pila, self.omitir = [], [], 0

    def handle_starttag(self, tag, attrs):
        if tag in self.VACIAS:
            if tag == "br" and not self.omitir:
                self.partes.append("\n")
            return
        nota = any(k == "class" and v and bool(self.OMITIR & set(v.split())) for k, v in attrs)
        self.omitir += nota
        visible = not self.omitir
        self.pila.append((tag, nota, visible))
        if visible and tag in self.BLOQUE:
            self.partes.append("\n")

    def handle_startendtag(self, tag, attrs):
        if tag == "br" and not self.omitir:
            self.partes.append("\n")

    def handle_endtag(self, tag):
        if tag in self.VACIAS or all(t != tag for t, _, _ in self.pila):
            return
        visible = False
        while self.pila:
            t, nota, visible = self.pila.pop()
            self.omitir -= nota
            if t == tag:
                break
        if visible and tag in self.BLOQUE:
            self.partes.append("\n")

    def handle_data(self, data):
        if not self.omitir:
            self.partes.append(data)


def html_a_texto(html):
    p = _Texto()
    p.feed(html)
    p.close()
    texto = re.sub(r"[ \t\u00a0]+", " ", "".join(p.partes))
    texto = re.sub(r" *\n *", "\n", texto)
    return re.sub(r"\n{3,}", "\n\n", texto).strip()


def _norma_version(cabecera):
    m = re.search(r"idNorma=(\d+)&idVersion=([\d-]+)", cabecera[0] if cabecera else "")
    if not m:
        raise ValueError("la primera línea no trae idNorma e idVersion")
    return m.group(1), m.group(2)


def completar(ruta):
    """Escribe <texto>_visor.txt con el texto completo, del JSON del visor, de las estructuras cortadas
    de <texto>.txt, y rehace las marcas de su cabecera. Una estructura cuyo texto del XML no es prefijo
    propio del del visor no se escribe. Devuelve el número de estructuras completadas."""
    ruta = Path(ruta)
    cabecera, estr = leer_texto(ruta)
    id_norma, fecha = _norma_version(cabecera)
    cortes = [e for e in estr if cortada(e["texto"])]
    if not cortes:
        if ruta_visor(ruta).is_file():
            print(f"{ruta.name}: el XML ya no trae estructuras cortadas; {ruta_visor(ruta).name} sobra")
        return 0
    datos = descargar_visor(id_norma, fecha)
    html = bloques_visor(datos)
    vig = (datos.get("metadatos") or {}).get("vigencia") or {}
    cuerpo, h = [], hashlib.sha256()
    for e in cortes:
        texto = html_a_texto(html.get(e["idParte"], ""))
        if not es_prefijo(e["texto"], texto):
            print(f"{ruta.name}: idParte={e['idParte']}: el texto del XML no es prefijo del del visor; no se completa")
            continue
        h.update((e["idParte"] + "|" + html[e["idParte"]]).encode("utf-8"))
        cuerpo += [f"### {e['tipo']} idParte={e['idParte']} caracteres_xml={len(normalizar(e['texto']))} "
                   f"caracteres_visor={len(normalizar(texto))}", texto, ""]
    if not cuerpo:
        return 0
    lineas = [
        f"# Fuente: {URL_VISOR.format(id_norma, fecha)}",
        f"# Descargado: {dt.date.today().isoformat()} (BCN, LeyChile, JSON del visor; HTML pasado a texto sin las notas de BCN)",
        f"# Vigencia de la versión según el JSON: {vig.get('inicio_vigencia')} a {vig.get('fin_vigencia') or 'sin fin'} "
        f"| idVersion: {fecha}",
        f"# Huella (sha256 del HTML de las estructuras incluidas): {h.hexdigest()}",
        f"# Completa las estructuras cortadas de {ruta.name}: en cada una, el texto del XML es prefijo de este "
        "(caracteres_xml y caracteres_visor cuentan el texto con los espacios normalizados)",
        "",
    ]
    ruta_visor(ruta).write_text("\n".join(lineas + cuerpo), encoding="utf-8")
    marcar(ruta)
    return len(cuerpo) // 3


def contrastar(rutas):
    """Compara con el JSON del visor (misma idVersion) cada estructura de los textos del XML dados:
    igual, cortada (el texto del XML es prefijo propio del del visor), distinta o sin_visor. Imprime una
    línea por estructura que no es igual y un resumen frente a la regla de `cortada`; no escribe nada."""
    total, n_textos = Counter(), 0
    for ruta in map(Path, rutas):
        cabecera, estr = leer_texto(ruta)
        html = bloques_visor(descargar_visor(*_norma_version(cabecera)))
        n_textos += 1
        for e in estr:
            a = normalizar(e["texto"])
            b = normalizar(html_a_texto(html[e["idParte"]])) if e["idParte"] in html else None
            if b is None:
                clase = "sin_visor"
            elif a == b:
                clase = "igual"
            else:
                clase = "cortada" if es_prefijo(a, b) else "distinta"
            regla = cortada(e["texto"])
            total[clase, regla] += 1
            if clase != "igual":
                k = next((i for i, (x, y) in enumerate(zip(a, b or "")) if x != y), min(len(a), len(b or "")))
                print(f"{ruta.name}\tidParte={e['idParte']}\t{clase}\tregla={'sí' if regla else 'no'}\t"
                      f"XML {len(a)} y visor {len(b or '')} caracteres; desde el {k}: XML {a[k:k + 25]!r}, "
                      f"visor {(b or '')[k:k + 25]!r}")
    clases = Counter()
    for (c, _), n in total.items():
        clases[c] += n
    no_cortadas = sum(n for (c, r), n in total.items() if c != "cortada")
    print(f"estructuras: {sum(clases.values())} en {n_textos} textos | " +
          " | ".join(f"{c}: {clases[c]}" for c in ("igual", "cortada", "distinta", "sin_visor")) +
          f" | la regla marca {total['cortada', True]} de {clases['cortada']} cortadas y "
          f"{sum(n for (c, r), n in total.items() if c != 'cortada' and r)} de {no_cortadas} no cortadas")
    return total


def render(datos, partes, id_norma, fecha_vig):
    raiz = ET.fromstring(datos)
    ident = raiz.find(NS + "Identificador")
    cuerpo, incluidas = [], []
    for a, t in estructuras(raiz):
        if not seleccionada(t, partes):
            continue
        t = re.sub(r"[ \t]+", " ", t).strip()
        incluidas.append((a.get("idParte"), t))
        cuerpo.append(f"### {a.get('tipoParte')} idParte={a.get('idParte')} fechaVersion={a.get('fechaVersion')} "
                      f"derogado={a.get('derogado')}")
        cuerpo.append(t)
        cuerpo.append("")
    lineas = [
        f"# Fuente: {URL.format(id_norma, fecha_vig)}",
        f"# Descargado: {dt.date.today().isoformat()} (BCN, LeyChile, XML obtxml opt=7)",
        f"# fechaVersion del XML: {raiz.get('fechaVersion')} | publicación: {ident.get('fechaPublicacion')} "
        f"| promulgación: {ident.get('fechaPromulgacion')} | vigente en idVersion: {fecha_vig}",
        f"# Huella (sha256 de las estructuras incluidas): {huella(datos, partes)}",
        linea_cortes(incluidas),
        "",
    ]
    return "\n".join(lineas + cuerpo)


def main():
    if len(sys.argv) > 1 and sys.argv[1] in ("marcar", "completar", "contrastar"):
        orden, rutas = sys.argv[1], sys.argv[2:]
        if orden == "contrastar":
            contrastar(rutas)
            return 0
        for r in rutas:
            n = marcar(r) if orden == "marcar" else completar(r)
            print(f"{Path(r).name}: {n} estructuras {'cortadas' if orden == 'marcar' else 'completadas'}")
        return 0
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
    con_cortes = []
    for f in fechas:
        datos = descargar(a.id_norma, f.isoformat())
        ruta = out / f"{a.nombre}_v{f.isoformat()}.txt"
        ruta.write_text(render(datos, a.partes, a.id_norma, f.isoformat()), encoding="utf-8")
        n = marcar(ruta)  # agrega la línea que nombra <texto>_visor.txt si ese archivo ya existe
        raiz = ET.fromstring(datos)
        con_cortes += [ruta] * bool(n)
        print(f"{f.isoformat()}  fechaVersion={raiz.get('fechaVersion')}  {ruta.name}" + (f"  cortadas: {n}" if n else ""))
    if con_cortes:
        print("Hay estructuras cortadas; su texto completo: python bcn_versiones.py completar "
              + " ".join(str(r) for r in con_cortes))


if __name__ == "__main__":
    sys.exit(main())
