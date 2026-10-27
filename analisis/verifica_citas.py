"""Comprueba que cada fila 'verificada' de normativa/cambios.csv cita la norma textualmente.

Para cada fila con estado_verificacion == 'verificada' exige fuente_url,
fecha_fuente y version_texto, y que cita_textual aparezca (tras normalizar
espacios) en normativa/textos/<texto_archivo>. Las demás filas se cuentan,
no se validan. Falla con AssertionError. La autoprueba comprueba que una cita
inexistente sí hace fallar la comprobación.

Informa además cuántas estructuras de los textos del XML de BCN están cortadas
(https://github.com/ingUANDES/donationstax/issues/28; regla `cortada` de bcn_versiones.py). Exige que la cabecera de cada
texto las liste como lo hace hoy la regla, que nombre <texto>_visor.txt si ese
archivo existe, y que en <texto>_visor.txt cada estructura esté cortada en
<texto>.txt y su texto del XML sea prefijo propio del del visor. La autoprueba
comprueba que una cabecera desactualizada y un texto del visor que no continúa
al del XML hacen fallar la comprobación.

Uso: python verifica_citas.py [cambios.csv] [carpeta_textos]
"""
import csv
import re
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import bcn_versiones as bcn  # noqa: E402

COLUMNAS = [
    "norma", "tipo_norma", "articulo_inciso", "vigencia_desde", "vigencia_hasta",
    "efecto_precio", "parametros", "cita_textual", "texto_archivo", "fuente_url",
    "fecha_fuente", "version_texto", "estado_verificacion",
]
OBLIGATORIAS = ["fuente_url", "fecha_fuente", "version_texto", "texto_archivo", "cita_textual"]
FUENTE_XML = "# Fuente: " + bcn.URL.split("?")[0]


def norm(s):
    return re.sub(r"\s+", " ", s).strip()


def verificar(csv_ruta, textos):
    with open(csv_ruta, newline="", encoding="utf-8") as f:
        lector = csv.DictReader(f)
        assert lector.fieldnames == COLUMNAS, f"columnas inesperadas: {lector.fieldnames}"
        filas = list(lector)
    verificadas = 0
    for i, r in enumerate(filas, start=2):
        if r["estado_verificacion"] != "verificada":
            continue
        for c in OBLIGATORIAS:
            assert r[c].strip(), f"{csv_ruta}:{i} verificada sin {c}"
        ruta = Path(textos) / r["texto_archivo"]
        assert ruta.is_file(), f"{csv_ruta}:{i} no existe {ruta}"
        texto = norm(ruta.read_text(encoding="utf-8"))
        assert norm(r["cita_textual"]) in texto, (
            f"{csv_ruta}:{i} la cita no aparece en {ruta.name}: {r['cita_textual'][:60]!r}"
        )
        verificadas += 1
    return len(filas), verificadas


def cortes(textos):
    """Estructuras cortadas de los textos del XML de BCN en `textos` y su texto completo del visor.
    Devuelve (textos del XML, estructuras, cortadas, textos con cortes, completadas)."""
    xml, visor = {}, {}
    for ruta in sorted(Path(textos).glob("*.txt")):
        if ruta.stem.endswith(bcn.SUFIJO_VISOR):
            visor[ruta.name] = ruta
        elif ruta.read_text(encoding="utf-8").startswith(FUENTE_XML):
            xml[ruta.name] = ruta
    n_estructuras, cortadas = 0, {}
    for nombre, ruta in xml.items():
        cabecera, estr = bcn.leer_texto(ruta)
        n_estructuras += len(estr)
        esperada = bcn.linea_cortes([(e["idParte"], e["texto"]) for e in estr])
        assert [c for c in cabecera if c.startswith(bcn.MARCA_CORTES)] == [esperada], (
            f"{nombre}: la cabecera no lista sus estructuras cortadas como la regla de hoy "
            "(rehacer con «python analisis/bcn_versiones.py marcar»)"
        )
        assert [c for c in cabecera if c.startswith(bcn.MARCA_VISOR)] == bcn.linea_visor(ruta), (
            f"{nombre}: la cabecera y la carpeta no coinciden en {bcn.ruta_visor(ruta).name} (rehacer con «marcar»)"
        )
        cortadas[nombre] = {e["idParte"]: e["texto"] for e in estr if bcn.cortada(e["texto"])}
    completadas = 0
    for nombre, ruta in visor.items():
        base = nombre[: -len(bcn.SUFIJO_VISOR + ".txt")] + ".txt"
        assert base in xml, f"{nombre}: no existe el texto del XML {base}"
        _, estr = bcn.leer_texto(ruta)
        vistos = set()
        for e in estr:
            i = e["idParte"]
            assert i not in vistos, f"{nombre}: idParte={i} repetido"
            vistos.add(i)
            assert i in cortadas[base], f"{nombre}: idParte={i} no está cortada en {base}"
            assert bcn.es_prefijo(cortadas[base][i], e["texto"]), (
                f"{nombre}: idParte={i}: el texto de {base} no es prefijo propio del del visor"
            )
            completadas += 1
    n_cortadas = sum(len(v) for v in cortadas.values())
    return len(xml), n_estructuras, n_cortadas, sum(1 for v in cortadas.values() if v), completadas


def autoprueba():
    with tempfile.TemporaryDirectory() as d:
        d = Path(d)
        (d / "t.txt").write_text("Artículo 1°.- el   texto  de prueba", encoding="utf-8")
        base = dict.fromkeys(COLUMNAS, "x")
        base.update(estado_verificacion="verificada", texto_archivo="t.txt")
        for cita, debe_pasar in [("el texto de prueba", True), ("texto que no está", False)]:
            with open(d / "c.csv", "w", newline="", encoding="utf-8") as f:
                w = csv.DictWriter(f, fieldnames=COLUMNAS)
                w.writeheader()
                w.writerow({**base, "cita_textual": cita})
            try:
                verificar(d / "c.csv", d)
                pasó = True
            except AssertionError:
                pasó = False
            assert pasó == debe_pasar, f"autoprueba: cita {cita!r} debía {'pasar' if debe_pasar else 'fallar'}"

    xml = ["# Fuente: " + bcn.URL.format(1, "2000-01-01"), "# Huella: x", "",
           "### Artículo idParte=1 fechaVersion=2000-01-01 derogado=no derogado", "Primero, que se corta en disminuyé", "",
           "### Artículo idParte=2 fechaVersion=2000-01-01 derogado=no derogado", "Segundo, completo.", ""]
    visor = ["# Fuente: " + bcn.URL_VISOR.format(1, "2000-01-01"), "",
             "### Artículo idParte=1 caracteres_xml=34 caracteres_visor=47", "Primero, que se corta en\ndisminuyéndose en el visor.", ""]
    vieja = bcn.MARCA_CORTES + " (marca de una versión anterior): 1"
    casos = [  # (texto del visor o None, ¿se marca la cabecera?, línea extra, ¿debe pasar?, cortadas, completadas)
        (visor, True, None, True, 1, 1),
        (None, True, None, True, 1, 0),
        (visor, False, None, False, None, None),
        (visor[:-2] + ["Otro texto que no continúa al del XML.", ""], True, None, False, None, None),
        (visor, True, vieja, False, None, None),
    ]
    for texto_visor, marcar, extra, debe_pasar, n_cortadas, n_completadas in casos:
        with tempfile.TemporaryDirectory() as d:
            d = Path(d)
            (d / "n_v2000-01-01.txt").write_text("\n".join(xml), encoding="utf-8")
            if texto_visor:
                (d / "n_v2000-01-01_visor.txt").write_text("\n".join(texto_visor), encoding="utf-8")
            if marcar:
                bcn.marcar(d / "n_v2000-01-01.txt")
            if extra:
                lineas = (d / "n_v2000-01-01.txt").read_text(encoding="utf-8").split("\n")
                (d / "n_v2000-01-01.txt").write_text("\n".join(lineas[:1] + [extra] + lineas[1:]), encoding="utf-8")
            try:
                r = cortes(d)
                pasó = True
            except AssertionError:
                pasó = False
            assert pasó == debe_pasar, f"autoprueba de cortes: el caso debía {'pasar' if debe_pasar else 'fallar'}"
            if pasó:
                assert r == (1, 2, n_cortadas, 1, n_completadas), f"autoprueba de cortes: conteos {r}"


if __name__ == "__main__":
    raiz = Path(__file__).resolve().parent.parent
    ruta = sys.argv[1] if len(sys.argv) > 1 else raiz / "normativa" / "cambios.csv"
    textos = sys.argv[2] if len(sys.argv) > 2 else raiz / "normativa" / "textos"
    autoprueba()
    total, ok = verificar(ruta, textos)
    print(f"citas OK: {total} filas, {ok} verificadas con cita textual comprobada")
    n_xml, n_estr, n_cortadas, n_con, n_completas = cortes(textos)
    print(f"textos del XML de BCN: {n_xml} archivos y {n_estr} estructuras; cortadas (el texto termina en un carácter "
          f"no ASCII): {n_cortadas}, en {n_con} archivos; con texto completo del visor: {n_completas}; "
          f"sin completar: {n_cortadas - n_completas}")
