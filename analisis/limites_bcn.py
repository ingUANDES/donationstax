"""Límites de versión de las series de textos del XML de BCN: qué cambia en cada uno, en el XML y en el texto
completo (https://github.com/ingUANDES/donationstax/issues/37).

Una serie son los textos <nombre>_v<fecha>.txt que bcn_versiones.py escribe desde el XML de BCN en una carpeta;
cada versión desde la segunda es un límite. El texto completo de una estructura es el del XML o, si el XML la
corta, el de <texto>_visor.txt (DECISIONES.md#d-21). Clases de un límite frente a la versión anterior:

- cambio: cambian las estructuras (idParte o derogado) o el texto completo de alguna, con los espacios
  normalizados.
- solo corte: el texto completo no cambia y el del XML sí, porque un corte aparece, desaparece o cae en otro
  lugar. No es un cambio de texto ni da una fecha de vigencia (DECISIONES.md#d-21, punto 4).
- solo espacios: el texto completo no cambia y el guardado sí, solo en espacios y sin cortes.
- sin cambio de texto: el texto guardado es el mismo; a lo más cambia la fechaVersion de alguna estructura.

Las versiones de clase «solo corte» se retiran de la serie a <carpeta>/retiradas/, con su _visor.txt. Sin
argumentos, el script imprime la tabla de cada serie con las retiradas intercaladas, que reproduce la del issue
37, y falla con AssertionError si un límite de la serie es «solo corte», si una retirada no lo es o si la
cabecera de una retirada no lista sus cortes como la regla de hoy. La autoprueba comprueba que esas tres
situaciones hacen fallar la comprobación. «Solo espacios» y «sin cambio de texto» se quedan en la serie mientras
DECISIONES.md#d-28 esté en estado Propuesta. Otra autoprueba corre, sin red, la bisección de bcn_versiones.py
sobre XML y JSON del visor ficticios (autoprueba_biseccion).

`visor` usa la red: compara cada límite de la tabla con el JSON del visor de LeyChile de las dos idVersion, que
es como se armó la tabla del issue 37, y falla si el visor no cambia en las mismas estructuras que el texto
completo. `comparar` no usa la red: exige que cada serie de la segunda carpeta (por ejemplo, la bajada de nuevo
con bcn_versiones.py y completada con `completar`) tenga en la primera las mismas versiones y el mismo texto
completo; acepta en la primera, de más, las versiones que DECISIONES.md#d-28 retiraría si se aprueba.

Uso:
    python limites_bcn.py [carpeta]
    python limites_bcn.py visor [carpeta]
    python limites_bcn.py comparar <carpeta> <otra_carpeta>
"""
import datetime as dt
import re
import sys
import tempfile
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import bcn_versiones as bcn  # noqa: E402
import verifica_citas as vc  # noqa: E402

RETIRADAS = "retiradas"
CLASES = ("cambio", "solo corte", "solo espacios", "sin cambio de texto")
RETIRABLES = {"solo corte"}  # DECISIONES.md#d-21, punto 4
SEGUN_D28 = {"solo espacios", "sin cambio de texto"}  # las retiraría DECISIONES.md#d-28, en estado Propuesta
_NOMBRE = re.compile(r"(.+)_v(\d{4}-\d\d-\d\d)\.txt$")
_ATRIBUTOS = re.compile(r"fechaVersion=(\S*) derogado=(.*)$")


def series(carpeta):
    """{nombre: [(fecha, ruta), ...]} de los textos del XML de BCN en `carpeta`, ordenados por fecha."""
    assert Path(carpeta).is_dir(), f"no existe la carpeta {carpeta}"
    salida = {}
    for ruta in sorted(Path(carpeta).glob("*.txt")):
        m = _NOMBRE.match(ruta.name)
        if m and ruta.read_text(encoding="utf-8").startswith(vc.FUENTE_XML):
            salida.setdefault(m.group(1), []).append((m.group(2), ruta))
    return {n: sorted(v) for n, v in salida.items()}


def etiqueta(texto):
    """«art. 20» si la estructura empieza con «Artículo 20», o sus primeros 30 caracteres."""
    t = bcn.normalizar(texto)
    m = re.match(r"(?i)\W*art[ií]culo\s+([^\s.,-]+(?:\s+bis)?)", t)
    return f"art. {m.group(1).rstrip('°º')}" if m else t[:30]


def version(ruta):
    """{idParte: estructura} de un texto del XML. Cada estructura trae el texto guardado, el completo con los
    espacios normalizados (del _visor.txt si está cortada), si está cortada, su fechaVersion y su derogado."""
    _, estr = bcn.leer_texto(ruta)
    rv = bcn.ruta_visor(ruta)
    visor = {e["idParte"]: e["texto"] for e in bcn.leer_texto(rv)[1]} if rv.is_file() else {}
    salida = {}
    for e in estr:
        corte = bcn.cortada(e["texto"])
        completo = visor.get(e["idParte"]) if corte else e["texto"]
        assert completo is not None, f"{ruta.name}: idParte={e['idParte']} está cortada y {rv.name} no trae su texto"
        m = _ATRIBUTOS.match(e["resto"])
        salida[e["idParte"]] = {"texto": e["texto"], "completo": bcn.normalizar(completo), "corte": corte,
                                "fechaVersion": m.group(1) if m else "", "derogado": m.group(2) if m else e["resto"],
                                "etiqueta": etiqueta(e["texto"])}
    return salida


def clasificar(a, b):
    """Clase del límite de la versión `b` frente a la anterior `a` (salidas de version()) y detalle por estructura."""
    clases, detalle = set(), []
    if [(i, a[i]["derogado"]) for i in a] != [(i, b[i]["derogado"]) for i in b]:
        clases.add("cambio")
    detalle += [f"{a[i]['etiqueta']} (idParte {i}): ya no está" for i in a if i not in b]
    for i in b:
        if i not in a:
            detalle.append(f"{b[i]['etiqueta']} (idParte {i}): nueva")
            continue
        x, y = a[i], b[i]
        partes = [f"derogado: {x['derogado']} a {y['derogado']}"] if x["derogado"] != y["derogado"] else []
        if x["completo"] != y["completo"]:
            clases.add("cambio")
            partes.append("cambia")
        elif x["texto"] != y["texto"]:
            clases.add("solo corte" if x["corte"] or y["corte"] else "solo espacios")
            if not (x["corte"] or y["corte"]):
                partes.append("cambian solo espacios")
        if x["corte"] != y["corte"]:
            partes.append("cortado" if y["corte"] else "completo otra vez")
        elif x["corte"] and x["texto"] != y["texto"]:
            partes.append("cortado en otro lugar")
        if x["fechaVersion"] != y["fechaVersion"]:
            partes.append(f"fechaVersion {x['fechaVersion']} a {y['fechaVersion']}")
        if partes:
            detalle.append(f"{y['etiqueta']} (idParte {i}): " + ", ".join(partes))
    return next((c for c in CLASES[:3] if c in clases), CLASES[3]), detalle


def rutas(carpeta):
    """{(nombre, fecha): (ruta, retirada)} de las series de `carpeta` y de su subcarpeta de retiradas."""
    salida = {}
    for base, retirada in ((Path(carpeta), False), (Path(carpeta) / RETIRADAS, True)):
        if base.is_dir():
            for nombre, vs in series(base).items():
                for f, r in vs:
                    assert (nombre, f) not in salida, f"{nombre}_v{f}: está en la serie y en {RETIRADAS}/"
                    salida[nombre, f] = (r, retirada)
    return salida


def tabla(carpeta):
    """Filas (serie, fecha, anterior, clase, detalle, retirada) de cada límite de las series de más de una
    versión, con las retiradas intercaladas. Falla si un límite de la serie es «solo corte», si una retirada no
    lo es o si la cabecera de una retirada no lista sus cortes como la regla de hoy (DECISIONES.md#d-21)."""
    todas = rutas(carpeta)
    assert todas, f"{carpeta}: no hay textos del XML de BCN"
    if (Path(carpeta) / RETIRADAS).is_dir():
        vc.cortes(Path(carpeta) / RETIRADAS)
    filas = []
    for nombre in sorted({n for n, _ in todas}):
        fechas = sorted(f for n, f in todas if n == nombre)
        vers = {f: version(todas[nombre, f][0]) for f in fechas}
        serie = [f for f in fechas if not todas[nombre, f][1]]
        assert serie and serie[0] == fechas[0], f"{nombre}: la primera versión no puede estar en {RETIRADAS}/"
        for f0, f1 in zip(fechas, fechas[1:]):
            clase, detalle = clasificar(vers[f0], vers[f1])
            retirada = todas[nombre, f1][1]
            assert not retirada or clase in RETIRABLES, (
                f"{RETIRADAS}/{nombre}_v{f1}.txt: su límite frente a {f0} es «{clase}» y solo se retira uno «solo corte»")
            filas.append((nombre, f1, f0, clase, detalle, retirada))
        for f0, f1 in zip(serie, serie[1:]):
            clase, _ = clasificar(vers[f0], vers[f1])
            assert clase not in RETIRABLES, (
                f"{nombre}_v{f1}.txt: frente a {f0} solo marca un corte; no es un cambio de texto ni da fecha de "
                f"vigencia (DECISIONES.md#d-21, punto 4): moverla a {RETIRADAS}/")
    return filas


def imprimir(filas, columna_extra=None):
    for nombre in dict.fromkeys(f[0] for f in filas):
        propias = [f for f in filas if f[0] == nombre]
        n_ret = sum(f[5] for f in propias)
        print(f"{nombre}: {len(propias) + 1} versiones, {n_ret} retiradas a {RETIRADAS}/")
        for fila in propias:
            _, f1, f0, clase, detalle, retirada = fila
            extra = f"\t{columna_extra(fila)}" if columna_extra else ""
            print(f"  {f1}{' (retirada)' if retirada else ''}\tfrente a {f0}\t{clase}\t{'; '.join(detalle) or '-'}{extra}")
    conteo = Counter(f[3] for f in filas)
    print(f"límites: {len(filas)} | " + " | ".join(f"{c}: {conteo[c]}" for c in CLASES)
          + f" | retiradas: {sum(f[5] for f in filas)}")


def contrastar_visor(carpeta):
    """Con red: en cada límite de la tabla, las estructuras cuyo texto cambia en el JSON del visor de las dos
    idVersion (con los espacios normalizados) deben ser las mismas cuyo texto completo cambia."""
    filas, todas, cache = tabla(carpeta), rutas(carpeta), {}

    def del_visor(nombre, fecha):
        if (nombre, fecha) not in cache:
            cabecera, estr = bcn.leer_texto(todas[nombre, fecha][0])
            html = bcn.bloques_visor(bcn.descargar_visor(*bcn._norma_version(cabecera)))
            cache[nombre, fecha] = {e["idParte"]: bcn.normalizar(bcn.html_a_texto(html[e["idParte"]]))
                                    if e["idParte"] in html else None for e in estr}
        return cache[nombre, fecha]

    distintos = []

    def columna(fila):
        nombre, f1, f0 = fila[:3]
        a, b = del_visor(nombre, f0), del_visor(nombre, f1)
        va, vb = version(todas[nombre, f0][0]), version(todas[nombre, f1][0])
        en_visor = sorted(i for i in b if b[i] is None or a.get(i) != b[i])
        en_completo = sorted(i for i in vb if i not in va or va[i]["completo"] != vb[i]["completo"])
        if en_visor != en_completo:
            distintos.append(f"{nombre}_v{f1}")
        return "visor: " + (", ".join(vb[i]["etiqueta"] for i in en_visor) or "sin cambio") + (
            "" if en_visor == en_completo else " (NO COINCIDE con el texto completo)")

    imprimir(filas, columna)
    assert not distintos, f"el visor no cambia en las mismas estructuras que el texto completo: {distintos}"


def comparar(carpeta, otra):
    """Sin red: cada serie de `otra` debe estar en `carpeta` con las mismas versiones y el mismo texto completo
    en cada estructura. En `carpeta` se aceptan de más las versiones de clase «solo espacios» o «sin cambio de
    texto», que la bisección ya no da y que DECISIONES.md#d-28 retiraría si se aprueba."""
    a, b, problemas = series(carpeta), series(otra), []
    for nombre, vs in sorted(b.items()):
        fa, fb = [f for f, _ in a.get(nombre, [])], [f for f, _ in vs]
        de_mas = []
        for f0, f1 in zip(fa, fa[1:]):
            if f1 not in fb:
                clase, _ = clasificar(version(dict(a[nombre])[f0]), version(dict(a[nombre])[f1]))
                (de_mas if clase in SEGUN_D28 else problemas).append(f"{nombre}_v{f1} ({clase})")
        if fa[:1] != fb[:1]:
            problemas.append(f"{nombre}: empieza en {fa[:1]} y en {fb[:1]}")
        problemas += [f"{nombre}_v{f}: solo en {otra}" for f in fb if f not in fa]
        for f, r in vs:
            if f in fa:
                x, y = version(dict(a[nombre])[f]), version(r)
                if [(i, x[i]["derogado"], x[i]["completo"]) for i in x] != [(i, y[i]["derogado"], y[i]["completo"]) for i in y]:
                    problemas.append(f"{nombre}_v{f}: el texto completo no es el mismo")
        print(f"{nombre}: {len(fb)} versiones en {otra} y {len(fa)} en {carpeta}; de más en {carpeta}, según "
              f"DECISIONES.md#d-28 (en estado Propuesta): {', '.join(de_mas) or 'ninguna'}")
    assert not problemas, "las series no coinciden: " + "; ".join(problemas)


def autoprueba():
    completo, cortado = "Primero, que el XML corta en disminuyéndose.", "Primero, que el XML corta en disminuyé"

    def escribir(d, fecha, texto, visor=None, marcar=True):
        ruta = d / f"n_v{fecha}.txt"
        ruta.write_text("\n".join(["# Fuente: " + bcn.URL.format(1, fecha), "# Huella: x", "",
                                   "### Artículo idParte=1 fechaVersion=2000-01-01 derogado=no derogado", texto, ""]),
                        encoding="utf-8")
        if visor:
            bcn.ruta_visor(ruta).write_text("\n".join(["# Fuente: " + bcn.URL_VISOR.format(1, fecha), "",
                                                       "### Artículo idParte=1 caracteres_xml=0 caracteres_visor=0", visor, ""]),
                                            encoding="utf-8")
        if marcar:
            bcn.marcar(ruta)

    casos = [  # (versiones: (fecha, texto, visor, ¿retirada?, ¿se marca?), ¿debe pasar?, clases esperadas)
        ([("2000-01-01", completo, None, False, True), ("2001-01-01", cortado, completo, True, True),
          ("2002-01-01", "Segundo texto.", None, False, True)], True, ["solo corte", "cambio"]),
        ([("2000-01-01", completo, None, False, True), ("2001-01-01", cortado, completo, False, True),
          ("2002-01-01", "Segundo texto.", None, False, True)], False, None),
        ([("2000-01-01", completo, None, False, True), ("2001-01-01", "Otro texto.", None, True, True),
          ("2002-01-01", "Segundo texto.", None, False, True)], False, None),
        ([("2000-01-01", completo, None, False, True), ("2001-01-01", cortado, completo, True, False),
          ("2002-01-01", "Segundo texto.", None, False, True)], False, None),
        ([("2000-01-01", completo, None, False, True), ("2001-01-01", completo.replace(" ", "  ", 1), None, False, True)],
         True, ["solo espacios"]),
    ]
    for versiones, debe_pasar, esperadas in casos:
        with tempfile.TemporaryDirectory() as d:
            d = Path(d)
            (d / RETIRADAS).mkdir()
            for fecha, texto, visor, retirada, marcar in versiones:
                escribir(d / RETIRADAS if retirada else d, fecha, texto, visor, marcar)
            try:
                filas = tabla(d)
                pasó = True
            except AssertionError:
                pasó = False
            assert pasó == debe_pasar, f"autoprueba: el caso {[v[0] for v in versiones]} debía {'pasar' if debe_pasar else 'fallar'}"
            if pasó:
                assert [f[3] for f in filas] == esperadas, f"autoprueba: clases {[f[3] for f in filas]}"


def _xml(texto):
    return ('<?xml version="1.0" encoding="UTF-8"?><Norma xmlns="http://www.leychile.cl/esquemas">'
            '<EstructuraFuncional idParte="1" derogado="no derogado"><Texto>' + texto +
            "</Texto></EstructuraFuncional></Norma>").encode("utf-8")


def autoprueba_biseccion():
    """Sin red: bcn_versiones.cambios sobre XML y JSON del visor ficticios, con una estructura y 300 días.
    Un corte que aparece o desaparece y una diferencia solo de espacios no dan límite; un cambio del texto
    completo después del punto del corte sí lo da, aunque el XML traiga el mismo texto cortado en las dos fechas."""
    d0 = dt.date(2000, 1, 1)
    completo, cortado = "Primero, que el XML corta en disminuyéndose.", "Primero, que el XML corta en disminuyé"
    espacios = completo.replace(" ", "  ", 1)
    casos = [  # (texto del XML por día, texto del visor por día, días de los límites, ¿solo corte?, ¿solo espacios?)
        (lambda d: completo if d < 50 else cortado if d < 120 else espacios if d < 200 else "Segundo texto.",
         lambda d: completo if d < 200 else "Segundo texto.", [0, 200], True, False),
        (lambda d: cortado, lambda d: completo if d < 100 else cortado + "ndose más tarde.", [0, 100], False, False),
        (lambda d: completo if d < 100 else espacios, lambda d: completo, [0], False, True),
    ]
    originales = bcn.descargar, bcn.descargar_visor
    try:
        for xml, visor, esperados, solo_corte, solo_espacios in casos:
            def dia(f):
                return (dt.date.fromisoformat(f) - d0).days
            bcn.descargar = lambda n, f: _xml(xml(dia(f)))
            bcn.descargar_visor = lambda n, f: {"html": [{"i": 1, "t": f"<div>{visor(dia(f))}</div>"}]}
            del bcn._solo_corte[:], bcn._solo_espacios[:]
            bcn._sin_completar.clear()
            limites = [(d - d0).days for d in bcn.cambios("1", d0, d0 + dt.timedelta(days=300), (), paso=64)]
            assert limites == esperados, f"autoprueba de la bisección: límites {limites} y no {esperados}"
            assert (bool(bcn._solo_corte), bool(bcn._solo_espacios)) == (solo_corte, solo_espacios), (
                f"autoprueba de la bisección: avisos de corte {bcn._solo_corte} y de espacios {bcn._solo_espacios}")
    finally:
        bcn.descargar, bcn.descargar_visor = originales
        del bcn._solo_corte[:], bcn._solo_espacios[:]
        bcn._sin_completar.clear()


if __name__ == "__main__":
    raiz = Path(__file__).resolve().parent.parent
    args = sys.argv[1:]
    if args[:1] == ["comparar"]:
        comparar(*args[1:3])
    elif args[:1] == ["visor"]:
        contrastar_visor(args[1] if len(args) > 1 else raiz / "normativa" / "textos")
    else:
        autoprueba()
        autoprueba_biseccion()
        imprimir(tabla(args[0] if args else raiz / "normativa" / "textos"))
