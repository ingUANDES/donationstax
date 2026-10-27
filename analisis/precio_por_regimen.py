"""Precio fiscal de donar por régimen y año comercial (P-04 del BLUEPRINT).

Lee tres tablas de normativa/:
- cambios.csv: los parámetros, cada uno con su cita.
- precio_casos.csv: qué fila de cambios.csv da c, g y tau en cada caso.
- precio_comparaciones.csv: qué par de casos (antes, después) mide el efecto de cada fila.

Calcula P = 1 - c - tau * g con precio_fiscal.precio_fiscal y escribe dos tablas:
- casos: caso, año comercial, contribuyente, régimen, c, g, tau, precio y subsidio (1 - precio).
- cambios: una línea por comparación, con el precio antes y después, la diferencia, el signo calculado y
  si coincide con efecto_precio de cambios.csv; y una línea por cada fila de cambios.csv sin comparación.
  n_casos cuenta los casos que citan la fila en cualquier columna de fuente.

Una referencia es "norma|articulo_inciso|vigencia_desde" de cambios.csv; si hay varias, van separadas por
"||". En precio_comparaciones.csv, si la clave tiene varias filas, "#<parámetro>" al final elige la que trae
ese parámetro. Reglas para c, g y tau:
  <parámetro>      valor numérico del parámetro en la fila (por ejemplo c o tau_idpc). Si la clave
                   tiene varias filas, se usa la única que trae ese parámetro.
  <parámetro>[i]   i-ésimo valor (desde 1) de una lista separada por "/" (escala_igc).
  ...%             el número se divide por 100 (las escalas del art. 52 de la LIR están en porcentaje).
                   De un valor como "40_sobre_310_uta" se toma el número inicial.
  0 o 1            constante; la fila citada es la que la justifica.
  1-c              g = 1 - c (art. 1 N° 14 de la Ley 19.885).
  na               solo para tau y solo si g = 0: tau no entra en el precio.

Falla con AssertionError si una referencia no existe, no es única o apunta a una fila que no está
«verificada»; si un caso o una comparación se repite o no existe; si un precio no está entre 0 y 1; o si
una fila verificada con efecto abarata, encarece, ambiguo o base no aparece en ningún caso ni comparación. La
autoprueba comprueba que esas tres fallas (fila no verificada, fila con efecto sin usar, precio fuera de
[0, 1]) sí detienen el cálculo.
Contraparte en R: precio_por_regimen.R (contraste.py compara las dos salidas).

Uso: python precio_por_regimen.py cambios.csv casos.csv comparaciones.csv salida_casos.csv salida_cambios.csv
"""
import csv
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from precio_fiscal import precio_fiscal  # noqa: E402

TOL = 1e-12
CON_EFECTO = {"abarata", "encarece", "ambiguo", "base"}
REGLA = re.compile(r"^([A-Za-z0-9_]+)(?:\[(\d+)\])?(%)?$")
NUMERO = re.compile(r"^-?[0-9]+(?:\.[0-9]+)?")


def leer(ruta):
    with open(ruta, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def clave(r):
    return f"{r['norma']}|{r['articulo_inciso']}|{r['vigencia_desde']}"


def parametros(texto):
    out = {}
    for parte in texto.split(";"):
        if "=" in parte:
            k, v = parte.split("=", 1)
            out[k.strip()] = v.strip()
    return out


class Normativa:
    def __init__(self, filas):
        for r in filas:
            for v in r.values():
                assert "\n" not in v and "\r" not in v, f"cambios.csv: salto de línea dentro de un campo ({clave(r)})"
        self.filas = filas
        self.usos = [set() for _ in filas]  # casos que citan cada fila

    def buscar(self, ref, caso):
        idx = [i for i, r in enumerate(self.filas) if clave(r) == ref]
        assert idx, f"{caso}: la referencia {ref!r} no está en cambios.csv"
        for i in idx:
            assert self.filas[i]["estado_verificacion"] == "verificada", f"{caso}: {ref!r} no está verificada"
        return idx

    def citar(self, ref, caso):
        for i in self.buscar(ref, caso):
            self.usos[i].add(caso)

    def valor(self, ref, regla, caso):
        idx = self.buscar(ref, caso)
        if regla in ("0", "1"):
            for i in idx:
                self.usos[i].add(caso)
            return float(regla)
        m = REGLA.match(regla)
        assert m, f"{caso}: regla {regla!r} no reconocida"
        nombre, pos, pct = m.groups()
        con = [i for i in idx if nombre in parametros(self.filas[i]["parametros"])]
        assert len(con) == 1, f"{caso}: {ref!r} tiene {len(con)} filas con el parámetro {nombre!r}"
        i = con[0]
        self.usos[i].add(caso)
        v = parametros(self.filas[i]["parametros"])[nombre]
        if pos is not None:
            v = v.split("/")[int(pos) - 1]
        n = NUMERO.match(v)
        assert n, f"{caso}: el parámetro {nombre!r} = {v!r} no es numérico"
        x = float(n.group(0))
        if pct:
            x = x / 100
        else:
            assert n.group(0) == v, f"{caso}: {nombre!r} = {v!r} trae texto; use la regla con %"
        return x


def calcular_casos(norma, casos):
    vistos, out = set(), []
    for k in casos:
        caso = k["caso"]
        assert caso not in vistos, f"caso repetido: {caso}"
        vistos.add(caso)
        c = norma.valor(k["c_fuente"], k["c"], caso)
        if k["g"] == "1-c":
            norma.citar(k["g_fuente"], caso)
            g = 1.0 - c
        else:
            g = norma.valor(k["g_fuente"], k["g"], caso)
        if k["tau"] == "na":
            assert g == 0.0, f"{caso}: tau = na exige g = 0"
            assert not k["tau_fuente"], f"{caso}: tau = na no lleva fuente"
            tau, tau_txt = 0.0, ""
        else:
            tau = norma.valor(k["tau_fuente"], k["tau"], caso)
            tau_txt = repr(tau)
        for ref in filter(None, k["otras_fuentes"].split("||")):
            norma.citar(ref, caso)
        p = precio_fiscal(c, g, tau)
        assert -TOL <= p <= 1.0 + TOL, f"{caso}: precio {p!r} fuera de [0, 1]"
        out.append({
            "caso": caso, "anio_comercial": k["anio_comercial"], "contribuyente": k["contribuyente"],
            "regimen": k["regimen"], "c": repr(c), "g": repr(g), "tau": tau_txt,
            "precio": repr(p), "subsidio": repr(1.0 - p),
        })
    return out


def efecto(delta):
    if delta < -TOL:
        return "abarata"
    if delta > TOL:
        return "encarece"
    return "neutro"


def calcular_cambios(norma, comparaciones, precios):
    por_fila = {i: [] for i in range(len(norma.filas))}
    vistas = set()
    for k in comparaciones:
        ref, _, nombre = k["fila_evaluada"].partition("#")
        idx = norma.buscar(ref, "comparación")
        if nombre:
            idx = [i for i in idx if nombre in parametros(norma.filas[i]["parametros"])]
        assert len(idx) == 1, f"comparación: {k['fila_evaluada']!r} no identifica una sola fila"
        for caso in (k["caso_antes"], k["caso_despues"]):
            assert caso in precios, f"comparación: el caso {caso!r} no existe"
        llave = (idx[0], k["caso_antes"], k["caso_despues"])
        assert llave not in vistas, f"comparación repetida: {llave}"
        vistas.add(llave)
        por_fila[idx[0]].append(k)
    out = []
    for i, r in enumerate(norma.filas):
        base = {
            "fila": str(i + 2), "norma": r["norma"], "articulo_inciso": r["articulo_inciso"],
            "vigencia_desde": r["vigencia_desde"], "efecto_declarado": r["efecto_precio"],
            "n_casos": str(len(norma.usos[i])),
        }
        if not por_fila[i]:
            assert r["estado_verificacion"] != "verificada" or r["efecto_precio"] not in CON_EFECTO \
                or norma.usos[i], (
                f"cambios.csv:{i + 2} ({clave(r)}) tiene efecto {r['efecto_precio']!r} y ningún caso la usa"
            )
            out.append({**base, "comparacion": "", "caso_antes": "", "caso_despues": "", "precio_antes": "",
                        "precio_despues": "", "delta": "", "efecto_calculado": "sin comparación",
                        "coincide": "", "nota": ""})
            continue
        for k in por_fila[i]:
            pa, pd = precios[k["caso_antes"]], precios[k["caso_despues"]]
            d = pd - pa
            e = efecto(d)
            dec = r["efecto_precio"]
            coincide = "ambiguo" if dec == "ambiguo" else ("sí" if dec == e else "no")
            out.append({**base, "comparacion": f"{i + 2}:{k['caso_antes']}>{k['caso_despues']}",
                        "caso_antes": k["caso_antes"], "caso_despues": k["caso_despues"],
                        "precio_antes": repr(pa), "precio_despues": repr(pd), "delta": repr(d),
                        "efecto_calculado": e, "coincide": coincide, "nota": k["nota"]})
    return out


COLS_CASOS = ["caso", "anio_comercial", "contribuyente", "regimen", "c", "g", "tau", "precio", "subsidio"]
COLS_CAMBIOS = ["fila", "norma", "articulo_inciso", "vigencia_desde", "efecto_declarado", "comparacion",
                "caso_antes", "caso_despues", "precio_antes", "precio_despues", "delta", "efecto_calculado",
                "coincide", "n_casos", "nota"]


def escribir(ruta, cols, filas):
    with open(ruta, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=cols, lineterminator="\n")
        w.writeheader()
        w.writerows(filas)


def autoprueba():
    fila = dict(norma="X", articulo_inciso="a", vigencia_desde="2000-01-01", efecto_precio="abarata",
                parametros="c=0.5;g=0.5;tau=0.2;tau_alta=3", estado_verificacion="verificada")
    otra = dict(fila, norma="Y", parametros="c=0.1", estado_verificacion="sin verificar")
    caso = dict(caso="k", anio_comercial="2000", contribuyente="x", regimen="x", c_fuente="X|a|2000-01-01",
                c="c", g_fuente="X|a|2000-01-01", g="g", tau_fuente="X|a|2000-01-01", tau="tau",
                anio_tau="x", otras_fuentes="", nota="")
    norma = Normativa([fila, otra])
    precios = {t["caso"]: float(t["precio"]) for t in calcular_casos(norma, [caso])}
    assert abs(precios["k"] - 0.4) <= TOL, "autoprueba: 1 - 0.5 - 0.2*0.5 debía dar 0.4"
    calcular_cambios(norma, [], precios)
    malos = [
        (lambda n: calcular_casos(n, [dict(caso, c_fuente="Y|a|2000-01-01")]), "fila no verificada"),
        (lambda n: calcular_cambios(n, [], {}), "fila con efecto sin usar"),
        (lambda n: calcular_casos(n, [dict(caso, tau="tau_alta")]), "precio fuera de [0, 1]"),
    ]
    for f, que in malos:
        try:
            f(Normativa([fila, otra]))
        except AssertionError:
            continue
        raise AssertionError(f"autoprueba: no se detectó {que}")


def main(cambios, casos, comparaciones, salida_casos, salida_cambios):
    norma = Normativa(leer(cambios))
    tabla = calcular_casos(norma, leer(casos))
    precios = {t["caso"]: float(t["precio"]) for t in tabla}
    cambios_out = calcular_cambios(norma, leer(comparaciones), precios)
    escribir(salida_casos, COLS_CASOS, tabla)
    escribir(salida_cambios, COLS_CAMBIOS, cambios_out)
    n_comp = sum(1 for r in cambios_out if r["comparacion"])
    n_no = sum(1 for r in cambios_out if r["coincide"] == "no")
    print(f"precio por régimen: {len(tabla)} casos, {n_comp} comparaciones, "
          f"{len(norma.filas)} filas de cambios.csv, {n_no} comparaciones con signo distinto del declarado")


if __name__ == "__main__":
    autoprueba()
    main(*sys.argv[1:6])
