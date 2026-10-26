"""Regla 7 de D-10: completa el RUT del donante por nombre en las planillas armonizadas de la Ley 19.885.

Lee las filas de armonizar.py (<carpeta_armonizado>/<año>.csv) y escribe:
- <salida_filas>/<año>.csv: por fila, el nombre normalizado, de dónde sale el RUT, el RUT final, la
  similitud y el identificador del donante (CAMPOS_FILA). Trae nombres: va en data_local/.
- <calib_identico.csv>: precisión del nombre idéntico al ocultar el RUT de un año a la vez.
- <calib_similitud.csv>: precisión y cobertura de la similitud por umbral, con el umbral elegido.
- <por_anio.csv>: cuántas filas quedan con RUT observado, completado o sin RUT, por año.
La contraparte en R es completar_rut.R; contraste_datos.py y contraste_armonizado.py comparan.

Reglas (D-10, regla 7; lo marcado «[i]» es inferencia del investigador, reunida en D-13):
- Nombre normalizado: se quitan tildes y diéresis y la Ñ y la Ç pasan a N y C (tabla DE/A), las
  letras a-z a mayúscula, U+FFFD a «?» (regla 6), todo lo que no sea A-Z, 0-9 o «?» a espacio, y se
  colapsan y recortan los espacios [i].
- RUT usable: rut_estado dv_valido o sin_dv (2022) [i: el número sin DV se usa]. Los dv_invalido
  no se usan (D-10). Diccionario: nombre normalizado -> conjunto de cuerpos, con todas las filas con
  RUT usable y nombre no vacío.
- Paso 1, nombre idéntico: dos nombres son compatibles si son iguales o, cuando alguno tiene «?»,
  si tienen el mismo largo y coinciden en las posiciones sin «?». Si los nombres compatibles del
  diccionario tienen un solo cuerpo, se asigna («nombre_identico»); si tienen varios, «ambiguo»
  [i]; si ninguno, pasa al paso 2.
- Consultas del paso 2: nombres sin candidato en el paso 1 y sin «?» (los nombres con «?» solo pasan
  por el paso 1) [i].
- Paso 2, similitud [i: métrica y bloqueo]: s = 1 - Levenshtein / largo del más largo, entre
  nombres sin «?» que comparten los 3 primeros caracteres. Se toma el mejor s del diccionario (todos
  los empates); si sus nombres tienen un solo cuerpo y s >= umbral, se asigna («similitud»); si
  tienen varios, «ambiguo»; si no hay candidato con s >= UMBRALES[0], «sin_candidato».
- Calibración, ocultando RUT conocidos: para cada año con RUT usable, el diccionario se arma con los
  demás años y se consultan los nombres de ese año con RUT usable; es correcta la asignación cuyo
  cuerpo está entre los observados para ese nombre en ese año. Se cuenta por nombre distinto [i].
  Umbral elegido: el menor de UMBRALES (en centésimos, aritmética entera) con al menos MIN_ASIGNADOS
  asignaciones agrupando los años y precisión >= 99 % (D-10) [i: mínimo de asignaciones]. Si
  ninguno cumple, el paso 2 no asigna nada.
- Lo que sigue sin RUT se identifica por su nombre normalizado y se marca como error de medición
  (D-10); sin nombre normalizado, el identificador queda vacío.

Uso: python completar_rut.py <carpeta_armonizado> <salida_filas> <calib_identico.csv> <calib_similitud.csv> <por_anio.csv>
"""
import csv
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
from rapidfuzz import process
from rapidfuzz.distance import Levenshtein

csv.field_size_limit(sys.maxsize)
DE = "ÁÀÂÄÃÅáàâäãåÉÈÊËéèêëÍÌÎÏíìîïÓÒÔÖÕóòôöõÚÙÛÜúùûüÑñÇçÝýÿ"
A = "AAAAAAAAAAAAEEEEEEEEIIIIIIIIOOOOOOOOOOUUUUUUUUNNCCYYY"
TABLA = str.maketrans(DE + "abcdefghijklmnopqrstuvwxyz\ufffd", A + "ABCDEFGHIJKLMNOPQRSTUVWXYZ?")
NO_NOMBRE = re.compile(r"[^A-Z0-9?]+")
USABLE = ("dv_valido", "sin_dv")
UMBRALES = list(range(80, 100))  # centésimos
MIN_ASIGNADOS = 100
CAMPOS_FILA = ["anio_archivo", "fila", "donante_norm", "rut_fuente", "rut_final", "similitud", "id_donante",
               "error_medicion"]
CAMPOS_IDENT = ["anio", "n_consultas", "n_asignados", "n_correctos", "n_ambiguos", "n_sin_candidato", "precision"]
CAMPOS_SIM = ["umbral", "n_consultas", "n_con_candidato", "n_asignados", "n_correctos", "precision", "elegido"]
CAMPOS_ANIO = ["anio", "n_filas", "n_observado", "n_nombre_identico", "n_similitud", "n_ambiguo", "n_sin_candidato",
               "n_sin_nombre", "n_error_medicion", "n_donantes_rut", "n_donantes_nombre"]


def normalizar(s):
    return NO_NOMBRE.sub(" ", s.translate(TABLA)).strip()


def precision(c, a):
    return f"{c / a:.6f}" if a else ""


class Diccionario:
    """Nombre normalizado -> cuerpos, con búsqueda por comodín y por bloque de 3 caracteres."""

    def __init__(self, pares):
        self.m = defaultdict(set)
        for n, c in pares:
            self.m[n].add(c)
        self.comodin = defaultdict(list)  # largo -> nombres con «?»
        self.por_largo = defaultdict(list)  # largo -> todos los nombres (para consultas con «?»)
        self.bloques = defaultdict(list)
        for n in sorted(self.m):
            self.por_largo[len(n)].append(n)
            if "?" in n:
                self.comodin[len(n)].append(n)
            else:
                self.bloques[n[:3]].append(n)

    @staticmethod
    def compatibles(a, b):
        return all(x == y or x == "?" or y == "?" for x, y in zip(a, b))

    def identico(self, q):
        """Cuerpos de los nombres compatibles con q."""
        if "?" in q:
            nombres = [n for n in self.por_largo.get(len(q), ()) if self.compatibles(q, n)]
        else:
            nombres = ([q] if q in self.m else []) + [n for n in self.comodin.get(len(q), ()) if self.compatibles(q, n)]
        return set().union(*(self.m[n] for n in nombres)) if nombres else set()

    def similares(self, consultas):
        """consulta -> (num, den, cuerpos) del mejor s = num/den >= UMBRALES[0]/100, o sin entrada."""
        res = {}
        por_bloque = defaultdict(list)
        for q in consultas:
            por_bloque[q[:3]].append(q)
        for k, qs in por_bloque.items():
            ch = self.bloques.get(k)
            if not ch:
                continue
            lc = np.array([len(c) for c in ch], dtype=np.int64)
            d = process.cdist(qs, ch, scorer=Levenshtein.distance, dtype=np.int64, workers=-1)
            for i, q in enumerate(qs):
                den = np.maximum(lc, len(q))
                num = den - d[i]
                ok = np.flatnonzero(100 * num >= UMBRALES[0] * den)
                if ok.size == 0:
                    continue
                # mejor fracción num/den, empates exactos por producto cruzado
                b = ok[np.argmax(num[ok] / den[ok])]
                emp = ok[num[ok] * den[b] == num[b] * den[ok]]
                res[q] = (int(num[b]), int(den[b]), set().union(*(self.m[ch[j]] for j in emp)))
        return res


def leer(carpeta):
    filas = {}
    for p in sorted(Path(carpeta).glob("[0-9][0-9][0-9][0-9].csv")):
        nombres = {}
        with open(p, newline="", encoding="utf-8") as f:
            filas[int(p.stem)] = [
                (r["fila"], nombres.setdefault(r["donante"], normalizar(r["donante"])),
                 r["rut_estado"] in USABLE, r["rut_cuerpo"])
                for r in csv.DictReader(f)]
    return filas


def calibrar(filas):
    pares = {a: {(n, c) for _, n, u, c in fs if u and n} for a, fs in filas.items()}
    anios = [a for a, ps in pares.items() if ps]
    ident, consultas_sim, n_sim = [], [], 0
    for t in anios:
        dic = Diccionario(p for a in anios if a != t for p in pares[a])
        ver = defaultdict(set)
        for n, c in pares[t]:
            ver[n].add(c)
        cnt, sin = Counter(), []
        for n in sorted(ver):
            cs = dic.identico(n)
            if len(cs) == 1:
                cnt["a"] += 1
                cnt["c"] += next(iter(cs)) in ver[n]
            elif cs:
                cnt["amb"] += 1
            else:
                cnt["sin"] += 1
                if "?" not in n:
                    sin.append(n)
        ident.append({"anio": t, "n_consultas": len(ver), "n_asignados": cnt["a"], "n_correctos": cnt["c"],
                      "n_ambiguos": cnt["amb"], "n_sin_candidato": cnt["sin"], "precision": precision(cnt["c"], cnt["a"])})
        n_sim += len(sin)
        for q, (num, den, cs) in dic.similares(sin).items():
            consultas_sim.append((num, den, cs, ver[q]))
        print("calibrado", t, flush=True)
    return ident, consultas_sim, n_sim


def tabla_similitud(consultas_sim, n_consultas):
    sim, elegido = [], None
    con_cand = len(consultas_sim)
    for k in UMBRALES:
        asig = [(cs, ver) for num, den, cs, ver in consultas_sim if 100 * num >= k * den and len(cs) == 1]
        a, c = len(asig), sum(1 for cs, ver in asig if next(iter(cs)) in ver)
        cumple = a >= MIN_ASIGNADOS and 100 * c >= 99 * a
        if cumple and elegido is None:
            elegido = k
        sim.append({"umbral": f"0.{k:02d}", "n_consultas": n_consultas, "n_con_candidato": con_cand, "n_asignados": a,
                    "n_correctos": c, "precision": precision(c, a), "elegido": str(k == elegido).lower()})
    return sim, elegido


def completar(filas, elegido):
    dic = Diccionario({(n, c) for fs in filas.values() for _, n, u, c in fs if u and n})
    faltan = sorted({n for fs in filas.values() for _, n, u, _ in fs if not u and n})
    asignacion, para_sim = {}, []
    for n in faltan:
        cs = dic.identico(n)
        if len(cs) == 1:
            asignacion[n] = ("nombre_identico", next(iter(cs)), "")
        elif cs:
            asignacion[n] = ("ambiguo", "", "")
        else:
            asignacion[n] = ("sin_candidato", "", "")
            if elegido is not None and "?" not in n:
                para_sim.append(n)
    for q, (num, den, cs) in dic.similares(para_sim).items():
        if 100 * num >= elegido * den:
            asignacion[q] = ("similitud", next(iter(cs)), f"{num}/{den}") if len(cs) == 1 else ("ambiguo", "", f"{num}/{den}")
    return asignacion


def main(carpeta, salida_filas, calib_identico, calib_similitud, por_anio):
    filas = leer(carpeta)
    ident, consultas_sim, n_sim = calibrar(filas)
    sim, elegido = tabla_similitud(consultas_sim, n_sim)
    asignacion = completar(filas, elegido)
    salida_filas = Path(salida_filas)
    salida_filas.mkdir(parents=True, exist_ok=True)
    resumen = []
    for a, fs in filas.items():
        cnt, ids_rut, ids_nom = Counter(), set(), set()
        with open(salida_filas / f"{a}.csv", "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f)
            w.writerow(CAMPOS_FILA)
            for fila, n, u, c in fs:
                if u:
                    fuente, rut, s = "observado", c, ""
                elif not n:
                    fuente, rut, s = "sin_nombre", "", ""
                else:
                    fuente, rut, s = asignacion[n]
                ident_d = f"rut:{rut}" if rut else (f"nombre:{n}" if n else "")
                if rut:
                    ids_rut.add(ident_d)
                elif n:
                    ids_nom.add(ident_d)
                cnt[fuente] += 1
                cnt["error"] += rut == ""
                w.writerow([a, fila, n, fuente, rut, s, ident_d, int(rut == "")])
        resumen.append({"anio": a, "n_filas": len(fs), "n_observado": cnt["observado"],
                        "n_nombre_identico": cnt["nombre_identico"], "n_similitud": cnt["similitud"],
                        "n_ambiguo": cnt["ambiguo"], "n_sin_candidato": cnt["sin_candidato"],
                        "n_sin_nombre": cnt["sin_nombre"], "n_error_medicion": cnt["error"],
                        "n_donantes_rut": len(ids_rut), "n_donantes_nombre": len(ids_nom)})
        print("completado", a, flush=True)
    for ruta, campos, tabla in ((calib_identico, CAMPOS_IDENT, ident), (calib_similitud, CAMPOS_SIM, sim),
                                (por_anio, CAMPOS_ANIO, resumen)):
        with open(ruta, "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=campos)
            w.writeheader()
            w.writerows(tabla)


if __name__ == "__main__":
    main(*sys.argv[1:6])
