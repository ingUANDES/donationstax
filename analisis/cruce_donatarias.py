"""Cruce por RUT de las donatarias de la Ley 21.440 con las instituciones de las planillas de la Ley 19.885.

Mide si los dos registros se pueden enlazar por donataria, no la sustitución entre regímenes: una
fila por año presente en las dos carpetas y una fila final con la unión de esos años. Para cada una,
cuántos RUT válidos hay en cada registro y en ambos, y qué parte del monto de cada registro va a los
RUT que están en ambos. La contraparte en R es cruce_donatarias.R; contraste_datos.py exige que
coincidan.

Reglas:
- Filas de datos: las de esquema_ley21440.py (RUT no vacío y Valor numérico) y las de
  esquema_datos.py (AÑO entero y MONTO TOTAL DONACIÓN numérico), con sus mismas reglas de lectura.
- Año: el del archivo, no el de la fecha de cada donación.
- Llave: el RUT sin puntos, espacios, tabuladores ni saltos de línea, y con la K en mayúscula. Es
  válida si queda como 7 u 8 dígitos, guion y dígito o K; las demás no entran en los conjuntos y se
  cuentan. El dígito verificador no se comprueba aquí.
- Monto: Valor en la Ley 21.440; MONTO TOTAL DONACIÓN en la Ley 19.885 (pesos nominales).

Uso: python cruce_donatarias.py <carpeta_19885> <carpeta_21440> <salida.csv>
"""
import csv
import math
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import esquema_datos as e19  # noqa: E402
import esquema_ley21440 as e21  # noqa: E402

CLAVE = re.compile(r"^\d{7,8}-[0-9K]$")
CAMPOS = [
    "anio", "n_rut_21440", "n_rut_19885", "n_rut_comun", "n_filas_21440_rut_no_valido",
    "n_filas_19885_rut_no_valido", "suma_valor_21440", "suma_valor_21440_rut_comun", "suma_monto_19885",
    "suma_monto_19885_rut_comun",
]


def clave(rut):
    return re.sub(r"[. \t\n\r]", "", rut).upper()


def montos_21440(ruta):
    _, fila_enc, datos, _, idx = e21.leer(ruta)
    filas = [f for f in datos[fila_enc + 1:] if any(f) and e21.es_fila_datos(f, idx)]
    return [(clave(f[idx["rut"]]), e19.num(f[idx["valor"]])) for f in filas]


def montos_19885(ruta):
    _, fila_enc, datos, enc = e19.tabla(ruta)
    idx = e19.roles(enc)
    assert idx["rut_institucion"] is not None, f"{ruta}: no tiene la columna RUT INSTITUCIÓN"
    filas = [f for f in datos[fila_enc + 1:] if any(f) and e19.clase_fila(f, idx) == "datos"]
    return [(clave(f[idx["rut_institucion"]]), e19.num(f[idx["mtotal"]])) for f in filas]


def fila(anio, m21, m19):
    r21 = {k for k, _ in m21 if CLAVE.match(k)}
    r19 = {k for k, _ in m19 if CLAVE.match(k)}
    comun = r21 & r19
    return {
        "anio": anio, "n_rut_21440": len(r21), "n_rut_19885": len(r19), "n_rut_comun": len(comun),
        "n_filas_21440_rut_no_valido": sum(1 for k, _ in m21 if not CLAVE.match(k)),
        "n_filas_19885_rut_no_valido": sum(1 for k, _ in m19 if not CLAVE.match(k)),
        "suma_valor_21440": e21.g17(math.fsum(v for _, v in m21)),
        "suma_valor_21440_rut_comun": e21.g17(math.fsum(v for k, v in m21 if k in comun)),
        "suma_monto_19885": e21.g17(math.fsum(v for _, v in m19)),
        "suma_monto_19885_rut_comun": e21.g17(math.fsum(v for k, v in m19 if k in comun)),
    }


def main(c19, c21, salida):
    c19, c21 = Path(c19), Path(c21)
    anios = sorted({int(p.stem) for p in c21.glob("[0-9][0-9][0-9][0-9].xlsx")}
                   & {int(p.stem) for p in c19.glob("[0-9][0-9][0-9][0-9].xlsx")})
    assert anios, "no hay años presentes en las dos carpetas"
    todos21, todos19 = [], []
    with open(salida, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=CAMPOS)
        w.writeheader()
        for a in anios:
            m21, m19 = montos_21440(c21 / f"{a}.xlsx"), montos_19885(c19 / f"{a}.xlsx")
            w.writerow(fila(a, m21, m19))
            todos21 += m21
            todos19 += m19
            print("cruce", a, flush=True)
        w.writerow(fila(f"{anios[0]}-{anios[-1]}", todos21, todos19))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3])
