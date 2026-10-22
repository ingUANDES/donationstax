"""Comprueba que cada fila 'verificada' de normativa/cambios.csv cita la norma textualmente.

Para cada fila con estado_verificacion == 'verificada' exige fuente_url,
fecha_fuente y version_texto, y que cita_textual aparezca (tras normalizar
espacios) en normativa/textos/<texto_archivo>. Las demás filas se cuentan,
no se validan. Falla con AssertionError. La autoprueba comprueba que una cita
inexistente sí hace fallar la comprobación.

Uso: python verifica_citas.py [cambios.csv] [carpeta_textos]
"""
import csv
import re
import sys
import tempfile
from pathlib import Path

COLUMNAS = [
    "norma", "tipo_norma", "articulo_inciso", "vigencia_desde", "vigencia_hasta",
    "efecto_precio", "parametros", "cita_textual", "texto_archivo", "fuente_url",
    "fecha_fuente", "version_texto", "estado_verificacion",
]
OBLIGATORIAS = ["fuente_url", "fecha_fuente", "version_texto", "texto_archivo", "cita_textual"]


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


if __name__ == "__main__":
    raiz = Path(__file__).resolve().parent.parent
    ruta = sys.argv[1] if len(sys.argv) > 1 else raiz / "normativa" / "cambios.csv"
    textos = sys.argv[2] if len(sys.argv) > 2 else raiz / "normativa" / "textos"
    autoprueba()
    total, ok = verificar(ruta, textos)
    print(f"citas OK: {total} filas, {ok} verificadas con cita textual comprobada")
