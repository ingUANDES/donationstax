Donations’ tax in Chile
================

## Estado

Fase: inventario normativo, perfiles de los datos de las Leyes 19.885 y 21.440 y armonización de las planillas, con el RUT completado por nombre, hechos; la estimación no ha empezado. Actualizado el 2026-10-26.

| Etapa | Estado | Detalle |
|---|---|---|
| E0 Andamiaje | listo | [BLUEPRINT.md](BLUEPRINT.md), [DECISIONES.md](DECISIONES.md), `make verificar` |
| E1 Inventario normativo | hecho; faltan las tasas transitorias de 2011 y 2012 y la Ley 21.850 ([issue 12](https://github.com/ingUANDES/donationstax/issues/12)) | [P-01 · Inventario normativo](BLUEPRINT.md#p-01), [normativa/cambios.csv](normativa/cambios.csv) |
| E2 Estructura de los datos | perfiles de la Ley 19.885 y de la Ley 21.440, cruce por donataria, armonización y RUT completado por nombre (las ocho reglas de [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10)) hechos | [P-02 · Estructura de los datos](BLUEPRINT.md#p-02), [P-06 · Armonizar las planillas](BLUEPRINT.md#p-06), [P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07), [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv), [datos/esquema_ley21440_por_anio.csv](datos/esquema_ley21440_por_anio.csv), [datos/cruce_donatarias_por_anio.csv](datos/cruce_donatarias_por_anio.csv), [datos/armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv), [datos/rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv) |
| E3 a E5 | sin iniciar | [Etapas](BLUEPRINT.md#etapas) |

De las catorce decisiones de [DECISIONES.md](DECISIONES.md), trece están aprobadas; la última tanda fue el 2026-10-26 ([D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13), con un cambio en su punto 1). [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14) está propuesta y espera aprobación. El canal de trabajo son los issues con las etiquetas del protocolo ([D-06 · Canal de trabajo](DECISIONES.md#d-06)), que ya existen; ningún issue lleva todavía una etiqueta de prioridad.

Siguiente paso: los issues [9](https://github.com/ingUANDES/donationstax/issues/9) y [10](https://github.com/ingUANDES/donationstax/issues/10) están cerrados ([PR 15](https://github.com/ingUANDES/donationstax/pull/15), [PR 18](https://github.com/ingUANDES/donationstax/pull/18) y [PR 19](https://github.com/ingUANDES/donationstax/pull/19)). Esperan que una persona les ponga `dueño:claude-science`: el [issue 21](https://github.com/ingUANDES/donationstax/issues/21) ([P-04 · Precio fiscal por régimen](BLUEPRINT.md#p-04)), el [issue 22](https://github.com/ingUANDES/donationstax/issues/22) ([P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#p-05)), el [issue 20](https://github.com/ingUANDES/donationstax/issues/20) (ajustar el año de la donación a [D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13) y reproducir su evaluación por script; la parte de [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14) depende de su aprobación), el [issue 12](https://github.com/ingUANDES/donationstax/issues/12) (Ley 21.850; puede necesitar `www.diariooficial.interior.gob.cl`) y el [issue 16](https://github.com/ingUANDES/donationstax/issues/16) (duplicados de la Ley 21.440). Ninguna estimación sobre datos reales antes de que las simulaciones de [P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#p-05) estén «verificadas» ([D-05 · Niveles de resultado](DECISIONES.md#d-05)).

## Cómo verificar

`make verificar` (requiere Python 3 y R). Qué comprueba y qué no: [Verificación](BLUEPRINT.md#verificación). `make verificar-datos` corre cinco bloques, en orden, cada uno con su objetivo: `verificar-datos-19885` (planillas del Ministerio), `verificar-datos-21440` (nómina de la Ley 21.440), `verificar-cruce` (cruce por RUT de donataria), `verificar-armonizado` (armonización de las planillas; escribe unos 4,3 GB en `data_local/armonizado/`) y `verificar-rut` (completa el RUT por nombre sobre esas filas; necesita el paquete stringdist de R). Pide los archivos en `data_local/` (cómo bajarlos: comentarios del [Makefile](Makefile)) y `pip install -r analisis/requirements.txt`; si `python3` no tiene `python-calamine`, `make verificar-datos PY=<ruta al python del entorno>`.

Entorno con que se corrió ([D-11 · Criterios de reproducibilidad](DECISIONES.md#d-11)): Python 3.13.16, python-calamine 0.9.0 (fijado en [requirements.txt](analisis/requirements.txt)) y R 4.3.3 con readxl 1.4.3, en un contenedor de Claude Code en la nube con 4 CPU, 15 GB de RAM y Linux 6.18. `make verificar` tarda 0,6 s. `make verificar-datos` tardó 7 min 54 s con el solo bloque de la Ley 19.885 (2026-10-23), 12 min 40 s con los tres primeros bloques (2026-10-24) y 36 min 57 s con los cinco (2026-10-24, sobre la pila de los PR 18 y 19, con numpy 2.5.4, rapidfuzz 3.14.6 y stringdist 0.9.12 de apt, no la 0.9.17: el resultado es idéntico, `cmp` pasa); todos con código 0. Una sola medición cada una, con las planillas ya descargadas.

Otro equipo (corrida del PR 15 para el [issue 10](https://github.com/ingUANDES/donationstax/issues/10), 2026-10-23): Python 3.13.17, python-calamine 0.9.0, y R 4.3.3 con readxl 1.4.3 de conda-forge, Linux 6.8.0, 16 GB de RAM y 8 CPU visibles. `make verificar` tardó 0,5 s y `make verificar-datos` 5 min 20 s, ambos con código 0 (una medición, con los archivos ya descargados). En el mismo equipo, el 2026-10-24 y ya con `verificar-armonizado` ([issue 9](https://github.com/ingUANDES/donationstax/issues/9)): `make verificar` 0,4 s y `make verificar-datos` 12 min 25 s, ambos con código 0; el proceso más grande llegó a 8,6 GB. Con `verificar-rut` (numpy 2.5.4, rapidfuzz 3.14.6 y stringdist 0.9.17): `make verificar` 0,25 s y `make verificar-datos` 18 min 55 s, con código 0 y 8,5 GB como máximo. No se probaron otras versiones.

Memoria: `readxl` carga la hoja entera aunque se le pidan pocas filas. Con las planillas de 2022, 2023 y 2025 de la Ley 19.885 (hojas de 513 a 593 MB de XML), el proceso de R más grande llegó a 8,5 GB; con unos 6 GB libres, R murió por falta de memoria (señal 9) en esos tres años. Inferencia: hacen falta unos 9 GB libres. En el R 4.3.3 de conda-forge, `R_ZIPCMD` viene vacía, y [esquema_datos.R](analisis/esquema_datos.R) usa `zip` en ese caso.

## Structure of the repo

This repo contains the code of the article entitled *More flexibility
reduces social donations*, the main files are:

  - [article-elsevier.Rmd](article-elsevier/article-elsevier.Rmd):
    generates the manuscript
      - [ETL.R](code/ETL.R) prepares the data used in the article
      - [mybibfile.bib](article-elsevier/mybibfile.bib): contains the
        database of references
  - [presentation.Rmd](presentation/presentation.Rmd): reproduces a
    presentation of the article

Research workflow (added 2026-10-22):

  - [BLUEPRINT.md](BLUEPRINT.md): method, stages and backlog
  - [DECISIONES.md](DECISIONES.md): decision log
  - [normativa/](normativa/cambios.csv): regulatory changes with verbatim citations
  - [analisis/](analisis/): Python and R scripts and test fixtures
  - `data_local/`: raw data, ignored by git, never committed

## Changelog

  - 2026-10-22: andamiaje de investigación (README con Estado,
    [BLUEPRINT.md](BLUEPRINT.md), [DECISIONES.md](DECISIONES.md),
    [Makefile](Makefile), [analisis/](analisis/),
    [normativa/cambios.csv](normativa/cambios.csv) con el esquema y sin filas).
  - 2026-10-22: [D-06 · Canal de trabajo](DECISIONES.md#d-06) actualizado:
    los issues se habilitaron ([issue 3](https://github.com/ingUANDES/donationstax/issues/3)).
  - 2026-10-22: [P-01 · Inventario normativo](BLUEPRINT.md#p-01): [normativa/cambios.csv](normativa/cambios.csv)
    (55 filas, 54 con cita literal comprobada), textos en
    [normativa/textos/](normativa/textos/) y [bcn_versiones.py](analisis/bcn_versiones.py)
    ([PR 4](https://github.com/ingUANDES/donationstax/pull/4)).
  - 2026-10-22: [P-02 · Estructura de los datos](BLUEPRINT.md#p-02), perfil de esquema de las planillas 2008-2025:
    [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv), contraste Python y R
    ([contraste_datos.py](analisis/contraste_datos.py)) y `make verificar-datos`
    ([PR 5](https://github.com/ingUANDES/donationstax/pull/5)).
  - 2026-10-23: se aprueban [D-05 · Niveles de resultado](DECISIONES.md#d-05) y [D-09 · Fuente y completitud de los datos](DECISIONES.md#d-09), se plantea [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10), se documentan los datos de la Ley 21.440 ([P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07),
    [instantánea de la fuente](datos/fuentes/ley21440_donaciones_2026-10-23.txt)) y se actualiza
    [D-06 · Canal de trabajo](DECISIONES.md#d-06).
  - 2026-10-23: se aprueban [D-02 · Alcance](DECISIONES.md#d-02) (con sus límites), [D-07 · Sentido del cambio de la Ley 20.316](DECISIONES.md#d-07), [D-08 · Ventana empírica](DECISIONES.md#d-08) y [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10); todas las decisiones quedan aprobadas ([DECISIONES.md](DECISIONES.md)).
  - 2026-10-23: se abre el [issue 12](https://github.com/ingUANDES/donationstax/issues/12) para
    inventariar la Ley 21.850, se agrega su fila en
    [BLUEPRINT.md](BLUEPRINT.md#evaluabilidad-de-cada-cambio), se propone
    [D-11 · Criterios de reproducibilidad](DECISIONES.md#d-11) y se actualiza
    [D-06 · Canal de trabajo](DECISIONES.md#d-06) con las etiquetas que faltan.
  - 2026-10-23: se aprueban [D-11 · Criterios de reproducibilidad](DECISIONES.md#d-11) y [D-12 · Variable de resultado](DECISIONES.md#d-12) (monto donado); se fija
    `python-calamine` en [requirements.txt](analisis/requirements.txt) y se registran el entorno
    y los tiempos en [Cómo verificar](#cómo-verificar).
  - 2026-10-23: [P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07): descarga con manifiesto
    ([descargar_ley21440.py](analisis/descargar_ley21440.py),
    [descargas_ley21440.csv](datos/descargas_ley21440.csv)), perfil de esquema
    ([esquema_ley21440_por_anio.csv](datos/esquema_ley21440_por_anio.csv)) y cruce por RUT con la
    Ley 19.885 ([cruce_donatarias_por_anio.csv](datos/cruce_donatarias_por_anio.csv)), contrastados
    en Python y R; `make verificar-datos` en tres bloques ([Makefile](Makefile)) y resultados en esa sección ([issue 10](https://github.com/ingUANDES/donationstax/issues/10)).
  - 2026-10-24: se fusiona el [PR 15](https://github.com/ingUANDES/donationstax/pull/15); se actualizan [D-06 · Canal de trabajo](DECISIONES.md#d-06) y este README con las etiquetas que ya existen, se registra la corrida completa de `make verificar-datos`, se nombran los códigos sueltos y se abre el [issue 16](https://github.com/ingUANDES/donationstax/issues/16) sobre los duplicados de la Ley 21.440.
  - 2026-10-24: [P-06 · Armonizar las planillas](BLUEPRINT.md#p-06), reglas 1 a 6 y 8 de [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10):
    [armonizar.py](analisis/armonizar.py) y [armonizar.R](analisis/armonizar.R), contraste fila a fila
    ([contraste_armonizado.py](analisis/contraste_armonizado.py)), tablas
    [armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv),
    [armonizacion_anios_donacion.csv](datos/armonizacion_anios_donacion.csv) y
    [diccionario_columnas.csv](datos/diccionario_columnas.csv); dos reglas de lectura nuevas en
    [esquema_datos.py](analisis/esquema_datos.py) y se propone [D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13)
    ([issue 9](https://github.com/ingUANDES/donationstax/issues/9)).
  - 2026-10-24: regla 7 de [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10) en [P-06 · Armonizar las planillas](BLUEPRINT.md#p-06): completar el RUT por
    nombre ([completar_rut.py](analisis/completar_rut.py), [completar_rut.R](analisis/completar_rut.R)),
    calibración ([rut_calibracion_identico.csv](datos/rut_calibracion_identico.csv),
    [rut_calibracion_similitud.csv](datos/rut_calibracion_similitud.csv)) y resultado
    ([rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv)); se amplía
    [D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13) ([issue 9](https://github.com/ingUANDES/donationstax/issues/9)).
  - 2026-10-26: se aprueba [D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13) (con un cambio en el punto 1: el año siempre se puede reconstruir desde el archivo), se propone [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14), se abren los [issue 20](https://github.com/ingUANDES/donationstax/issues/20), [21](https://github.com/ingUANDES/donationstax/issues/21) y [22](https://github.com/ingUANDES/donationstax/issues/22), se registra la corrida completa de `make verificar-datos` y se nombran los códigos sueltos.
