Donations’ tax in Chile
================

## Estado

Fase: inventario normativo y perfil de los datos hechos; la estimación no ha empezado. Actualizado el 2026-10-23.

| Etapa | Estado | Detalle |
|---|---|---|
| E0 Andamiaje | listo | [BLUEPRINT.md](BLUEPRINT.md), [DECISIONES.md](DECISIONES.md), `make verificar` |
| E1 Inventario normativo | hecho; faltan las tasas transitorias de 2011 y 2012 | [P-01 · Inventario normativo](BLUEPRINT.md#p-01), [normativa/cambios.csv](normativa/cambios.csv) |
| E2 Estructura de los datos | perfil hecho; faltan armonizar las planillas y perfilar los datos de la Ley 21.440 | [P-02 · Estructura de los datos](BLUEPRINT.md#p-02), [P-06](BLUEPRINT.md#p-06), [P-07](BLUEPRINT.md#p-07), [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv) |
| E3 a E5 | sin iniciar | [Etapas](BLUEPRINT.md#etapas) |

Todas las decisiones, de D-01 a D-10, están aprobadas; la última tanda fue el 2026-10-23 ([D-02](DECISIONES.md#d-02), [D-07](DECISIONES.md#d-07), [D-08](DECISIONES.md#d-08) y [D-10](DECISIONES.md#d-10)). El canal de trabajo son los issues con las etiquetas del protocolo ([D-06 · Canal de trabajo](DECISIONES.md#d-06)); falta confirmar el nombre de las etiquetas de prioridad.

Siguiente paso: los issues [#9 (P-06)](https://github.com/ingUANDES/donationstax/issues/9) y [#10 (P-07)](https://github.com/ingUANDES/donationstax/issues/10) esperan que una persona les ponga `dueño:claude-science`; no dependen de nada pendiente (P-07 necesita autorizados `donacionesley21440.gob.cl` y `cdn.hacienda.cl`). [P-04](BLUEPRINT.md#p-04) y [P-05](BLUEPRINT.md#p-05) pueden empezar; ninguna estimación sobre datos reales antes de que P-05 esté «verificada» ([D-05](DECISIONES.md#d-05)).

## Cómo verificar

`make verificar` (requiere Python 3 y R). Qué comprueba y qué no: [Verificación](BLUEPRINT.md#verificación). `make verificar-datos` perfila las planillas del Ministerio en Python y en R (tarda varios minutos; pide las planillas en `data_local/` y `analisis/requirements.txt`).

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
  - 2026-10-22: P-01, inventario normativo: [normativa/cambios.csv](normativa/cambios.csv)
    (55 filas, 54 con cita literal comprobada), textos en
    [normativa/textos/](normativa/textos/) y [bcn_versiones.py](analisis/bcn_versiones.py)
    ([PR 4](https://github.com/ingUANDES/donationstax/pull/4)).
  - 2026-10-22: P-02, perfil de esquema de las planillas 2008-2025:
    [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv), contraste Python y R
    ([contraste_datos.py](analisis/contraste_datos.py)) y `make verificar-datos`
    ([PR 5](https://github.com/ingUANDES/donationstax/pull/5)).
  - 2026-10-23: se aprueban D-05 y D-09, se plantea D-10 (criterio para completar o excluir), se
    documentan los datos de la Ley 21.440 ([P-07](BLUEPRINT.md#p-07),
    [instantánea de la fuente](datos/fuentes/ley21440_donaciones_2026-10-23.txt)) y se actualiza
    [D-06](DECISIONES.md#d-06).
  - 2026-10-23: se aprueban D-02 (con sus límites), D-07, D-08 y D-10; todas las decisiones
    quedan aprobadas ([DECISIONES.md](DECISIONES.md)).
