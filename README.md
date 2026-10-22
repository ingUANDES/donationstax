Donations’ tax in Chile
================

## Estado

Fase: andamiaje listo; la investigación empírica no ha empezado. Actualizado el 2026-10-22.

| Etapa | Estado | Detalle |
|---|---|---|
| E0 Andamiaje | listo | [BLUEPRINT.md](BLUEPRINT.md), [DECISIONES.md](DECISIONES.md), `make verificar` |
| E1 Inventario normativo | sin iniciar | [P-01 · Inventario normativo](BLUEPRINT.md#p-01) |
| E2 Estructura de los datos | sin iniciar | [P-02 · Estructura de los datos](BLUEPRINT.md#p-02) |
| E3 a E5 | sin iniciar | [Etapas](BLUEPRINT.md#etapas) |

Bloqueos y decisiones abiertas: el canal de issues está deshabilitado ([D-06 · Canal de trabajo](DECISIONES.md#d-06)); falta resolver el sentido del cambio de la Ley 20.316 ([D-07 · Sentido del cambio de la Ley 20.316](DECISIONES.md#d-07)) y fijar los límites del alcance ([D-02 · Alcance](DECISIONES.md#d-02)).

Siguiente paso: [P-01](BLUEPRINT.md#p-01) y [P-02](BLUEPRINT.md#p-02) son independientes entre sí y necesitan acceso a `bcn.cl`, `sii.cl` y `midesof.gob.cl`; [P-03 · Corregir el origen de las fechas](BLUEPRINT.md#p-03) no necesita red.

## Cómo verificar

`make verificar` (requiere Python 3 y R). Qué comprueba y qué no: [Verificación](BLUEPRINT.md#verificación).

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
