# Decisiones

Serie **D-nn**: decisiones del proyecto; este es el único archivo donde se define el prefijo D- y donde viven sus ítems. Estados: **Aprobada** (la aprobó una persona del equipo; se indica quién y dónde), **Propuesta** (la plantea el investigador y no rige hasta que una persona la apruebe) y **Abierta** (falta información). Claude Science no aprueba decisiones. Para citar un ítem desde otro archivo: `[D-03 · Python y R en paralelo](DECISIONES.md#d-03)`.

### D-01

**El repositorio es la fuente de verdad.** La memoria de Claude Science no lo es. El trabajo va en ramas `cs/<n>-<tema>`; nadie sube directamente a `master`.

Estado: Aprobada. Instrucción inicial del usuario en la sesión de Claude Code del 2026-10-22 (no verificable desde el repo).

### D-02

**Alcance: todos los cambios regulatorios que hayan abaratado o encarecido donar con fines sociales, y cada uno se testea por separado.** Abaratar o encarecer se mide con el precio fiscal de donar. Qué normas entran, incluidas las de borde (otros regímenes de donación, cambios generales de tasas), lo propone [P-01 · Inventario normativo](BLUEPRINT.md#p-01) y lo aprueba una persona.

Estado: Aprobada en lo general. Usuario, sesión de Claude Code del 2026-10-22 (no verificable desde el repo): «detectar todos los cambios regulatorios que hayan abaratado o encarecido donar con fines sociales de forma de testear cada cambio y evaluar su impacto». Los límites del alcance están Abiertos.

### D-03

**Python y R en paralelo para contrastar resultados.** Todo resultado numérico se calcula en las dos implementaciones y `make verificar` compara las salidas con tolerancia explícita. El nivel que da este contraste está en [D-05 · Niveles de resultado](#d-05).

Estado: Aprobada. Usuario, sesión de Claude Code del 2026-10-22 (no verificable desde el repo): «vamos a codear en python y R simultáneamente para contrastar los resultados y agregarles un nivel de verificación superior».

### D-04

**La estructura de los datos se re-audita desde la web del Ministerio antes de usar cualquier supuesto previo.** Ni el ETL actual ([code/ETL.R L18-L32](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/code/ETL.R#L18-L32), años 2010-2020 desde otro dominio) ni el extracto empaquetado se consideran vigentes. Fuente: [Donaciones recibidas Ley N° 19.885](https://www.midesof.gob.cl/sociedad-civil/donaciones-sociales/donaciones-recibidas-ley-n19-885/). Tarea: [P-02 · Estructura de los datos](BLUEPRINT.md#p-02).

Estado: Aprobada. Usuario, sesión de Claude Code del 2026-10-22 (no verificable desde el repo): «se han actualizado mucho en la web donde están disponibles».

### D-05

**Niveles de resultado.** *Borrador*: lo produce un script, sin contraste. *Contrastado*: Python y R coinciden dentro de la tolerancia. *Verificada*: contrastado, además el diseño recupera un efecto conocido en datos simulados con semilla fija y, si depende de una norma, la cita textual está comprobada por [verifica_citas.py](analisis/verifica_citas.py). Ningún resultado se llama «verificada» sin un script en `analisis/` que falle con `AssertionError` si no se cumple. Tolerancia para cálculos deterministas: 1e-12 (la usa [contraste.py](analisis/contraste.py)); para estimadores, se fija por estimador antes de correrlo y se registra aquí.

Estado: Propuesta. La tolerancia de 1e-12 y la división en tres niveles son del investigador (inferencia); la regla del `AssertionError` viene de la instrucción inicial del usuario.

### D-06

**Canal de trabajo.** Los issues estuvieron deshabilitados: el 2026-10-22 crear uno respondió «410 Issues has been disabled in this repository». Después se habilitaron: a las 13:06 UTC del mismo día se creó con éxito el [issue 3](https://github.com/ingUANDES/donationstax/issues/3), y esa creación hizo que GitHub creara la etiqueta `propuesta` (color por defecto, sin descripción). Ese mismo día no existían `dueño:claude-science` ni `bloqueado` (`prioridad:*`, sin comprobar), así que el protocolo «toma el primer issue con `dueño:claude-science`» no puede ejecutarse hasta que una persona cree las etiquetas y asigne. Mientras tanto, el backlog son los ítems P-nn de [BLUEPRINT.md](BLUEPRINT.md#pendientes), tomados en el orden que fije una persona.

Estado: Abierta. Opciones: pasar el backlog a issues (con las etiquetas del protocolo) y que BLUEPRINT los enlace, o mantenerlo en BLUEPRINT.

### D-07

**Sentido del cambio de la Ley 20.316.** El encargo inicial dice que el aporte al Fondo Mixto era voluntario bajo la Ley 19.885 y que la Ley 20.316 lo hizo condicional al 33 % del exceso sobre 1.000 UTM (texto pegado por el usuario el 2026-10-22; no verificable desde el repo). El [artículo del repo, L117](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L117) dice lo contrario: «The mayor changes was the elimination of the obligatory of the donation to the MF». La respuesta fija el signo de la hipótesis y el título del artículo; solo la norma la resuelve, vía [P-01 · Inventario normativo](BLUEPRINT.md#p-01).

Estado: Abierta. Sin verificar contra la norma.
