# Decisiones

Serie **D-nn**: decisiones del proyecto; este es el único archivo donde se define el prefijo D- y donde viven sus ítems. Estados: **Aprobada** (la aprobó una persona del equipo; se indica quién y dónde), **Propuesta** (la plantea el investigador y no rige hasta que una persona la apruebe), **Abierta** (falta información) y **Cerrada por evidencia** (la norma o los datos la resuelven y falta que una persona la dé por aprobada). Claude Science no aprueba decisiones. Para citar un ítem desde otro archivo: `[D-03 · Python y R en paralelo](DECISIONES.md#d-03)`.

### D-01

**El repositorio es la fuente de verdad.** La memoria de Claude Science no lo es. El trabajo va en ramas `cs/<n>-<tema>`; nadie sube directamente a `master`.

Estado: Aprobada. Instrucción inicial del usuario en la sesión de Claude Code del 2026-10-22 (no verificable desde el repo).

### D-02

**Alcance: todos los cambios regulatorios que hayan abaratado o encarecido donar con fines sociales, y cada uno se testea por separado.** Abaratar o encarecer se mide con el precio fiscal de donar. Qué normas entran, incluidas las de borde (otros regímenes de donación, cambios generales de tasas), lo fijan los límites de más abajo, que propuso [P-01 · Inventario normativo](BLUEPRINT.md#p-01).

Estado: Aprobada. En lo general, usuario, sesión de Claude Code del 2026-10-22 (no verificable desde el repo): «detectar todos los cambios regulatorios que hayan abaratado o encarecido donar con fines sociales de forma de testear cada cambio y evaluar su impacto». Los límites de abajo: usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «D-02 […] se dan aprobados».

Límites (los propuso el investigador el 2026-10-22; aprobados el 2026-10-23):

- Dentro: el Título I de la Ley 19.885 y sus modificaciones, los cambios de la tasa y escala del impuesto a la renta que alteran el precio de donar, y los regímenes que compiten con él para el mismo donante: la Ley 21.440 y las donaciones de la medida alternativa de la Ley 21.015.
- Fuera por ahora: los regímenes de donación con otros fines (cultura, deporte, universidades, reconstrucción). Entran solo como controles o a través del límite global del artículo 10, que los suma. Las normas que el artículo 10 enumera están en el texto de la [Ley 19.885 vigente](normativa/textos/ley19885_v2024-08-24.txt).

### D-03

**Python y R en paralelo para contrastar resultados.** Todo resultado numérico se calcula en las dos implementaciones y `make verificar` compara las salidas con tolerancia explícita. El nivel que da este contraste está en [D-05 · Niveles de resultado](#d-05).

Estado: Aprobada. Usuario, sesión de Claude Code del 2026-10-22 (no verificable desde el repo): «vamos a codear en python y R simultáneamente para contrastar los resultados y agregarles un nivel de verificación superior».

### D-04

**La estructura de los datos se re-audita desde la web del Ministerio antes de usar cualquier supuesto previo.** Ni el ETL actual ([code/ETL.R L18-L32](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/code/ETL.R#L18-L32), años 2010-2020 desde otro dominio) ni el extracto empaquetado se consideran vigentes. Fuente: [Donaciones recibidas Ley N° 19.885](https://www.midesof.gob.cl/sociedad-civil/donaciones-sociales/donaciones-recibidas-ley-n19-885/). Tarea: [P-02 · Estructura de los datos](BLUEPRINT.md#p-02).

Estado: Aprobada. Usuario, sesión de Claude Code del 2026-10-22 (no verificable desde el repo): «se han actualizado mucho en la web donde están disponibles».

### D-05

**Niveles de resultado.** *Borrador*: lo produce un script, sin contraste. *Contrastado*: Python y R coinciden dentro de la tolerancia. *Verificada*: contrastado, además el diseño recupera un efecto conocido en datos simulados con semilla fija y, si depende de una norma, la cita textual está comprobada por [verifica_citas.py](analisis/verifica_citas.py). Ningún resultado se llama «verificada» sin un script en `analisis/` que falle con `AssertionError` si no se cumple. Tolerancia para cálculos deterministas: 1e-12 (la usa [contraste.py](analisis/contraste.py)); para estimadores, se fija por estimador antes de correrlo y se registra aquí.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «propuesta D-05 debe quedar aprobada». La tolerancia de 1e-12 y la división en tres niveles las planteó el investigador (inferencia); la regla del `AssertionError` viene de la instrucción inicial del usuario.

### D-06

**Canal de trabajo.** Los issues estuvieron deshabilitados (el 2026-10-22, crear uno respondió «410 Issues has been disabled in this repository») y después se habilitaron. El 2026-10-23 existen las etiquetas `dueño:claude-science`, `bloqueado` y `propuesta` (comprobado con la API de GitHub). No existen `prioridad:alta`, `prioridad:media`, `prioridad:baja` ni `dueño:humano` (comprobado con la API de GitHub el 2026-10-23; las pruebas con otros nombres de prioridad tampoco encontraron etiqueta). El handoff del 2026-10-22 fija esos cuatro nombres (no verificable desde el repo): hay que crearlos. Mientras no existan las de prioridad, el orden lo fija la persona que asigna `dueño:claude-science`. El protocolo «toma el primer issue con `dueño:claude-science`» puede ejecutarse; los issues con `propuesta` no se toman, y [BLUEPRINT.md](BLUEPRINT.md#pendientes) sigue describiendo cada pendiente.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «etiquetas creadas». Falta crear las etiquetas de prioridad y `dueño:humano`.

### D-07

**Sentido del cambio de la Ley 20.316.** El encargo inicial dice que el aporte al Fondo Mixto era voluntario bajo la Ley 19.885 y que la Ley 20.316 lo hizo condicional al 33 % del exceso sobre 1.000 UTM (texto pegado por el usuario el 2026-10-22; no verificable desde el repo). El [artículo del repo, L117](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L117) dice lo contrario: «The mayor changes was the elimination of the obligatory of the donation to the MF». La respuesta fija el signo de la hipótesis y el título del artículo; solo la norma la resuelve, vía [P-01 · Inventario normativo](BLUEPRINT.md#p-01).

Resolución por la norma (2026-10-22): el texto original de la Ley 19.885 vigente desde 2003-08-06 dice en el artículo 1 N° 2: «Para que proceda este beneficio, a lo menos el 33% de la donación que da derecho al mismo deberá efectuarse al fondo que establece el artículo 3º.» ([ley19885_v2003-08-06.txt](normativa/textos/ley19885_v2003-08-06.txt), BCN; fila correspondiente en [cambios.csv](normativa/cambios.csv)). Es decir, el aporte al Fondo era obligatorio para toda donación con beneficio, y la Ley 20.316 lo flexibilizó (a la fecha de la donación desde 2009-01-01, según su [artículo 2 transitorio](normativa/textos/ley20316_v2026-10-22.txt)). El artículo del repo describe el sentido correcto; el encargo inicial no.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «D-07 el artículo es el sentido correcto». La norma la había resuelto en el mismo sentido.

### D-08

**Ventana empírica.** Los cambios se prueban con datos de 2012 a 2025; los de 2009 a 2011 no se tratan como evaluables con las planillas actuales, y los datos anteriores a 2012 sirven solo de contexto descriptivo. Razón: la [tabla de evaluabilidad](BLUEPRINT.md#evaluabilidad-de-cada-cambio) y el [perfil de los datos](BLUEPRINT.md#p-02).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «D-08 […] se dan aprobados». La razón es inferencia del investigador. El usuario indicó que por ahora no hay más información para completar la cobertura ([D-09 · Fuente y completitud de los datos](#d-09)); se revisa si aparecen datos completos de 2008 a 2011.

### D-09

**Fuente y completitud de los datos.** Los archivos vigentes en la web del Ministerio son la mejor fuente disponible; se contrastan con versiones anteriores solo para ver cambios de cobertura. El usuario indica que el Ministerio los va actualizando y limpiando con el tiempo (nombres de personas con caracteres corruptos, `AÑO` incoherente con el archivo de origen, montos no numéricos), por lo que la cobertura y los valores del Fondo Mixto se toman como los disponibles. Los RUT que existen se pueden usar para completar los faltantes por similitud de nombre. Por ahora no hay más información para completar; en el futuro se evaluará completar con los datos de [viantirreau/donaciones](https://github.com/viantirreau/donaciones). Qué completar y qué excluir lo fija [D-10 · Criterio para completar o excluir](#d-10).

Los datos del régimen de la Ley 21.440 están publicados en [donacionesley21440.gob.cl/donaciones](https://donacionesley21440.gob.cl/donaciones); su tarea es [P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «los datos más recientes en la web son lo mejor que hay […] la cobertura y valores de fondo mixto se deben tomar como los que están disponibles».

### D-10

**Criterio para completar o excluir filas de las planillas de la Ley 19.885.** Lo plantea el investigador por delegación del usuario ([D-09](#d-09): «toma un criterio razonable para completar o excluir»). Cada regla se aplica con un contador por año (cuántas filas toca), se calcula en Python y en R y se contrasta; nada se borra en silencio.

1. **Montos.** No se imputan. Si el monto total no es numérico pero los de Fondo Mixto e Institución sí lo son, el total se recupera como su suma solo en los años en que, en las filas válidas, el total coincide con esa suma en al menos el 99,9 % de los casos; si no, la fila se excluye del análisis y se cuenta.
2. **Fondo Mixto.** Cuenta el monto cuando es numérico. Donde viene como «SI/NO» se usa como indicador, y el cumplimiento del 33 % se marca «no medible» (2019, 2021 y parte de 2018); no se imputa.
3. **Fechas.** Manda la fecha de la donación cuando es válida y cae en el rango de los años publicados; si es texto, se interpreta con los formatos observados. Si es inválida o está fuera de rango, se usa `AÑO` como año y el mes queda desconocido, y la fila no entra en análisis por mes.
4. **`AÑO` incoherente con el archivo.** El año de la donación es el de la fecha válida, no el del archivo ni el de la columna `AÑO`.
5. **Duplicados exactos.** Se conservan con marca y se informa un análisis de sensibilidad que los excluye: sin identificador de donación no se puede saber si son errores o donaciones iguales el mismo día.
6. **Nombres corruptos** (U+FFFD). La fila se conserva; al comparar nombres, esas posiciones valen como comodín.
7. **RUT faltante.** Se completa con el RUT observado del mismo donante, primero por nombre normalizado idéntico y luego por similitud, con un umbral calibrado ocultando RUT conocidos y exigiendo al menos 99 % de precisión en esa prueba. Los RUT con dígito verificador inválido (Módulo 11) no se usan para completar. Lo que siga sin RUT se analiza por nombre normalizado y se marca como error de medición.
8. **Cobertura.** Se toma la disponible; no se imputan años ni filas ([D-08 · Ventana empírica](#d-08)).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «D-10 […] se dan aprobados». Los umbrales de 99,9 % y 99 % son del investigador (inferencia) y se revisan con los datos.

### D-11

**Criterios de reproducibilidad.** Se toman como aplicables [The Turing Way](https://book.the-turing-way.org/reproducible-research/compendia) y la [política de datos y código de la Econometric Society](https://mail.econometricsociety.org/publications/es-data-editor-website/data-and-code-availability-policy). Enlaces tomados del handoff del 2026-10-22; no leí ninguna de las dos páginas, así que su contenido está sin verificar. Se concretan en cinco criterios:

1. Un script maestro: `make verificar` y, para lo que necesita datos, `make verificar-datos` ([Makefile](Makefile)).
2. Semilla fija en toda simulación, escrita en el script ([BLUEPRINT.md, Verificación](BLUEPRINT.md#verificación)).
3. En el README: software y versiones usadas, hardware y tiempo de ejecución de `make verificar` y de `make verificar-datos`.
4. Dependencias con versión fija: `analisis/requirements.txt` hoy no fija ninguna, y la versión de R y de `readxl` no está registrada.
5. Carpeta de resultados: abierta. `salidas/` no se versiona porque se regenera; qué cifras se guardan y dónde se decide cuando exista el primer resultado de estimación.

No se adopta la versión mínima de Python (3.12 o superior) del handoff: viene de un issue de otro repositorio que no pude leer. La versión se fija con la que corra `make verificar`.

Estado: Propuesta. Origen: el handoff recoge una decisión del usuario del 2026-10-20, en otra conversación: «se toman como aplicables The Turing Way y la política de la Econometric Society» (no verificable desde el repo; el archivo no está en él). Los cinco criterios y la lectura del quinto son inferencia del investigador. Falta que una persona la apruebe.

