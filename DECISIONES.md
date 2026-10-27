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

Aclaración del 2026-10-27 para los cálculos deterministas, sin estimador: «verificada» es contrastado (Python = R con 1e-12), con los valores esperados calculados a mano coincidiendo, y con cada parámetro con su cita comprobada por [verifica_citas.py](analisis/verifica_citas.py) y sin lectura pendiente. Se evalúa caso por caso: los casos que dependen de una lectura pendiente quedan «contrastado». Hace falta, además, el script con `AssertionError` que ya pide la regla general. El [issue 31](https://github.com/ingUANDES/donationstax/issues/31) pide aplicarlo al precio fiscal de [P-04 · Precio fiscal por régimen](BLUEPRINT.md#p-04).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «propuesta D-05 debe quedar aprobada». La tolerancia de 1e-12 y la división en tres niveles las planteó el investigador (inferencia); la regla del `AssertionError` viene de la instrucción inicial del usuario. La aclaración de los cálculos deterministas: usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «estoy de acuerdo con tus recomendaciones».

### D-06

**Canal de trabajo.** Los issues estuvieron deshabilitados (el 2026-10-22, crear uno respondió «410 Issues has been disabled in this repository») y después se habilitaron. El 2026-10-24 existen las etiquetas `dueño:claude-science`, `dueño:humano`, `bloqueado`, `propuesta`, `prioridad:alta`, `prioridad:media` y `prioridad:baja` (comprobado con la API de GitHub). El handoff del 2026-10-22 fija `prioridad:alta`, `prioridad:media`, `prioridad:baja` y `dueño:humano` (no verificable desde el repo). También existen `prioridad:alto`, `prioridad:1` y `prioridad:p1`, que el protocolo no usa; una persona decide si se borran. Ningún issue abierto lleva todavía una etiqueta de prioridad, así que el orden lo fija la persona que asigna `dueño:claude-science`. El protocolo «toma el primer issue con `dueño:claude-science`» puede ejecutarse; los issues con `propuesta` no se toman, y [BLUEPRINT.md](BLUEPRINT.md#pendientes) sigue describiendo cada pendiente.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «etiquetas creadas». Las de prioridad y `dueño:humano` se comprobaron el 2026-10-24; quién las creó y cuándo no lo sé.

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
3. **Fechas.** Manda la fecha de la donación cuando es válida y cae en el rango de los años publicados; si es texto, se interpreta con los formatos observados. Si es inválida o está fuera de rango, se usa `AÑO` como año y el mes queda desconocido, y la fila no entra en análisis por mes. (Reemplazada en parte por [D-14 · Año de la donación cuando la fecha cae en otro año](#d-14): la fecha ya no manda sobre el año del archivo.)
4. **`AÑO` incoherente con el archivo.** El año de la donación es el de la fecha válida, no el del archivo ni el de la columna `AÑO`. (Reemplazada por [D-14 · Año de la donación cuando la fecha cae en otro año](#d-14).)
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
4. Dependencias con versión fija en `analisis/requirements.txt`; la versión de R y de `readxl`, en el README.
5. Carpeta de resultados: `salidas/` no se versiona porque se regenera. Las tablas derivadas que sí se versionan van en `datos/`; las entradas de la normativa (`cambios.csv`, `precio_casos.csv` y `precio_comparaciones.csv`) van en `normativa/`. Decidido el 2026-10-27, a propuesta del investigador, con la aprobación del usuario («estoy de acuerdo con tus recomendaciones», conversación de Claude Code del 2026-10-27, no verificable desde el repo); las dos salidas de [P-04 · Precio fiscal por régimen](BLUEPRINT.md#p-04) se movieron de `normativa/` a `datos/`. Qué cifras de una estimación se guardan y dónde se decide cuando exista el primer resultado de estimación.

No se adopta la versión mínima de Python (3.12 o superior) del handoff: viene de un issue de otro repositorio que no pude leer. La versión se fija con la que corra `make verificar`.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «D-11 aprobado». Origen: el handoff recoge una decisión del usuario del 2026-10-20, en otra conversación: «se toman como aplicables The Turing Way y la política de la Econometric Society» (no verificable desde el repo; el archivo no está en él). Los cinco criterios y la lectura del quinto son inferencia del investigador.

### D-12

**Variable de resultado: monto donado.** El monto donado es el monto total de cada donación: en las planillas de la Ley 19.885, la columna `MONTOTOTALDONACION` ([esquema_por_anio.csv](datos/esquema_por_anio.csv)), con el total recuperado y las filas excluidas según [D-10 · Criterio para completar o excluir](#d-10); en los datos de la Ley 21.440, su columna de valor ([P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07)).

Queda abierto:

- La unidad: pesos nominales, como vienen en los archivos, o convertidos a UTM o a pesos reales. La serie de UTM o de IPC no está en el repo y no se inventa: se pide en un issue cuando algún diseño la necesite.
- El nivel de agregación (donación, donante-año, donante-institución-año): lo fija cada diseño en [P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#p-05).

Limitación (inferencia): las planillas son de donaciones recibidas (así se titula la fuente en [D-04](#d-04)), así que no traen donantes que no donaron; el monto es condicional a donar y el efecto sobre la decisión de donar no se identifica solo con estos datos.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-23 (no verificable desde el repo): «la variable resultado debería ser monto donado». Las columnas, los puntos abiertos y la limitación son del investigador (inferencia).

### D-13

**Casos que el criterio para completar o excluir no cubre al armonizar las planillas de la Ley 19.885.** Los plantea el investigador al aplicar [D-10 · Criterio para completar o excluir](#d-10) en [P-06 · Armonizar las planillas](BLUEPRINT.md#p-06). La regla exacta de cada caso está en la cabecera de [armonizar.py](analisis/armonizar.py) (puntos 1 a 5) o de [completar_rut.py](analisis/completar_rut.py) (puntos 6 a 8), marcada «[i]», y sus conteos por año en [armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv) y [rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv).

1. **Filas sin `AÑO` ni fecha válida.** Toman el año del archivo, con el mes desconocido: el año siempre se puede reconstruir (ver el Estado). Cambio del 2026-10-26: la propuesta original las excluía («sin_anio»). Es 1 fila de 2014; [armonizar.py](analisis/armonizar.py) y [armonizar.R](analisis/armonizar.R) la incluyen desde el [issue 20](https://github.com/ingUANDES/donationstax/issues/20).
2. **Detalles de la regla 1.** El total coincide con Fondo Mixto + Institución si la diferencia es de a lo más 1e-6. Un año sin filas con los tres montos numéricos no es apto para recuperar totales (2019 y 2021, donde el Fondo Mixto viene como SI/NO; no tienen totales no numéricos, así que no cambia nada). Los 9 montos de 2024 escritos como «$ 20,000,000» no se leen y su fila se excluye. Alternativa: leerlos quitando «$» y las comas.
3. **Fondo Mixto vacío o con otro texto.** El 33 % tampoco es medible: 32.510 filas de 2014, por ejemplo.
4. **Fechas.** Se interpretan los formatos que lista la cabecera de armonizar.py. Las fechas parciales (solo el mes, mes y año, día y mes, solo el año) y los años de 2 o 5 dígitos no se interpretan; esas filas toman el año de `AÑO` y el mes queda desconocido. Alternativa: usar el mes cuando el texto lo nombra. Las fechas válidas van del 2008-01-01 al 2025-12-31, y `AÑO` debe estar en el mismo rango.
5. **RUT del donante.** El formato de los valores sin guion se decide por año con Módulo 11: en 2022 son el número sin dígito verificador; en los demás años, cuerpo + dígito verificador. Un valor de 9 dígitos o terminado en K se lee siempre como cuerpo + dígito verificador.
6. **Nombre normalizado (regla 7).** Se quitan tildes y diéresis, la Ñ y la Ç pasan a N y C, todo va a mayúscula, U+FFFD pasa a «?» y lo demás que no sea letra, dígito o «?» pasa a espacio. La tabla exacta está en la cabecera de [completar_rut.py](analisis/completar_rut.py).
7. **RUT que se usan para completar.** Los de dígito verificador válido y los números sin dígito verificador de 2022, que no se pueden comprobar. Si los nombres compatibles tienen más de un RUT, el caso queda «ambiguo» y no se completa.
8. **Similitud.** Se mide como s = 1 − distancia de Levenshtein / largo del nombre más largo, y no con Jaro-Winkler. Razón: las bibliotecas rápidas de Python (rapidfuzz, jellyfish) y stringdist de R implementan variantes distintas de Jaro, que difirieron en 38 de 310 pares de prueba (comprobación sin script), y no habría contraste Python/R exacto. Detalles:
   - se compara solo con nombres que empiezan con los mismos 3 caracteres;
   - los nombres con «?» pasan solo por el nombre idéntico;
   - se cuenta por nombre distinto;
   - el umbral es el menor de 0,80 a 0,99 con al menos 100 asignaciones y precisión de al menos 99 %.

   Con estas reglas, ningún umbral cumple y el paso por similitud no completa ningún RUT ([P-06 · Armonizar las planillas](BLUEPRINT.md#p-06)).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-26 (no verificable desde el repo): «primero aprueba D-13 evaluando que dependiendo del archivo de origen el año siempre se puede reconstruir». Evaluación del investigador (comprobación sin script, ver [D-14 · Año de la donación cuando la fecha cae en otro año](#d-14)): el año del archivo está siempre disponible, así que ninguna fila necesita excluirse por falta de año. De ahí el cambio en el punto 1. La evaluación decía también que la columna `AÑO` lo repite en las 1.824 filas sin fecha válida (100 %). El script del [issue 20](https://github.com/ingUANDES/donationstax/issues/20) lo corrige: son 1.823 de 1.824, y la que falta es la fila de 2014 sin `AÑO` de este punto ([anio_evaluacion_por_anio.csv](datos/anio_evaluacion_por_anio.csv), fila «total»). Los otros siete puntos se aprueban como los planteó el investigador. [armonizar.py](analisis/armonizar.py) y [armonizar.R](analisis/armonizar.R) aplican el punto 1 desde el issue 20.

### D-14

**Año de la donación cuando la fecha cae en un año distinto del archivo.** La regla 4 de [D-10 · Criterio para completar o excluir](#d-10), aprobada, fija que el año de la donación es el de la fecha válida y no el del archivo ni el de la columna `AÑO`. La evaluación del 2026-10-26 sobre las filas armonizadas se hizo sin script; desde el [issue 20](https://github.com/ingUANDES/donationstax/issues/20), [evalua_anio.py](analisis/evalua_anio.py) y [evalua_anio.R](analisis/evalua_anio.R) la reproducen, contrastada, en [anio_evaluacion_por_anio.csv](datos/anio_evaluacion_por_anio.csv) (fila «total» y filas por año) y [anio_conflictos.csv](datos/anio_conflictos.csv), sobre las filas incluidas. Muestra:

- La columna `AÑO` coincide con el año del archivo en 1.823 de las 1.824 filas sin fecha válida y en el 99,999 % de las 7.875.527 que sí la tienen. Corrección del script: la evaluación sin script decía «1.824 (100 %)»; la fila que falta es la de 2014 sin `AÑO` ([D-13](#d-13), punto 1).
- La fecha cae en el mismo año del archivo en el 99,932 % de esas filas. En las 5.387 restantes, `AÑO` coincide con el año del archivo en 5.352 (99,4 %) y con el de la fecha en 35; en ninguna coincide con un tercer año.
- De las 5.387, la mayoría tiene la fecha en el año siguiente al del archivo, y 4.439 de ellas en febrero. Inferencia: son fechas del certificado y no de la donación (sin verificar; el archivo no lo dice).

Regla:

- El año de la donación es el del archivo de origen, que siempre se puede reconstruir.
- La fecha vale solo si hay información de fecha: una fecha válida da el mes cuando cae en el año del archivo. Sin información de fecha (vacía, no interpretable, inválida o fuera de rango), se usa la información del archivo de origen: su año, y el mes queda desconocido.
- Si la fecha válida cae en otro año, el mes queda desconocido y la fila se marca. El año de la fecha se conserva en una columna aparte para sensibilidad: cambiar de criterio es cambiar de columna.
- Reemplaza la regla 4 de [D-10 · Criterio para completar o excluir](#d-10) y la parte de la regla 3 que da prioridad a la fecha.

Efecto: mueve 5.387 filas (0,07 %) entre años de donación. Entre las filas con fecha válida, 2018 tiene 482.043 con el año de la fecha y 486.497 con el del archivo; 2019, 474.445 y 469.930.

Aclaraciones, a partir de las cuatro preguntas que dejó el [PR 25](https://github.com/ingUANDES/donationstax/pull/25) y de las recomendaciones del investigador:

1. Las 35 filas en que `AÑO` coincide con el año de la fecha y no con el del archivo toman el año del archivo, como dice la regla.
2. Las 8 filas con la fecha en el año del archivo y `AÑO` distinto toman el año del archivo y el mes de la fecha. No se marcan por fila: basta el conteo `n_anio_columna_distinto_archivo`.
3. `fecha_donacion` conserva la fecha completa aunque el mes de la donación quede desconocido. Su mes no es el de la donación: para eso está `mes_donacion`.
4. [armonizacion_anios_donacion.csv](datos/armonizacion_anios_donacion.csv) se retira, porque los dos años coinciden por construcción. Lo pide el [issue 26](https://github.com/ingUANDES/donationstax/issues/26).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-26 (no verificable desde el repo): «aceptar propuesta si complementar que la regla 4 del d-10 solo vale si hay info de fecha sino se usa la info del archivo de origen». La plantea el investigador a partir de lo que dijo el usuario al aprobar [D-13 · Casos que el criterio para completar o excluir no cubre](#d-13). Cómo la leo (inferencia): la propuesta rige tal cual, y el complemento fija lo que pasa sin fecha, que es usar el archivo. Si se quiso que la fecha siga mandando cuando cae en otro año, basta cambiar la columna del año, y por eso el año de la fecha se conserva. [armonizar.py](analisis/armonizar.py) y [armonizar.R](analisis/armonizar.R) la aplican desde el [issue 20](https://github.com/ingUANDES/donationstax/issues/20): el año de la fecha queda en la columna `anio_fecha` y la marca, en `fecha_otro_anio`. Las cuatro aclaraciones: usuario, conversación de Claude Code del 2026-10-26 (no verificable desde el repo): «Apruebo tus 4 recomendaciones».

### D-15

**Tolerancia entre Python y R para las simulaciones.** Los scripts de [P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#p-05) generan en los dos lenguajes las mismas extracciones. Usan MT19937 iniciado con `init_genrand(semilla)` y uniformes $y/2^{32}$ de la salida de 32 bits; en R, `runif` con ese estado cargado en `.Random.seed`. Las normales salen por Box–Muller, $\sqrt{-2\ln u_1}\cos(2\pi u_2)$, con $u_1$ y $u_2$ de dos bloques consecutivos. Sobre los mismos datos, las diferencias solo pueden venir del álgebra lineal y del redondeo de `log` y `cos`. La tolerancia, para cada cifra de salida, es $|x_{py} - x_R| \le 10^{-8}\max(1, |x_{py}|)$. La copia versionada de cada salida se compara con la misma tolerancia, y no con `cmp`, porque `log` y `cos` pueden diferir en el último bit entre plataformas.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador el 2026-10-27, antes de correr las simulaciones ([issue 22](https://github.com/ingUANDES/donationstax/issues/22)). La fija el commit que la agrega, anterior al de los resultados. Inferencia: $10^{-8}$ relativo deja varios órdenes de margen sobre el redondeo de mínimos cuadrados con matrices de este tamaño, y detecta cualquier diferencia de implementación.

### D-16

**Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM.** El caso simulado:

- 300.000 montos por donante, institución y año, la unidad del umbral desde 2012 ([cambios.csv, L22](normativa/cambios.csv#L22)). $z^\ast$, el logaritmo del monto contrafactual en UTM con el precio del tramo inferior en todo el rango, sigue $N(\ln 1000 - 0{,}5,\ 1)$.
- Precios: $P_0$ del tramo hasta 1.000 UTM y $P_1$ del exceso sin aporte al Fondo Mixto, de los casos `e2012_hasta_1000` y `e2012_exceso_sin_fondo` de [precio_fiscal_por_caso.csv](datos/precio_fiscal_por_caso.csv). $P_1/P_0$ no depende de la tasa, porque $g = 1 - c$ en los dos tramos (inferencia).
- Elasticidad $\varepsilon = 0{,}5$ y $\Delta z^\ast = \varepsilon\ln(P_1/P_0)$.
- El 20 % de los donantes aporta al Fondo lo que exige el art. 1 N° 8 ([cambios.csv, L20](normativa/cambios.csv#L20)) y no enfrenta el quiebre: dona $z^\ast$.
- El resto dona $z^\ast$ si $z^\ast \le k = \ln 1000$. Si $k < z^\ast \le k + \Delta z^\ast$, se agolpa en un punto uniforme de $[k - 0{,}04,\ k)$. Si $z^\ast > k + \Delta z^\ast$, dona $z^\ast - \Delta z^\ast$.

Estimador:

- Tramos de ancho $w = 0{,}02$ en $z$ sobre $[k - 1,\ k + 1)$.
- Un polinomio de orden $q$ a cada lado de $k$ y una dummy por tramo de la ventana excluida. La masa en exceso $\hat B$ es la suma de las dummies.
- $\hat\varepsilon = 2w\hat B / [(\hat h_- + \hat h_+)\ln(P_1/P_0)]$, con $\hat h_-$ y $\hat h_+$ el polinomio de cada lado evaluado en $k$. Es la fórmula del trapecio, exacta si la densidad contrafactual es lineal entre $k$ y $k + \Delta z^\ast$.
- Error estándar: desviación de 200 réplicas que suman a los conteos ajustados residuos remuestreados con reposición.
- Especificación principal: $q = 5$ y ventana $[k - 0{,}08,\ k)$.

Tolerancias, con $B$ el número simulado de donantes que se agolpan:

1. Principal, sin los donantes del Fondo: $|\hat\varepsilon - \varepsilon| \le 0{,}10\,\varepsilon$ y $|\hat B - B| \le 0{,}10\,B$.
2. Sensibilidad, sin los donantes del Fondo: $q$ de 3 a 6; límite inferior de la ventana en $k - 0{,}06$, $k - 0{,}08$ o $k - 0{,}10$; superior en $k$ o $k + 0{,}02$. En cada una de las 24 combinaciones, $|\hat\varepsilon - \varepsilon| \le 0{,}15\,\varepsilon$.
3. Con todos los donantes, la fórmula del quiebre simple, especificación principal: $|\hat B - B| \le 0{,}10\,B$; $\hat\varepsilon \le 0{,}9\,\varepsilon$, de modo que el sesgo se detecta; y $|\hat\varepsilon - 0{,}8\,\varepsilon| \le 0{,}15 \cdot 0{,}8\,\varepsilon$. El valor $0{,}8\,\varepsilon$ es el esperado si el 20 % que no se agolpa entra en la densidad del denominador (inferencia).
4. El error estándar de la especificación principal es finito y positivo.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador el 2026-10-27, antes de correr las simulaciones ([issue 22](https://github.com/ingUANDES/donationstax/issues/22)). Los parámetros del caso son elección del investigador, no cifras de los datos, salvo los precios. Aprobarla no cambia el resultado de la simulación: las cinco reglas de sensibilidad que incumple están registradas en [sim_hallazgos.csv](analisis/fixtures/sim_hallazgos.csv) y la discusión del rediseño está en el [issue 34](https://github.com/ingUANDES/donationstax/issues/34); cambiar esta tolerancia sería una decisión nueva.

### D-17

**Tolerancias de la simulación de estudio de eventos y diferencias en diferencias.** El caso simulado:

- 40.000 donantes, años 2012 a 2019, un monto por donante y año.
- $y_{it} = \alpha_i + \lambda_t + \beta T_i 1[t \ge 2015] + v_{it}$, con $y$ el logaritmo del monto anual en UTM, $\alpha_i \sim N(\ln 1000 - 1,\ 1{,}2^2)$, $v_{it} \sim N(0,\ 0{,}6^2)$ independiente entre años, $\lambda_t = 0{,}02\,(t - 2012)$ y $\beta = 0{,}15$.
- Estimador: estudio de eventos con efectos fijos de unidad (desviaciones de la media de cada identificador), dummies de año y $T_i$ por dummy de año, con 2014 como referencia. Da siete coeficientes $\hat\gamma_t$.

Casos y tolerancias:

1. $T_i$ exógeno, Bernoulli de probabilidad 1/2: $|\hat\gamma_t - \beta 1[t \ge 2015]| \le 0{,}03$ en cada año.
2. Tratamiento por el nivel previo, $T_i = 1[y_{i,2014} > \ln 1000]$: $|\hat\gamma_t - (\beta 1[t \ge 2015] + b)| \le 0{,}03$ en cada año, con $b = -(\sigma_v^2/s)\,\phi(a)\,[1/(1 - \Phi(a)) + 1/\Phi(a)]$, $s^2 = \sigma_\alpha^2 + \sigma_v^2$ y $a = (\ln 1000 - \mu_\alpha - \lambda_{2014})/s$. Además, $|b| \ge 0{,}06$. $b$ es la reversión a la media; los coeficientes de 2012 y 2013 también la toman, así que la prueba de tendencias previas la detecta.
3. Grupo de control por nivel permanente: tratados si $e^{\alpha_i} > 1000$; control si $200 \le e^{\alpha_i} \le 900$. Los del control con $e^{\alpha_i} > 700$ reciben $\beta_c = 0{,}5$ desde 2015, como los que dejó fuera la regla de dos instituciones y 700 UTM ([cambios.csv, L7](normativa/cambios.csv#L7)). Con una sola dummy $T_i 1[t \ge 2015]$: $|\hat\delta - (\beta - \pi_c\beta_c)| \le 0{,}03$, con $\pi_c$ la proporción simulada del control sobre 700 UTM, y $\pi_c\beta_c \ge 0{,}06$.
4. Error por RUT faltante. Cada donante-año se marca con probabilidad $e_t$ = `n_error_medicion`/`n_filas` de [rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv): 32,4 % en 2012, 29,2 % en 2013 y 56,4 % en 2014, y lo que dé el archivo en 2015 a 2019. Un donante-año marcado queda con el identificador de nombre del donante, distinto del de RUT: sus años marcados se unen entre sí y no con los que tienen RUT. Inferencia sobre el mecanismo: el RUT se completó por nombre idéntico ([D-10 · Criterio para completar o excluir](#d-10), regla 7), así que lo que quedó sin RUT es un nombre que no aparece igual en los años con RUT. Los casos 1 y 2 se estiman otra vez con esos identificadores; en el 2, $T$ sale de la observación de 2014 de cada identificador, y los que no la tienen se excluyen. Valen las tolerancias de los casos 1 y 2. Además, la proporción de donantes con el mismo identificador en 2014 y en 2015 queda a $\pm 0{,}02$ de $(1 - e_{2014})(1 - e_{2015})$.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador el 2026-10-27, antes de correr las simulaciones ([issue 22](https://github.com/ingUANDES/donationstax/issues/22)). Los parámetros del caso son elección del investigador, no cifras de los datos, salvo $e_t$. Una fila por donante-año es una simplificación (inferencia): con varias filas por año, el error también partiría el monto anual. Aprobarla no cambia el resultado de la simulación: las dos reglas del coeficiente de 2012 que incumple están registradas en [sim_hallazgos.csv](analisis/fixtures/sim_hallazgos.csv) y la discusión del rediseño está en el [issue 34](https://github.com/ingUANDES/donationstax/issues/34); cambiar esta tolerancia sería una decisión nueva.

### D-18

**Tolerancias de la simulación de variables instrumentales.** El caso simulado:

- 5.000 empresas en los años 2013 a 2017, 2019, 2020, 2023 y 2025, los que tienen precio por régimen en [precio_fiscal_por_caso.csv](datos/precio_fiscal_por_caso.csv).
- Mitad de tipo A (art. 14 letra A de 2017 a 2019 y desde 2020) y mitad de tipo B (letra B de 2017 a 2019 y Pro Pyme desde 2020); de 2013 a 2016 las dos tienen el mismo precio. $P^{leg}$ es el precio del tramo hasta 1.000 UTM de esos casos.
- $\ln D_{it} = \alpha_i + \lambda_t - \varepsilon\ln P^{ef}_{it} + u_{it}$, con $\varepsilon = 1$, $\alpha_i \sim N(0, 1)$, $\lambda_t = 0{,}03\,(t - 2013)$, $u_{it} = 0{,}5\,(0{,}6\,\eta_{it} + 0{,}8\,e_{it})$ y $\eta_{it}, e_{it} \sim N(0, 1)$.
- Pérdida: $L_{it} = 1[\eta_{it} < -1{,}2816]$, un 10 %. $P^{ef} = P^{leg}$ sin pérdida y 1 con pérdida (caso `e2019_perdida`). La pérdida hace endógeno el precio efectivo.
- Error por RUT faltante como en [D-17](#d-17), punto 4, con el $e_t$ de esos años.

Estimadores, todos con efectos fijos de identificador (desviaciones de su media), y tolerancias:

1. MCO con dummies de año: $\hat\varepsilon_{MCO} - \varepsilon \ge 0{,}15$. El sesgo existe y va en la dirección conocida.
2. MC2E con dummies de año e instrumento $\ln P^{leg}$: $|\hat\varepsilon - \varepsilon| \le 0{,}15$ sin y con el error por RUT, y $F \ge 10$ en la primera etapa.
3. Instrumento sin variación entre donantes: $\ln P^{leg}$ del tipo A en cada año, con dummies de año. El residuo del instrumento tras los efectos fijos debe ser $\le 10^{-10}$ en valor absoluto, es decir, no identificado. Lo mismo con el precio de una tasa constante de 27 % para todos (caso `e2025_14A_hasta_1000`).
4. El instrumento por año del punto 3, sin dummies de año: $|\hat\varepsilon - \varepsilon| \ge 0{,}15$. El sesgo viene de $\lambda_t$, que se mueve con la tasa.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador el 2026-10-27, antes de correr las simulaciones ([issue 22](https://github.com/ingUANDES/donationstax/issues/22)). Los parámetros del caso son elección del investigador, no cifras de los datos, salvo los precios y $e_t$.

### D-20

**Duplicados exactos de la nómina de la Ley 21.440.** La regla 5 de [D-10 · Criterio para completar o excluir](#d-10) trata los duplicados exactos solo en las planillas de la Ley 19.885. En la nómina de la Ley 21.440 cada fila trae la donataria (RUT y nombre), `Valor`, `Fecha` y, desde 2024, `Nac/Ext.`, y ningún dato del donante ([P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07)). Dos filas del mismo archivo son duplicados exactos si coinciden en todas sus columnas; copia extra es cada fila igual a una anterior del mismo archivo. Las multiplicidades y la concentración por donataria, fecha y valor están descritas en [P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07), a partir de [duplicados_ley21440.py](analisis/duplicados_ley21440.py) y [duplicados_ley21440.R](analisis/duplicados_ley21440.R) (nivel contrastado).

Norma. El portal de la Secretaría Técnica mantiene «La nómina histórica de las donaciones recibidas por las entidades inscritas en el registro, con indicación de su fecha, monto y si provienen desde el exterior.» (DL 3.063, art. 46 G N° 2, incorporado por el art. 1 de la Ley 21.440; texto de BCN vigente al 2026-10-22, cuya fechaVersion es la de publicación, 2022-04-12: [ley21440_v2026-10-22.txt, L109](normativa/textos/ley21440_v2026-10-22.txt#L109); fila [L73 de cambios.csv](normativa/cambios.csv#L73), cita comprobada por [verifica_citas.py](analisis/verifica_citas.py)). La ley rige desde el primer día del mes siguiente a su publicación (art. primero transitorio inc. 1, [L151](normativa/textos/ley21440_v2026-10-22.txt#L151)), es decir, desde el 2022-05-01, y el portal debía funcionar a más tardar tres meses después de la publicación (art. tercero transitorio, [L158](normativa/textos/ley21440_v2026-10-22.txt#L158)). El artículo nombra la fecha, el monto y la procedencia de cada donación, no al donante; en la Ley 19.885, en cambio, el donatario informa al SII la identidad del donante ([cambios.csv, L25](normativa/cambios.csv#L25)).

Sensibilidad del monto total, en pesos nominales ([datos/duplicados_ley21440_por_anio.csv, L2-L6](datos/duplicados_ley21440_por_anio.csv#L2-L6), columnas `n_filas_datos`, `n_duplicados_exactos`, `prop_filas_copias_extra`, `suma_valor`, `suma_valor_sin_duplicados` y `prop_valor_copias_extra`). «Sin duplicados» cuenta una vez cada fila distinta:

| Año | Filas | Copias extra | Monto con duplicados | Monto sin duplicados | Diferencia |
|---|---:|---:|---:|---:|---:|
| 2022 | 414 | 25 (6,0 %) | 8.073.239.854 | 8.067.574.854 | 0,07 % |
| 2023 | 14.291 | 10.302 (72,1 %) | 44.245.732.763 | 42.834.504.779 | 3,19 % |
| 2024 | 428.151 | 417.446 (97,5 %) | 66.629.506.401 | 63.722.164.149 | 4,36 % |
| 2025 | 533.650 | 510.089 (95,6 %) | 91.560.892.461 | 82.735.639.540 | 9,64 % |
| 2022-2025 | 976.506 | 937.862 (96,0 %) | 210.509.371.479 | 197.359.883.322 | 6,25 % |

El total que trae la fila superior de cada archivo es el monto con duplicados en los cuatro años ([esquema_ley21440_por_anio.csv, L2-L5](datos/esquema_ley21440_por_anio.csv#L2-L5), columna `total_cabecera`; lo exige `duplicados_ley21440.py --perfil` en `make verificar-duplicados-21440`).

Regla:

1. No se excluye ninguna fila. El monto de la Ley 21.440 de [D-12 · Variable de resultado](#d-12) es la suma de `Valor` de todas las filas, con duplicados, igual al total que publica cada archivo.
2. Todo resultado que use ese monto informa al lado el mismo resultado sin duplicados (una fila por grupo de filas idénticas del mismo archivo), con la misma agregación y el mismo período.
3. Si un diseño arma una tabla por fila, cada fila lleva su multiplicidad y la marca de copia extra, con las definiciones de la cabecera de [duplicados_ley21440.py](analisis/duplicados_ley21440.py).
4. El número de filas no se usa como número de donaciones ni de donantes. Si un diseño necesita el número de donaciones, informa las filas distintas y el total de filas como cotas inferior y superior (inferencia: valen si el único error posible es repetir una donación completa).
5. La regla se revisa si el portal publica un identificador de la donación o del donante, o si el reglamento del art. 46 A del DL 3.063 fija cómo se informa cada donación (no lo leí: sin verificar).

Razones (inferencia del investigador):

- La norma describe una nómina de donaciones con fecha, monto y procedencia, sin donante: dos donaciones distintas pueden dar la misma fila.
- Quien publica cuenta las copias: el total de cada archivo es el monto con duplicados.
- Es la regla 5 de [D-10 · Criterio para completar o excluir](#d-10) llevada a la Ley 21.440, con dos diferencias: desde 2023 las copias son la mayoría de las filas, y la norma describe la nómina como una lista de donaciones sin donante.
- Las copias pesan mucho en filas y poco en monto: excluirlas baja el monto entre 0,07 % y 4,36 % de 2022 a 2024, y 9,64 % en 2025. Se concentran en una o dos donatarias por año ([P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07)); no se calculó cuánto monto de copias tiene cada donataria. Si ese monto se reparte como el número de copias, la sensibilidad cambia sobre todo el monto de esas donatarias, y la donataria es la unidad de la comparación entre regímenes.

Evidencia débil:

- Que cada fila sea una donación es inferencia: el art. 46 G N° 2 dice qué se indica de las donaciones, no que cada una ocupe una fila, y el reglamento no se leyó.
- En 2025, el 20,5 % de las copias extra tiene fecha 2025-12-31 y un solo par donataria-fecha reúne el 18,4 % ([duplicados_ley21440_concentracion.csv, L15-L16](datos/duplicados_ley21440_concentracion.csv#L15-L16)). Si son filas que una donataria fecha al cierre del año (inferencia, sin verificar), esa fecha no es la de la donación. La regla no lo resuelve: afecta a los análisis por fecha, no al monto anual.
- Los valores con más copias son 30.000 pesos en 2022, 120.000 en 2023 y 5.000 en 2024 y 2025 ([duplicados_ley21440_concentracion.csv](datos/duplicados_ley21440_concentracion.csv), líneas 5, 9, 13 y 17, columna `categoria_top1_copias`). 120.000 es doce veces 10.000: si algunas donatarias informan totales anuales por donante (inferencia, sin verificar), una fila es un donante-año y no una donación. Eso también es compatible con conservar las copias, pero entonces las cotas del punto 4 no valen para el número de donaciones.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador en el [issue 16](https://github.com/ingUANDES/donationstax/issues/16); no excluye nada de los datos. El código lo reservó el usuario en la conversación del 2026-10-27 (no verificable desde el repo).

### D-21

**Texto completo de las estructuras que el XML de BCN corta.** El XML de BCN (`obtxml?opt=7`) trae incompleto el texto de algunas estructuras; la evidencia y la regla con que se detecta están en [Textos cortados del XML de BCN, en P-01 · Inventario normativo](BLUEPRINT.md#textos-cortados-del-xml-de-bcn). Se propone:

1. Una estructura está cortada si su texto en el XML termina en un carácter no ASCII (regla `cortada` de [bcn_versiones.py](analisis/bcn_versiones.py)). La cabecera de cada texto las lista y [verifica_citas.py](analisis/verifica_citas.py) las cuenta.
2. Su texto completo es el del JSON del visor de LeyChile (`servicios-leychile.bcn.cl`, `get_norma_json`) para la misma `idVersion`, sin las notas de BCN, y solo si el texto del XML es su prefijo. Va en `<texto>_visor.txt`, aparte del texto del XML, y verifica_citas.py comprueba el prefijo.
3. Una fila de [cambios.csv](normativa/cambios.csv) puede quedar «verificada» con una cita de un `_visor.txt`: su `texto_archivo` es ese archivo y su `fuente_url`, la del JSON.
4. Un límite de versión de bcn_versiones.py que solo marca la aparición o la desaparición de un corte no es un cambio de texto y no da una fecha de vigencia ([issue 37](https://github.com/ingUANDES/donationstax/issues/37)).

Alternativas: volver a bajar el XML, que el 2026-10-26 trajo completas 4 de las 15 estructuras cortadas, pero no las otras 11; o el Diario Oficial, cuya edición electrónica no trae la del 2012-09-27 ([Textos cortados del XML de BCN, en P-01 · Inventario normativo](BLUEPRINT.md#textos-cortados-del-xml-de-bcn)) y que para leyes más recientes podría servir de contraste independiente (sin verificar).

Evidencia débil: lo que sigue al corte depende solo del visor. `make verificar` comprueba que el visor coincide con el XML hasta el corte, no después. La dirección del JSON sale del código del visor (`main.*.bundle.js` de www.bcn.cl/leychile, consulta del 2026-10-26); si BCN la documenta como servicio está sin verificar.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador en el [issue 28](https://github.com/ingUANDES/donationstax/issues/28).
