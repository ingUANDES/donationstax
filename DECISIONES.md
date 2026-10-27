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

Regla propuesta:

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

Estado: Propuesta. La plantea el investigador en el [issue 16](https://github.com/ingUANDES/donationstax/issues/16); no rige hasta que una persona la apruebe, y mientras tanto no se excluye nada de los datos. El código lo reservó el usuario en la conversación del 2026-10-27 (no verificable desde el repo).
