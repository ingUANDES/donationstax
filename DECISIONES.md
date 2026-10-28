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
4. [armonizacion_anios_donacion.csv](https://github.com/ingUANDES/donationstax/blob/90bb98c2cce8a90afb240f74daee3d657c61dcfa/datos/armonizacion_anios_donacion.csv) se retira, porque los dos años coinciden por construcción. Lo pide el [issue 26](https://github.com/ingUANDES/donationstax/issues/26).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-26 (no verificable desde el repo): «aceptar propuesta si complementar que la regla 4 del d-10 solo vale si hay info de fecha sino se usa la info del archivo de origen». La plantea el investigador a partir de lo que dijo el usuario al aprobar [D-13 · Casos que el criterio para completar o excluir no cubre](#d-13). Cómo la leo (inferencia): la propuesta rige tal cual, y el complemento fija lo que pasa sin fecha, que es usar el archivo. Si se quiso que la fecha siga mandando cuando cae en otro año, basta cambiar la columna del año, y por eso el año de la fecha se conserva. [armonizar.py](analisis/armonizar.py) y [armonizar.R](analisis/armonizar.R) la aplican desde el [issue 20](https://github.com/ingUANDES/donationstax/issues/20): el año de la fecha queda en la columna `anio_fecha` y la marca, en `fecha_otro_anio`. Las cuatro aclaraciones: usuario, conversación de Claude Code del 2026-10-26 (no verificable desde el repo): «Apruebo tus 4 recomendaciones». El cambio de ruta del enlace de la aclaración 4, hecho por el [PR 46](https://github.com/ingUANDES/donationstax/pull/46) (ahora apunta a la tabla en el último commit que la trae), lo aprobó el usuario en la conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «aprueba D-23, D-26 y D-14 numeral 4». El texto de la aclaración no cambió.

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

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador el 2026-10-27, antes de correr las simulaciones ([issue 22](https://github.com/ingUANDES/donationstax/issues/22)). Los parámetros del caso son elección del investigador, no cifras de los datos, salvo los precios. Aprobarla no cambia el resultado de la simulación: las cinco reglas de sensibilidad que incumple están registradas en [sim_hallazgos.csv](analisis/fixtures/sim_hallazgos.csv) y la discusión del rediseño está en el [issue 34](https://github.com/ingUANDES/donationstax/issues/34); cambiar esta tolerancia sería una decisión nueva. Desde el 2026-10-28, sus reglas de una sola extracción las reemplaza [D-24 · Monte Carlo del bunching con regla de selección fijada antes de correr](#d-24) ([D-19 · Método de Monte Carlo para las simulaciones de bunching y de estudio de eventos](#d-19), punto 11).

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

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Acepto propuestas en las decisiones D-15 a D-18, D-20 y D-21». La planteó el investigador el 2026-10-27, antes de correr las simulaciones ([issue 22](https://github.com/ingUANDES/donationstax/issues/22)). Los parámetros del caso son elección del investigador, no cifras de los datos, salvo $e_t$. Una fila por donante-año es una simplificación (inferencia): con varias filas por año, el error también partiría el monto anual. Aprobarla no cambia el resultado de la simulación: las dos reglas del coeficiente de 2012 que incumple están registradas en [sim_hallazgos.csv](analisis/fixtures/sim_hallazgos.csv) y la discusión del rediseño está en el [issue 34](https://github.com/ingUANDES/donationstax/issues/34); cambiar esta tolerancia sería una decisión nueva. Desde el 2026-10-28, sus reglas de una sola extracción las reemplaza [D-25 · Monte Carlo del estudio de eventos, con cobertura y prueba de tendencias previas](#d-25) ([D-19 · Método de Monte Carlo para las simulaciones de bunching y de estudio de eventos](#d-19), punto 11).

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

### D-19

**Método de Monte Carlo para las simulaciones de bunching y de estudio de eventos.** Cambia el criterio de juicio de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16) y [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17), que juzgan una sola extracción, por el desempeño en muchas repeticiones. No toca [D-15 · Tolerancia entre Python y R para las simulaciones](#d-15) ni [D-18 · Tolerancias de la simulación de variables instrumentales](#d-18). Lo aplican [D-24 · Monte Carlo del bunching con regla de selección fijada antes de correr](#d-24) y [D-25 · Monte Carlo del estudio de eventos, con cobertura y prueba de tendencias previas](#d-25). Motivo: los hallazgos del [issue 34](https://github.com/ingUANDES/donationstax/issues/34), en que una sola semilla pasa o falla una regla absoluta por azar.

1. Repeticiones. Cada script fija una semilla base $S_0$ y la lista larga de semillas $S_0 + r$, con $r = 0, \dots, R_L - 1$. Cada repetición inicia con su semilla el generador de [azar.py](analisis/azar.py) y [azar.R](analisis/azar.R), así que cualquier repetición se reproduce sola. La lista corta son las primeras $R_C$ semillas de la larga. `make verificar` corre la corta y `make verificar-mc` la larga.
2. Número de repeticiones, por el error de Monte Carlo (EMC): $n_{sim} = \mathrm{Var}/\mathrm{EMC}^2$ (Morris, White y Crowther 2019, *Statistics in Medicine* 38(11):2074–2102, sección 5.3, ec. 1, DOI [10.1002/sim.8086](https://doi.org/10.1002/sim.8086); comprobado en la versión aceptada, arXiv:1712.03198v3, no en la paginación de la revista). Con la desviación de 0,054 de $\hat\varepsilon$ con órdenes 5 y 6 (comprobación con 20 semillas del [issue 34](https://github.com/ingUANDES/donationstax/issues/34), sin script, sin verificar), un EMC del sesgo de 0,004 pide 183 repeticiones. Se fija $R_L = 200$, que da $0{,}054/\sqrt{200} = 0{,}0038$. Con $R_L = 200$, el EMC de una cobertura de 95 % o de una tasa de rechazo de 5 % es $\sqrt{0{,}95 \cdot 0{,}05/200} = 0{,}0154$. Se fija $R_C = 10$: la lista corta comprueba la implementación, el contraste entre Python y R y la copia versionada. Su EMC es $\sqrt{20} \approx 4{,}5$ veces el de la larga y no decide el nivel de ningún resultado.
3. Medidas de desempeño, de la tabla 6 de Morris et al. (2019). Para una estimación $\hat\theta_r$ con objetivo $\theta_r$, fijo o calculado en cada repetición, sea $d_r = \hat\theta_r - \theta_r$. El sesgo es $\bar d$. El error estándar empírico es $EE = \sqrt{\sum_r (d_r - \bar d)^2/(R - 1)}$. El EMC del sesgo es $EE/\sqrt{R}$. La cobertura de un intervalo y la tasa de rechazo de una prueba son proporciones de repeticiones. Su EMC se toma en el valor nominal $p_0$, $\sqrt{p_0(1 - p_0)/R}$. Morris et al. lo calculan con la proporción estimada; usar la nominal es elección del investigador, porque la regla contrasta la hipótesis de que el intervalo o la prueba están calibrados.
4. Reglas, de dos tipos. Las de (a) a (d) comprueban la ausencia de un problema y fallan solo si la simulación lo muestra con un margen de tres EMC:
   - (a) Sesgo: $|\bar d| \le 0{,}25\,EE + 3\,EMC$. Con un sesgo de $0{,}25\,EE$, un intervalo normal al 95 % de ancho correcto cubre $\Phi(1{,}71) - \Phi(-2{,}21) = 94{,}3\ \%$ (cálculo del investigador; Morris et al., figura 1, muestran cómo el sesgo reduce la cobertura). El umbral de 0,25 es elección del investigador.
   - (b) Cobertura de un intervalo al 95 %: $|\hat c - 0{,}95| \le 3\sqrt{0{,}95 \cdot 0{,}05/R}$.
   - (c) Tamaño de una prueba al 5 %: $|\hat t - 0{,}05| \le 3\sqrt{0{,}05 \cdot 0{,}95/R}$. Es el criterio de Bertrand, Duflo y Mullainathan: con errores estándar correctos, la nula de efecto cero se rechaza el 5 % de las veces (versión de trabajo NBER w8841, marzo de 2002, sección 3.1, comprobada; la versión publicada, *Quarterly Journal of Economics* 2004, 119(1):249–275, DOI [10.1162/003355304772839588](https://doi.org/10.1162/003355304772839588), no se leyó).
   - (d) Potencia de una prueba al 5 %: $\hat t \ge 0{,}80 - 3\sqrt{0{,}80 \cdot 0{,}20/R}$. El 80 % es el valor de referencia que usa Roth (2022, *AER: Insights* 4(3):305–322, p. 309, DOI [10.1257/aeri.20210236](https://doi.org/10.1257/aeri.20210236); comprobado en el texto publicado).

   La regla (e) comprueba que un sesgo conocido existe, y exige mostrarlo con el mismo margen: $\bar{\hat\theta} + 3\,EMC \le u$ si el sesgo debe dejar la media bajo $u$, y $\bar{\hat\theta} - 3\,EMC \ge u$ si debe dejarla sobre $u$, con $EMC = EE/\sqrt{R}$.
5. Falsas alarmas (inferencia, con normalidad aproximada): si el desempeño verdadero está justo en el umbral, una regla de tipo (a) o (d) falla por azar con probabilidad cercana a 0,13 %, y una de tipo (b) o (c) con 0,27 %. Con unas 70 reglas, la probabilidad de alguna falla espuria es menor que 20 % en ese caso extremo, y mucho menor si el desempeño está lejos del umbral.
6. El bootstrap va solo dentro de cada repetición. Su intervalo se juzga por la cobertura contra el valor verdadero; la validación del diseño es el Monte Carlo.
7. Las reglas de selección de especificación se fijan antes de correr y no se eligen por su desempeño en esta simulación. Un Monte Carlo diseñado por el investigador selecciona bien entre estimadores solo en condiciones restrictivas, y la recomendación es usar varios estimadores y compararlos (Advani, Kitagawa y Słoczyński 2019, *Journal of Applied Econometrics* 34(6):893–910, DOI [10.1002/jae.2724](https://doi.org/10.1002/jae.2724); resumen y conclusión, comprobados en arXiv:1809.09527v2). Las especificaciones alternativas se informan como sensibilidad, sin regla.
8. Python y R corren las mismas semillas, y cada cifra de salida se contrasta con la tolerancia de [D-15 · Tolerancia entre Python y R para las simulaciones](#d-15), en las dos listas.
9. Las reglas incumplidas se registran en [sim_hallazgos.csv](analisis/fixtures/sim_hallazgos.csv) con las simulaciones `bunching_mc_corta`, `bunching_mc_larga`, `eventos_mc_corta` y `eventos_mc_larga`. Como en [P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#p-05), el script falla con `AssertionError` si aparece una falla no registrada o si una registrada deja de fallar.
10. Nivel ([D-05 · Niveles de resultado](#d-05)): una simulación sube a «verificada» solo con la lista larga, si Python y R coinciden, ninguna regla queda incumplida y el script falla con `AssertionError` cuando alguna no se cumple. Con hallazgos queda «contrastado».
11. [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16) y [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17) siguen aprobadas, y sus scripts siguen en `make verificar` sin cambios. Si una persona aprueba esta decisión, [D-24 · Monte Carlo del bunching con regla de selección fijada antes de correr](#d-24) y [D-25 · Monte Carlo del estudio de eventos, con cobertura y prueba de tendencias previas](#d-25), las reglas de una sola extracción quedan reemplazadas; retirar esos scripts es trabajo aparte.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-28 (no verificable desde el repo): «Apruebo D-19, D-24 y D-25». La planteó el investigador el 2026-10-28 ([issue 34](https://github.com/ingUANDES/donationstax/issues/34)), antes de correr ninguna repetición; la fijó el commit que la agrega, anterior al de los resultados. Concreta la propuesta que el usuario aceptó en la conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «Para el 34 acepto la propuesta». El código lo reservó el usuario. $R_L$, $R_C$, el umbral de 0,25 y el margen de tres EMC son elección del investigador. Con esta aprobación quedan reemplazadas las reglas de una sola extracción de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16) y [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17) (punto 11); sus scripts siguen en `make verificar` hasta que se retiren, trabajo aparte.

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

### D-23

**Precio fiscal a nivel del contribuyente que dona.** El precio de [P-04 · Precio fiscal por régimen](BLUEPRINT.md#p-04) se calcula para quien hace la donación y usa el beneficio. Para una empresa, en primera categoría; para una persona, en el global complementario o en el impuesto único de segunda categoría (art. 1 bis de la Ley 19.885). No se calcula cómo el crédito por donaciones y el menor impuesto de primera categoría pasan a los impuestos finales del dueño de la empresa (global complementario o adicional) en los regímenes del art. 14 de la LIR: letras A y B de 2017 a 2019, y letras A y D desde 2020.

Límites que quedan explícitos:

1. Para una empresa, P es el costo de donar para la empresa, no para su dueño. Con integración total o parcial, el impuesto de primera categoría es crédito contra los impuestos finales del dueño, y una donación que lo cambia puede cambiar también lo que paga el dueño. El costo para el dueño puede entonces ser distinto de P, en un sentido y una magnitud que no se calcularon (inferencia; las reglas del art. 14 no se leyeron).
2. No tienen caso los regímenes en que la empresa no paga impuesto de primera categoría: el Pro Pyme transparente (art. 14 letra D N° 8 de la LIR, desde 2020) y el art. 14 ter (hasta 2019). Su precio depende de los impuestos finales del dueño, que quedan fuera.
3. Un diseño que use estos precios mide la respuesta al precio de la empresa. Si quien decide la donación es el dueño y su costo difiere, el precio tiene error de medición (inferencia).
4. Con esta regla, el paso a los impuestos del dueño es un límite del alcance y no una lectura pendiente de los casos de empresa, para efectos de la aclaración de [D-05 · Niveles de resultado](#d-05) sobre los cálculos deterministas.
5. Se revisa si un diseño de [P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#p-05) necesita el precio del dueño, por ejemplo para comparar empresas con personas.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «aprueba D-23, D-26 y D-14 numeral 4», después de leer el texto de esta decisión. La planteó el investigador en el [issue 29](https://github.com/ingUANDES/donationstax/issues/29), punto 5, a partir de una indicación previa del usuario en la misma conversación: «ok con precio a nivel empresa» (escribió «39» y confirmó que quería decir 29). El código lo reservó el usuario en esa conversación.

### D-24

**Monte Carlo del bunching con regla de selección fijada antes de correr.** Reemplazaría las tolerancias de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16) si se aprueba. Usa el método de [D-19 · Método de Monte Carlo para las simulaciones de bunching y de estudio de eventos](#d-19), con $R_L = 200$, $R_C = 10$ y semilla base $S_0 = 34000000$.

Procesos generadores. Los cuatro tienen la estructura de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16): precios $P_0$ y $P_1$ de los casos `e2012_hasta_1000` y `e2012_exceso_sin_fondo`, $\varepsilon = 0{,}5$, el 20 % de los donantes aporta al Fondo y no enfrenta el quiebre, y los que se agolpan quedan uniformes en $[k - 0{,}04,\ k)$, con $k = \ln 1000$. Lo que cambia es la densidad contrafactual $h$ de $z^\ast$, el tamaño o los montos:

1. Base: el de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16), con 300.000 montos y $z^\ast \sim N(k - 0{,}5,\ 1)$. En $k$, $h'/h = -0{,}5$ y $h''/h = -0{,}75$.
2. Curvatura: $z^\ast \sim N(k - 0{,}25,\ 0{,}5^2)$. En $k$, $h'/h = -1$ y $h''/h = -3$, cuatro veces la curvatura relativa de la base.
3. Ruido: el de la base con 75.000 montos. La desviación relativa de cada conteo se duplica.
4. Montos redondos: el de la base, y cada donante que no se agolpa redondea, con probabilidad 0,05, su monto en pesos al múltiplo más cercano de 5.000.000; si el redondeo da 0, no redondea. El monto en pesos es $e^z$ por la UTM de un mes de 2012 elegido al azar con igual probabilidad ([utm_mensual.csv, L50-L61](datos/utm_mensual.csv#L50-L61)), y vuelve a UTM con la UTM del mismo mes ([D-26 · UTM que se aplica a cada donación](#d-26), punto 1). Con la UTM de enero (39.138, L50) y la de diciembre (40.206, L61), 40 millones de pesos son 1.022,0 y 994,9 UTM: ese pico cae entre $k - 0{,}005$ y $k + 0{,}022$, a ambos lados del umbral (cálculo del investigador). Inferencia previa a correr: este proceso probablemente sesga $\hat B$ hacia arriba.

En cada repetición, las extracciones van en este orden: normales para $z^\ast$, uniformes para el Fondo, para la posición del agolpe, para el mes y para el redondeo, en los cuatro procesos (los tres primeros no usan las dos últimas), y después las del bootstrap.

Regla de selección de la ventana y del orden, adaptada de Bosch, Dekker y Strohmaier (2020, *International Tax and Public Finance* 27:951–979, pp. 955–957 y apéndice 1, pp. 973–974, DOI [10.1007/s10797-020-09590-w](https://doi.org/10.1007/s10797-020-09590-w); comprobado en el texto). Tramos de 0,02 sobre $[k - 1,\ k + 1)$, polinomio a cada lado de $k$ y fórmula de $\hat\varepsilon$ de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16):

1. El tramo del umbral es $j_0 = [k - 0{,}02,\ k)$, porque los que se agolpan quedan bajo $k$.
2. Para cada región excluida, de los tramos $j_0 - x_-$ a $j_0 + x_+$ con $x_-, x_+ \in \lbrace 0, \dots, 10\rbrace$ (121 regiones):
   - (a) Para cada $q \in \lbrace 1, \dots, 7\rbrace$, se ajusta por MCO el polinomio de orden $q$ a cada lado de $k$ con los tramos que quedan fuera de la región. Se elige el $q$ de menor $BIC = n \ln(RSS/n) + 2(q + 1)\ln n$, con $n$ el número de tramos usados. Bosch et al. eligen el orden por el BIC (p. 957).
   - (b) Banda superior al 95 % de la predicción media: $\hat N_j + 1{,}96\,\hat s\sqrt{x_j'(X'X)^{-1}x_j}$, con $\hat s^2 = RSS/(n - 2(q + 1))$. Bosch et al. piden el intervalo de confianza «using standard procedures» (p. 974) sin decir si es el de la media o el de una predicción; usar el de la media es lectura del investigador (inferencia).
   - (c) Si el conteo $N_{j_0}$ no supera la banda, la ventana de la región es $\lbrace j_0\rbrace$. Si la supera, la ventana es la sucesión de tramos contiguos que contiene a $j_0$ y cuyo conteo supera la banda.
3. Las cotas $l^\ast$ y $u^\ast$ son las medianas de las 121 cotas inferiores y superiores. Bosch et al. aceptan la moda o la mediana (p. 974); con 121 valores, la mediana es un valor observado y no tiene empates.
4. Con la ventana $[l^\ast, u^\ast]$ excluida, se elige $q^\ast$ por el BIC como en 2(a). Se estima con una dummy por tramo de la ventana; $\hat B$ es su suma y $\hat\varepsilon$ sale de la fórmula de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16).
5. Error estándar: el bootstrap de residuos de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16), con 200 réplicas y $q^\ast$, $l^\ast$ y $u^\ast$ fijos. No incluye la incertidumbre de la selección; la cobertura mide cuánto pesa. Intervalo: $\hat\varepsilon \pm 1{,}96\,\widehat{ee}$.

Diferencias con Bosch et al.: un polinomio a cada lado de $k$, como en [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16), porque en un quiebre con desplazamiento constante en logaritmo la densidad observada salta en $k$ (inferencia); tramos fijos de 0,02 en vez de la regla de Freedman–Diaconis; región excluida hasta 10 tramos por lado en vez de 15.

Sensibilidad informada, sin regla: las 24 combinaciones de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16), punto 2 (orden 3 a 6; ventana desde $k - 0{,}06$, $k - 0{,}08$ o $k - 0{,}10$, y hasta $k$ o $k + 0{,}02$), en los cuatro procesos. Se informan la media y el $EE$ de $\hat\varepsilon$. El orden 3 y las ventanas anchas quedan aquí.

Tolerancias (tipos de [D-19 · Método de Monte Carlo para las simulaciones de bunching y de estudio de eventos](#d-19), punto 4), las mismas en las dos listas:

1. En cada proceso, con la muestra sin los donantes del Fondo: sesgo de $\hat\varepsilon$ respecto de $\varepsilon$, tipo (a); sesgo de $\hat B - B_r$, con $B_r$ el número simulado que se agolpa en la repetición $r$, tipo (a); cobertura del intervalo de $\hat\varepsilon$, tipo (b); y error estándar finito y positivo en todas las repeticiones. Son 16 reglas.
2. En el proceso base, con todos los donantes y la misma regla de selección aplicada a sus conteos: sesgo de $\hat B - B_r$, tipo (a); detección del sesgo de la fórmula del quiebre simple, $\bar{\hat\varepsilon} + 3\,EMC \le 0{,}9\,\varepsilon$, tipo (e); y sesgo respecto de $0{,}8\,\varepsilon$, tipo (a). El valor $0{,}8\,\varepsilon$ es inferencia, como en [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16), punto 3. Son 3 reglas.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-28 (no verificable desde el repo): «Apruebo D-19, D-24 y D-25». La planteó el investigador el 2026-10-28 ([issue 34](https://github.com/ingUANDES/donationstax/issues/34)), antes de correr ninguna repetición; la fijó el commit que la agrega, anterior al de los resultados. Concreta el punto 2 de la propuesta que el usuario aceptó el 2026-10-27 (conversación de Claude Code, no verificable desde el repo): «Para el 34 acepto la propuesta». El código lo reservó el usuario. Los procesos 2 a 4, sus parámetros y las adaptaciones de la regla son elección del investigador, no cifras de los datos, salvo los precios y la UTM. Con esta aprobación reemplaza las tolerancias de [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16); los hallazgos del bunching siguen registrados en [sim_hallazgos.csv](analisis/fixtures/sim_hallazgos.csv).

### D-25

**Monte Carlo del estudio de eventos, con cobertura y prueba de tendencias previas.** Reemplazaría las tolerancias de [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17) si se aprueba. Usa el método de [D-19 · Método de Monte Carlo para las simulaciones de bunching y de estudio de eventos](#d-19), con $R_L = 200$, $R_C = 10$ y semilla base $S_0 = 34100000$. Con la desviación de 0,0082 de los coeficientes del caso de nivel previo (20 semillas del [issue 34](https://github.com/ingUANDES/donationstax/issues/34), sin script, sin verificar), $R_L = 200$ da un EMC del sesgo de 0,0006.

Los casos, el proceso generador, el orden de las extracciones y los estimadores son los de [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17), casos 1 a 4, sin cambios. Se agregan:

1. Varianza robusta por conglomerado, con el identificador que usa cada caso (el del RUT, o el del nombre en los casos con error por RUT): $\hat V = \frac{G}{G - 1}\frac{n - 1}{n - K}(\tilde X'\tilde X)^{-1}\big(\sum_g \tilde X_g'\hat u_g\hat u_g'\tilde X_g\big)(\tilde X'\tilde X)^{-1}$, con $\tilde X$ las desviaciones de la media de cada identificador, $G$ identificadores, $n$ filas y $K$ columnas de $\tilde X$. Es el estimador que Bertrand et al. llaman de matriz de varianzas y covarianzas arbitraria dentro de cada grupo, consistente cuando el número de grupos tiende a infinito (versión NBER w8841, sección 4.4, comprobada).
2. Intervalo de cada coeficiente: $\hat\gamma_t \pm 1{,}96\sqrt{\hat V_{tt}}$.
3. Prueba de tendencias previas: Wald conjunta de $\gamma_{2012} = \gamma_{2013} = 0$, $W = \hat\gamma_{pre}'\hat V_{pre}^{-1}\hat\gamma_{pre}$, que rechaza si $W > -2\ln 0{,}05 = 5{,}9915$, el cuantil 0,95 de $\chi^2_2$. Se informa también, sin regla, la tasa con que rechaza el criterio que Roth (2022, p. 309) describe en la práctica publicada: algún coeficiente previo significativo por sí solo al 95 %.

Tolerancias (tipos de [D-19 · Método de Monte Carlo para las simulaciones de bunching y de estudio de eventos](#d-19), punto 4), las mismas en las dos listas:

1. Exógeno (caso 1) y exógeno con error por RUT (caso 4): en cada año, sesgo de $\hat\gamma_t$ respecto de $\beta 1[t \ge 2015]$, tipo (a), y cobertura de su intervalo, tipo (b); y tamaño de la prueba de Wald, tipo (c). Son 15 reglas por caso.
2. Nivel previo (caso 2) y nivel previo con error por RUT (caso 4): en cada año, sesgo de $\hat\gamma_t$ respecto de $\beta 1[t \ge 2015] + b$, con el $b$ analítico de [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17), tipo (a); y potencia de la prueba de Wald, tipo (d), que mide cuánto detecta la prueba la reversión a la media. Son 8 reglas por caso, más $|b| \ge 0{,}06$ (analítico, una regla). Se informa sin regla la cobertura de cada intervalo respecto de $\beta 1[t \ge 2015]$. Seleccionar unidades por valores extremos de una variable que cambia en el tiempo produce reversión a la media (Daw y Hatfield 2018, *Health Services Research* 53(6):4138–4156, sección «Regression to the Mean and Bias in Matched Difference-in-Differences Analysis», DOI [10.1111/1475-6773.12993](https://doi.org/10.1111/1475-6773.12993); comprobado en PMC6232412).
3. Control de 200 a 900 UTM (caso 3): sesgo de $\hat\delta$ respecto de $\beta - \pi_{c,r}\beta_c$, con $\pi_{c,r}$ la proporción simulada en la repetición $r$, tipo (a); y detección de la contaminación, $\overline{\pi_c\beta_c} - 3\,EMC \ge 0{,}06$, tipo (e). Se informa sin regla la cobertura del intervalo de $\hat\delta$ respecto de $\beta$ y de $\beta - \pi_{c,r}\beta_c$. Son 2 reglas.
4. Error por RUT: sesgo de la proporción de donantes con el mismo identificador en 2014 y en 2015 respecto de $(1 - e_{2014})(1 - e_{2015})$, tipo (a). Una regla.

En total, 50 reglas.

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-28 (no verificable desde el repo): «Apruebo D-19, D-24 y D-25». La planteó el investigador el 2026-10-28 ([issue 34](https://github.com/ingUANDES/donationstax/issues/34)), antes de correr ninguna repetición; la fijó el commit que la agrega, anterior al de los resultados. Concreta los puntos 1, 3 y 4 de la propuesta que el usuario aceptó el 2026-10-27 (conversación de Claude Code, no verificable desde el repo): «Para el 34 acepto la propuesta». El código lo reservó el usuario. La varianza por conglomerado, la prueba de Wald y sus umbrales son elección del investigador. Con esta aprobación reemplaza las tolerancias de [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17).

### D-26

**UTM que se aplica a cada donación.** Los diseños de [P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#p-05) miden el monto en UTM: el quiebre de 1.000 UTM del bunching ([D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16)) y el logaritmo del monto en UTM de los estudios de eventos y de las variables instrumentales ([D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](#d-17), [D-18 · Tolerancias de la simulación de variables instrumentales](#d-18)). Las planillas traen pesos nominales ([D-12 · Variable de resultado](#d-12)). La serie mensual de la UTM de 2008 a 2025, del SII, está descrita en [Serie de la UTM, en P-05 · Simulaciones de efecto conocido](BLUEPRINT.md#serie-de-la-utm) ([issue 33](https://github.com/ingUANDES/donationstax/issues/33)).

Norma e interpretación administrativa (citas comprobadas por [verifica_citas.py](analisis/verifica_citas.py)):

- La Ley 19.885 fija el umbral en 1.000 UTM por donante, institución y año desde 2012-02-08 ([cambios.csv, L19](normativa/cambios.csv#L19), [L22](normativa/cambios.csv#L22)), y no dice con qué valor de la UTM se convierte cada donación (inferencia: no lo dice el texto de [ley19885_v2012-02-08.txt](normativa/textos/ley19885_v2012-02-08.txt); el reglamento no se leyó).
- La Circular N° 71 del SII (interpretación administrativa, no ley), N° II letra D-1 a-1, sobre el texto de la Ley 20.316: «Para medir dicho límite, el monto de cada donación debe expresarse en UTM, dividiendo el monto de la donación por el valor de dicha Unidad vigente en el mes en que ésta se realice.» ([cambios.csv, L84](normativa/cambios.csv#L84)). La Circular N° 49, del 2012-11-05, reemplaza esa letra D-1 ([circular_sii_49_2012.txt, L137-L139](normativa/textos/circular_sii_49_2012.txt#L137-L139)), y su texto nuevo no repite la regla ni da otra (búsqueda en el texto del repo; [issue 42](https://github.com/ingUANDES/donationstax/issues/42)).
- Para el tope de 14.000 UTM del crédito, la misma Circular 71 usa «el valor de la UTM vigente a la fecha del balance» ([cambios.csv, L85](normativa/cambios.csv#L85)). La Ley 21.440 fija sus topes «considerando el valor de la unidad tributaria mensual del mes de cierre del ejercicio respectivo» ([cambios.csv, L37](normativa/cambios.csv#L37), [L38](normativa/cambios.csv#L38)).
- El art. 1 N° 13 reajusta la parte de la donación con crédito «en la forma establecida para los pagos provisionales obligatorios», desde el desembolso ([cambios.csv, L83](normativa/cambios.csv#L83)); la Circular 49 lo lee como la variación del IPC entre el mes anterior al desembolso y el mes anterior al del balance ([cambios.csv, L86](normativa/cambios.csv#L86)). Es un reajuste del crédito, no una regla para medir el umbral (inferencia).

Regla:

1. El monto en UTM de una donación es su monto en pesos dividido por la UTM del mes de la donación (`mes_donacion` de la armonización, según [D-14 · Año de la donación cuando la fecha cae en otro año](#d-14)) en el año de la donación, que es el del archivo.
2. Si el mes es desconocido, se usa la UTM de diciembre del año del archivo.
3. Las sumas por donante, institución y año (la unidad del umbral desde 2012) suman donaciones ya convertidas, como pide la Circular 71 para el límite: «La suma de todas las donaciones efectuadas en el ejercicio, convertidas a dicha unidad» ([cambios.csv, L84](normativa/cambios.csv#L84)).
4. Sensibilidad: todo resultado en UTM se informa también con la UTM de diciembre del año del archivo para todas las filas. Cambiar de criterio es cambiar de columna.
5. Los montos no se reajustan por IPC antes de convertirlos.
6. En la nómina de la Ley 21.440 vale la misma regla; su fecha es siempre una fecha de Excel del año del archivo ([P-07 · Datos del régimen de la Ley 21.440](BLUEPRINT.md#p-07)), así que el mes siempre se conoce.

Magnitud de la sensibilidad ([datos/utm_por_anio.csv](datos/utm_por_anio.csv), nivel contrastado). Para una donación de mes $m$ conocido, pasar de la regla al criterio de diciembre multiplica su monto en UTM por $\text{UTM}_m/\text{UTM}_{dic}$. Salvo en 2009, la UTM de diciembre es la máxima del año (columna `razon_dic_sobre_max` igual a 1), así que el logaritmo del monto baja, y a lo más en `log_dic_sobre_min`:

- En la ventana de 2012 a 2025, $\text{UTM}_{dic}/\min_m \text{UTM}_m$ va de 1,0181 en 2017 ([L11](datos/utm_por_anio.csv#L11)) a 1,1233 en 2022 ([L16](datos/utm_por_anio.csv#L16)), con un logaritmo de 0,0179 a 0,1163; el segundo valor más alto es 1,0626, en 2021 ([L15](datos/utm_por_anio.csv#L15)).
- La ventana excluida de la especificación principal del bunching mide 0,08 en logaritmo ([D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](#d-16)). En 2022, el criterio de diciembre correría parte de la masa agolpada fuera de esa ventana (inferencia).
- Con efectos fijos de año, en los estudios de eventos y en las variables instrumentales solo cuenta cómo se reparten las donaciones entre los meses de cada año (inferencia).
- Filas con el mes desconocido, a las que se aplica el punto 2 ([armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv), columna `n_mes_desconocido`): 4.558 de 486.537 en 2018 ([L12](datos/armonizacion_por_anio.csv#L12)) y 1.104 de 551.640 en 2015 ([L9](datos/armonizacion_por_anio.csv#L9)); en los demás años de 2012 a 2025, 502 o menos (2019, [L13](datos/armonizacion_por_anio.csv#L13)).

Razones (inferencia del investigador):

- La regla coincide con la única instrucción del SII que el repo tiene sobre cómo medir el límite de 1.000 UTM, la de la Circular 71.
- Diciembre es el mes de cierre del ejercicio de quienes cierran el año calendario, y es la UTM que la Circular 71 usa para el tope de 14.000 UTM y la que la Ley 21.440 usa para sus topes. Que todos los donantes cierren en diciembre está sin verificar.
- Las dos convenciones se acercan a un mismo valor: según la serie, la UTM se reajusta con el IPC del segundo mes anterior ([Serie de la UTM](BLUEPRINT.md#serie-de-la-utm)), así que $\text{UTM}_{dic}/\text{UTM}_m$ es la variación del IPC de $m-2$ a octubre; el reajuste del N° 13 es la variación de $m-1$ a noviembre. El monto reajustado dividido por la UTM de diciembre se parece al monto nominal dividido por la UTM de su mes, salvo por ese desfase de un mes.

Alternativas: la UTM de diciembre para todas las filas (es la sensibilidad del punto 4); la UTM promedio del año para el mes desconocido; reajustar cada monto por IPC hasta noviembre y dividir por la UTM de diciembre, que se acerca al crédito como lo calcula el SII (inferencia) y requiere un índice de IPC armado con la variación mensual de [datos/utm_mensual.csv](datos/utm_mensual.csv).

Evidencia débil:

- La instrucción de la Circular 71 interpreta el texto de 2010 a 2012, y la Circular 49 la reemplazó sin repetirla. Para las donaciones desde la Ley 20.565, que son las de la ventana empírica, no hay en el repo fuente administrativa sobre qué UTM mide el umbral (sin verificar; [issue 42](https://github.com/ingUANDES/donationstax/issues/42)).
- El mes es el de la fecha de la planilla. Algunas fechas podrían ser del certificado y no de la donación ([D-14 · Año de la donación cuando la fecha cae en otro año](#d-14), inferencia sin verificar).
- Que la UTM se reajuste con el IPC del segundo mes anterior sale de la serie, no de una norma leída: el Código Tributario no está en `normativa/textos/` (sin verificar).

Estado: Aprobada. Usuario, conversación de Claude Code del 2026-10-27 (no verificable desde el repo): «aprueba D-23, D-26 y D-14 numeral 4», después de leer el texto de esta decisión. La regla es la sugerencia por defecto que dio el usuario al encargar el issue 33 (misma conversación): «la UTM del mes de la donación si se conoce y la de diciembre del año del archivo si no, con sensibilidad entre ambas». El código lo reservó el usuario en esa conversación. Las citas, la magnitud y las razones son del investigador.

### D-27

**Texto que rige en las estructuras en que el XML de BCN trae U+FFFD.** En cuatro estructuras de los textos del repo, el XML de BCN (`obtxml?opt=7`) trae el carácter de reemplazo U+FFFD y el JSON del visor de LeyChile de la misma `idVersion` trae una letra. Los dos textos tienen el mismo largo y solo difieren en esa posición (clase `fffd` de `bcn_versiones.py contrastar`; corrida en [Textos cortados del XML de BCN, en P-01 · Inventario normativo](BLUEPRINT.md#textos-cortados-del-xml-de-bcn)). El [issue 37](https://github.com/ingUANDES/donationstax/issues/37) listó tres; la cuarta llegó con el texto de la Ley 21.850, bajado después. Bajado otra vez el 2026-10-28, el XML trae el U+FFFD en las cuatro (comprobación sin script en `analisis/`). Ninguna cita de [cambios.csv](normativa/cambios.csv) cae en ellas.

| Estructura | XML | Visor | Diario Oficial |
|---|---|---|---|
| Ley 20.431, art. 6 N° 4, que agrega el art. 4° bis al DFL 7 de 1980 ([ley20431_v2026-10-22.txt, L174](normativa/textos/ley20431_v2026-10-22.txt#L174)) | «Art�culo 4º bis» | «Articulo 4º bis» | sin verificar: la edición electrónica del 2010-04-30 dice «No existen publicaciones» |
| Ley 21.210, epígrafe del Párrafo VI, con el texto que le dio la Ley 21.713 ([ley21210_v2026-10-22.txt, L3548](normativa/textos/ley21210_v2026-10-22.txt#L3548)) | «Reuni�nes» | «Reuniones» | «Reuniones»: Ley 21.713, p. 58 del PDF, numeral 15; el epígrafe original, en la Ley 21.210, p. 90 |
| Ley 21.210, art. trigésimo segundo transitorio, letra e) ([ley21210_v2026-10-22.txt, L5995](normativa/textos/ley21210_v2026-10-22.txt#L5995)) | «Instrucci�nes» | «Instrucciones» | «Instrucciones»: Ley 21.210, p. 113 |
| Ley 21.850, art. décimo cuarto transitorio ([ley21850_v2026-10-27.txt, L931](normativa/textos/ley21850_v2026-10-27.txt#L931)) | «Resoluci�nes» | «Resoluciones» | «Resoluciones»: Ley 21.850, p. 42 |

Fuentes del Diario Oficial: PDF firmados, pasados a texto con `pdftotext` y consultados el 2026-10-28; no se versionan (no verificables desde el repo).

- Ley 21.210: edición N° 42.587 del 2020-02-24, CVE 1730320, https://www.diariooficial.interior.gob.cl/publicaciones/2020/02/24/42587/01/1730320.pdf, sha256 `6ee7cf86beb9530ab3968b1fbf02005dfa10d77b0f90454ed15f70b305e7bf78`.
- Ley 21.713: edición N° 43.982-B del 2024-10-24, CVE 2561825, https://www.diariooficial.interior.gob.cl/publicaciones/2024/10/24/43982-B/01/2561825.pdf, sha256 `1b649dcc4af15cec525ffacfe5824d1dec158a266a0b466e89ffd9b709d243a8`. El numeral 15 dice «Reemplázase el epígrafe del Párrafo VI por “Reuniones con el Servicio de Impuestos Internos, el Servicio de Tesorerías y el Servicio Nacional de Aduanas”».
- Ley 21.850: edición N° 44.576-B del 2026-10-17, CVE 2888352, https://www.diariooficial.interior.gob.cl/publicaciones/2026/10/17/44576-B/01/2888352.pdf, sha256 `46ae2b8104768c9fd2417f1103d14deca602f27ef9be4cbe5ebf8ad57e6c9c7b`.

Se propone:

1. Un U+FFFD del XML no es texto de la norma: marca un carácter que el XML no escribió. Para el repo, rige el texto publicado en el Diario Oficial.
2. El visor no reemplaza al Diario Oficial en esas posiciones. Coincide con él en las tres estructuras contrastadas, pero en la de la Ley 20.431 trae «Articulo», sin tilde, y es la única vez que el visor lo escribe así en esa ley: «Artículo» aparece 15 veces con tilde y una sin ella (comprobación sin script en `analisis/`). Inferencia, sin verificar: en las cuatro el visor trae la letra sin tilde, y en los tres plurales esa es la grafía del Diario Oficial.
3. Una fila de [cambios.csv](normativa/cambios.csv) cuya cita incluya la posición de un U+FFFD solo queda «verificada» contra un extracto del Diario Oficial versionado en `normativa/textos/`, como [ley21735_do_2025-03-26.txt](normativa/textos/ley21735_do_2025-03-26.txt), con ese extracto en `texto_archivo` y la dirección del PDF en `fuente_url`. No sirve el XML, porque el U+FFFD no es texto, ni el visor, por el punto 2. Sin el extracto, la fila queda «sin verificar». Una cita que no incluya esa posición se comprueba contra el XML, como hasta ahora.
4. Si se aprueba, [verifica_citas.py](analisis/verifica_citas.py) pasa a fallar cuando la cita de una fila verificada trae U+FFFD ([issue 51](https://github.com/ingUANDES/donationstax/issues/51)). `contrastar` ya cuenta estas estructuras aparte, con la clase `fffd`.

Alternativas: el texto del visor, como hace [D-21 · Texto completo de las estructuras que el XML de BCN corta](#d-21) con las cortadas, que en la Ley 20.431 daría «Articulo»; volver a bajar el XML, que el 2026-10-28 trae el mismo U+FFFD; o dejar el texto del XML y no citar esos pasajes.

Evidencia débil: el Diario Oficial se leyó en el texto que `pdftotext` saca del PDF, y los PDF no están en el repo. La causa del U+FFFD está sin verificar. De la Ley 20.431 no hay texto publicado en la edición electrónica, así que el texto que rige en esa estructura sigue sin verificar.

Estado: Propuesta. La planteó el investigador el 2026-10-28 ([issue 37](https://github.com/ingUANDES/donationstax/issues/37), punto 3). El código lo reservó el usuario en la conversación del 2026-10-28 (no verificable desde el repo).

### D-28

**Qué cuenta como cambio de texto en la bisección de bcn_versiones.py.** Según [D-21 · Texto completo de las estructuras que el XML de BCN corta](#d-21), punto 4, un límite de versión que solo marca un corte no es un cambio de texto. Para aplicarlo, la bisección compara las estructuras cortadas con su texto completo, del JSON del visor, y esa comparación solo puede hacerse con los espacios normalizados, porque el visor no conserva los espacios del XML. Si en las estructuras sin corte contaran los espacios del XML, el resultado dependería de la descarga. El XML del 2010-07-30 bajado el 2026-10-28 trae cortado el art. 20 de la LIR (comprobación sin script en `analisis/`), y con esa descarga no se ve el espacio que el XML agrega el 2010-07-31 ([lir_art20_31_52_v2010-07-31.txt, L12](normativa/textos/lir_art20_31_52_v2010-07-31.txt#L12)). La bisección del 2026-10-22 sí lo vio.

Se propone:

1. Dos fechas tienen el mismo texto si traen las mismas estructuras (idParte y derogado) y, en cada una, el mismo texto completo con los espacios normalizados, como compara [verifica_citas.py](analisis/verifica_citas.py). El texto completo es el del XML, o el del visor si el XML corta la estructura. Una diferencia solo de espacios o de atributos del XML (`fechaVersion`) no es un cambio de texto ni da fecha de vigencia.
2. En la serie de la LIR, eso deja fuera dos versiones (clases de [limites_bcn.py](analisis/limites_bcn.py)):
   - 2010-07-31, «solo espacios»: el art. 20 agrega un espacio antes de un salto de línea ([lir_art20_31_52_v2010-07-31.txt, L12](normativa/textos/lir_art20_31_52_v2010-07-31.txt#L12)).
   - 2025-07-11, «sin cambio de texto»: el texto guardado es el de 2024-01-01, y solo cambia la `fechaVersion` del art. 20. El XML del día anterior trae saltos de línea al final del art. 20 que el de este día no trae y que el texto guardado no conserva (descargas del 2026-10-28, comprobación sin script en `analisis/`). La bisección corregida informa ese tramo como «solo en espacios».

   En las dos fechas cambia la `fechaVersion` del art. 20, y son las de publicación de las Leyes 20.455 y 21.755 ([ley20455_v2010-08-01.txt, L3](normativa/textos/ley20455_v2010-08-01.txt#L3), [ley21755_v2026-10-26.txt, L3](normativa/textos/ley21755_v2026-10-26.txt#L3)), que fijan tasas de primera categoría en artículos propios ([cambios.csv, L57-L58](normativa/cambios.csv#L57-L58) y [cambios.csv, L68](normativa/cambios.csv#L68)). Inferencia: BCN marca el art. 20 al publicarse esas leyes, sin cambiar su texto.
3. Si se aprueba, esas dos versiones pasan a `normativa/textos/retiradas/`, y limites_bcn.py agrega «solo espacios» y «sin cambio de texto» a las clases que no pueden quedar en una serie (`RETIRABLES`; [issue 51](https://github.com/ingUANDES/donationstax/issues/51)). Mientras tanto siguen en la serie, y `limites_bcn.py comparar` las acepta de más.
4. [bcn_versiones.py](analisis/bcn_versiones.py) ya compara así desde el issue 37, porque es la forma de comparar con el visor sin depender de la descarga. Si esta decisión no se aprueba, falta decidir cómo tratar los espacios cuando una de las dos fechas trae la estructura cortada.

Alternativas: contar los espacios del XML en las estructuras sin corte y normalizarlos solo en las cortadas, con lo que el resultado depende de la descarga, como en 2010-07-30; o comparar el texto del visor de todas las estructuras, que también normaliza los espacios y pide el JSON del visor en cada fecha de la bisección.

Evidencia débil: que el corte oculte el espacio de 2010-07-31 se vio en una sola descarga. Si las del 2026-10-22 de los días anteriores traían el art. 20 completo está sin verificar; la de 2009-04-28 sí lo traía ([lir_art20_31_52_v2009-04-28.txt](normativa/textos/lir_art20_31_52_v2009-04-28.txt)).

Estado: Propuesta. La planteó el investigador el 2026-10-28 ([issue 37](https://github.com/ingUANDES/donationstax/issues/37), puntos 1 y 2). El código lo reservó el usuario en la conversación del 2026-10-28 (no verificable desde el repo).

