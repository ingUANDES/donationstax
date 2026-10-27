# Blueprint

Pregunta de investigación: evaluar el impacto de los cambios regulatorios en las donaciones con fines sociales en Chile, con el alcance de [D-02 · Alcance](DECISIONES.md#d-02) y el monto donado como variable de resultado ([D-12 · Variable de resultado](DECISIONES.md#d-12)). Este archivo define el método y el backlog; el estado está en el [README](README.md#estado) y las decisiones en [DECISIONES.md](DECISIONES.md).

Serie **P-nn**: pendientes. Este es el único archivo donde se define el prefijo P- y donde viven sus ítems. Se toman en el orden que fije una persona ([D-06 · Canal de trabajo](DECISIONES.md#d-06)). Citar desde otro archivo: `[P-01 · Inventario normativo](BLUEPRINT.md#p-01)`.

## Etapas

| Etapa | Producto | Cómo se verifica | Backlog |
|---|---|---|---|
| E1 Inventario normativo | [normativa/cambios.csv](normativa/cambios.csv) y textos en `normativa/textos/` | [verifica_citas.py](analisis/verifica_citas.py) | [P-01](#p-01) |
| E2 Estructura de los datos | [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv), [datos/esquema_ley21440_por_anio.csv](datos/esquema_ley21440_por_anio.csv), [datos/cruce_donatarias_por_anio.csv](datos/cruce_donatarias_por_anio.csv), [datos/armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv), [datos/rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv), [datos/anio_evaluacion_por_anio.csv](datos/anio_evaluacion_por_anio.csv) | esquema calculado en Python y en R, comparados ([contraste_datos.py](analisis/contraste_datos.py)) | [P-02](#p-02), [P-06](#p-06), [P-07](#p-07) |
| E3 Precio fiscal por régimen | [datos/precio_fiscal_por_caso.csv](datos/precio_fiscal_por_caso.csv) y [datos/precio_fiscal_por_cambio.csv](datos/precio_fiscal_por_cambio.csv), de [precio_por_regimen.py](analisis/precio_por_regimen.py) y [precio_por_regimen.R](analisis/precio_por_regimen.R) | [contraste.py](analisis/contraste.py) contra valores calculados a mano | [P-04](#p-04) |
| E4 Diseños de estimación por cambio | scripts en `analisis/`, Python y R | simulación con efecto conocido y contraste | [P-05](#p-05) |
| E5 Resultados y manuscrito | tablas y texto del artículo | solo cifras con nivel «verificada» ([D-05 · Niveles de resultado](DECISIONES.md#d-05)) | sin abrir |

## Verificación

El único punto de entrada es `make verificar` ([Makefile](Makefile)). Hoy comprueba que Python y R calculan el mismo precio fiscal sobre casos ficticios con valor esperado calculado a mano, que ambos contrastes fallan cuando se altera una salida, que cada fila normativa marcada `verificada` cita el texto de la norma literalmente, que la cabecera de cada texto del XML de BCN lista sus estructuras cortadas y que el texto completo de cada una, tomado del visor de LeyChile, empieza con el del XML ([P-01 · Inventario normativo](#p-01)), y que el precio fiscal por régimen y año de [P-04 · Precio fiscal por régimen](#p-04) da lo mismo en Python, en R y en el cálculo a mano y coincide con la salida versionada. Una fila ficticia no es evidencia sobre la norma: los casos de [analisis/fixtures/](analisis/fixtures/precio_fiscal_prueba.csv) solo prueban la implementación.

Reglas: niveles de resultado en [D-05 · Niveles de resultado](DECISIONES.md#d-05); semilla fija en toda simulación; antes de usar datos reales, el diseño se prueba con datos simulados de efecto conocido; ningún dato confidencial en el repositorio (los crudos van en `data_local/`, ignorado por git).

Criterios con que se audita cada avance:

1. `make verificar` termina con código 0 en una máquina limpia con Python 3 y R.
2. Cada fila normativa `verificada` pasa la comprobación de cita textual y distingue ley, reglamento y circular o resolución del SII, y el texto vigente de las versiones anteriores.
3. Ninguna cifra se llama «verificada» sin script en `analisis/`; toda cifra citada lleva archivo, versión o commit y línea.
4. Lo que es inferencia del investigador está marcado como tal; lo que no se pudo comprobar dice «sin verificar».
5. El diff no contiene datos confidenciales ni cambios a prioridades; no hay push a `master`.

## Riesgos de identificación a testear

Son inferencias del investigador, no hallazgos. Cada una se convierte en un caso de [P-05 · Simulaciones de efecto conocido](#p-05).

- Variables instrumentales con la alícuota legal del impuesto de primera categoría: varía solo por año, así que es colineal con efectos fijos de año; hace falta variación entre donantes.
- Diferencias en diferencias con tratamiento definido por el nivel previo de la variable resultado (más de 1.000 UTM): invita a reversión a la media.
- Grupo de control de 200 a 900 UTM: cruza los 700 UTM por institución que describe el [artículo, L121](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L121) (sin verificar contra la norma).
- Bunching: si la regla solo cambia el precio del exceso sobre 1.000 UTM, el conjunto presupuestario tiene un quiebre y no un salto, y quien aporta el 33 % al Fondo Mixto evita el precio mayor; la elasticidad no sale de la fórmula del quiebre simple (sin verificar contra la norma).
- Panel de donantes sin RUT: la unión por nombre mete error de medición en el período previo ([P-02](#p-02)).
- Tasa marginal constante de 27 % para todos los donantes: no es la tasa de todos los años ni de todos los contribuyentes ([P-04](#p-04)).

## Evaluabilidad de cada cambio

Inferencia del investigador a partir de [cambios.csv](normativa/cambios.csv) y [esquema_por_anio.csv](datos/esquema_por_anio.csv); el marco es la ventana de [D-08 · Ventana empírica](DECISIONES.md#d-08) (aprobada) y cambia si cambian los datos.

| Cambio | Fecha del tratamiento | ¿Evaluable con las planillas? |
|---|---|---|
| Ley 20.316 | donaciones desde 2009 | No: 2008 y 2009 tienen 5 y 12 filas |
| Ley 20.565, de salto a quiebre | 2012-02-08 | El bunching posterior en el quiebre sí; el efecto del cambio no, porque el período previo (2011) tiene 656 filas |
| Cambios de tasa del impuesto a la renta | por año comercial ([P-04 · Precio fiscal por régimen](#p-04)) | No como cambio aislado (inferencia): la tasa cambia por año y es la misma para todos los donantes de un régimen, y las planillas no traen el régimen ni el tramo ([P-02](#p-02)); con efectos fijos de año no queda variación ([P-05](#p-05)) |
| Ley 21.210, artículo 10 | 2020-03-01 | En principio sí; el RUT del donante falta en 2019 |
| Ley 21.440 | 2022-07-01 | La salida del régimen de la Ley 19.885, sí. El régimen nuevo tiene datos publicados, pero sin donante ([P-07](#p-07)): la sustitución solo se mide agregada por donataria y fecha, y solo en las 95 de 659 donatarias de 2022 a 2025 que están en ambos registros (inferencia) |
| Ley 21.015 | por verificar | No: sus donaciones usan otro certificado y no dan crédito; los datos del Certificado N° 60 siguen pedidos en el [issue 6](https://github.com/ingUANDES/donationstax/issues/6) |
| Ley 21.850 | por verificar; vigencia desde noviembre o diciembre de 2026, según fuentes que no son el texto de la ley | No, si rige desde 2026: queda fuera de la ventana (inferencia); fuentes y qué falta leer en el [issue 12](https://github.com/ingUANDES/donationstax/issues/12) |

La ventana empírica es 2012 a 2025 ([D-08 · Ventana empírica](DECISIONES.md#d-08)).

## Pendientes

### P-01

**Inventario normativo (E1).** Estado: hecho el 2026-10-22, salvo lo de «Falta en este pendiente».

Producto: [normativa/cambios.csv](normativa/cambios.csv), una fila por cambio de parámetro con su cita literal, que [verifica_citas.py](analisis/verifica_citas.py) comprueba contra los textos de [normativa/textos/](normativa/textos/). Los textos de leyes salen de BCN con [bcn_versiones.py](analisis/bcn_versiones.py) y los de circulares, del SII; las estructuras que el XML de BCN corta se completan como dice [Textos cortados del XML de BCN](#textos-cortados-del-xml-de-bcn). La descarga y la comprobación de citas son solo en Python: el contraste Python y R de [D-03 · Python y R en paralelo](DECISIONES.md#d-03) se aplica a resultados numéricos. La columna `efecto_precio` es el signo a priori frente al régimen inmediatamente anterior para un donante que no cambia su conducta; el cálculo está en [P-04 · Precio fiscal por régimen](#p-04).

Lo que la norma dice y cambia el diseño de [P-05](#p-05) (cada punto tiene su fila en cambios.csv; lo que va más allá de la fila es inferencia del investigador):

- **El tratamiento rige por la fecha de la donación, no por la de la ley.** El artículo 2 transitorio de la Ley 20.316 la hace regir para donaciones desde 2009, aunque BCN muestra el texto nuevo desde 2010; dos de sus numerales rigen desde 2010 y se ven en BCN desde 2011. El evento de la Ley 20.316 es 2009.
- **La forma de la regla en 1.000 UTM cambió.** Entre la versión de 2010 y la de 2012 la tasa menor se aplicaba a la donación completa (un salto); desde 2012-02-08, solo a la parte que excede (un quiebre). Son dos diseños distintos de bunching; las planillas del Ministerio solo cubren con volumen el segundo ([P-02](#p-02)).
- **La unidad del umbral cambió.** Hasta 2012-02-08 se sumaban las donaciones a cualquier institución; desde entonces, las hechas a una misma institución en el año (N° 9). La Circular N° 71 del SII dice que la relación con el Fondo se aprecia por institución. Inferencia: la variable de ejecución debe definirse por donante, institución y año, y contrastarse con el total por donante.
- **Hubo una regla de dos instituciones y 700 UTM** entre 2010-01-01 y 2012-02-07, y los 25 % y 40 % para adicciones existen desde 2010; la Ley 20.565 los volvió marginales.
- **El precio de donar también cambió por el impuesto, y desde 2017 no hay un solo τ.** Las filas de los artículos 20 y 52 de la Ley sobre Impuesto a la Renta dan la tasa de primera categoría y la escala del global complementario por versión; desde 2017 la tasa depende del régimen del artículo 14. Inferencia: [P-04](#p-04) debe trabajar con τ por tipo de donante y por régimen, y las planillas no traen el régimen ni el tramo del donante ([P-02](#p-02)), así que el precio solo se puede acotar con escenarios.
- **Lo que no cambió en 2016:** la Ley 20.900 solo derogó el título de donaciones políticas.
- **Dos regímenes paralelos desde 2018 y 2022.** Las donaciones de la medida alternativa de la Ley 21.015 no dan crédito, solo gasto necesario, y su vigencia sigue sin verificar. La Ley 21.440 permite deducir de la base, con topes en UTM y en porcentaje, fuera del límite global del artículo 10, y desde 2022-07-01.

Contradicciones del encargo y del artículo del repo, resueltas con la norma:

- Sentido de la Ley 20.316: [D-07 · Sentido del cambio de la Ley 20.316](DECISIONES.md#d-07) (el artículo del repo acierta; el encargo no).
- Vigencias: el encargo acierta en las fechas de las Leyes 19.885, 20.316 (entrada en vigencia), 20.565 y 21.440. La tabla del [artículo, L369-L375](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L369-L375) está mal en la Ley 19.885 (aparece «2006») y en la «Ley 20.900(1)»: la modificación del 2012-02-08 es la Ley 20.565 (el D.O. de esa fecha lo dice la [Circular N° 49](normativa/textos/circular_sii_49_2012.txt)).
- La regla de dos instituciones y 700 UTM sí existió (el artículo acierta; el encargo la omitía) y los 25 % y 40 % no son de la Ley 20.565 (el encargo se equivoca).
- El encargo dice que las donaciones de la Ley 21.015 «aplican el esquema vigente de la Ley 19.885»: no dan crédito.
- Fuentes del encargo: no se revisaron una por una. La fuente 1 es un documento cuyo nombre de archivo dice 2006 y se cita para leyes de 2009 a 2022 (sigue sin leer), varias referencias repiten la misma URL y hay afirmaciones con las nueve fuentes en bloque.

#### Textos cortados del XML de BCN

Hallazgo del [issue 28](https://github.com/ingUANDES/donationstax/issues/28). El XML de BCN (`obtxml?opt=7`) trae incompleto el texto de algunas estructuras, y el corte cae justo después de un carácter no ASCII, que el XML escribe como referencia numérica: el `<Texto>` del artículo 1° de la Ley 20.630 termina en `disminuy&#233;</Texto>`, y el visor sigue con «ndose».

- **Marca.** [bcn_versiones.py](analisis/bcn_versiones.py) lista en la cabecera de cada texto las estructuras cuyo texto termina en un carácter no ASCII (regla `cortada`). En `make verificar`, [verifica_citas.py](analisis/verifica_citas.py) las cuenta y falla si una cabecera no coincide con la regla.
- **Texto completo.** Sale del JSON del visor de LeyChile (`servicios-leychile.bcn.cl`, `get_norma_json`) para la misma `idVersion`, pasado a texto sin las notas de BCN. `bcn_versiones.py completar` lo escribe en `<texto>_visor.txt` solo si el texto del XML es su prefijo, y verifica_citas.py vuelve a comprobar ese prefijo. Que una cita tomada de esos archivos pueda quedar «verificada» depende de [D-21 · Texto completo de las estructuras que el XML de BCN corta](DECISIONES.md#d-21), que está propuesta. La edición electrónica del Diario Oficial no sirve para la Ley 20.630: la del 2012-09-27 dice «No existen publicaciones en esta edición en la fecha seleccionada» (consulta del 2026-10-26, no verificable desde el repo).
- **Contraste con el visor.** `python3 analisis/bcn_versiones.py contrastar` sobre los 44 textos del XML (2026-10-26, hora de Chile; usa la red y no está en `make verificar`) dio: «estructuras: 651 en 44 textos | igual: 633 | cortada: 15 | distinta: 3 | sin_visor: 0 | la regla marca 15 de 15 cortadas y 0 de 636 no cortadas». Las 15 cortadas están en los 10 textos que tienen `_visor.txt` en [normativa/textos/](normativa/textos/); siete no estaban en la tabla del [issue 28](https://github.com/ingUANDES/donationstax/issues/28). Las 3 distintas difieren por un U+FFFD del XML ([issue 37](https://github.com/ingUANDES/donationstax/issues/37)). Una regla de «termina a mitad de palabra» (en letra) habría marcado 73 estructuras que no están cortadas (40 títulos, 17 párrafos y 8 artículos que terminan en «Derogado» o «INCISO DEROGADO», entre otras) y habría perdido el corte en «4° y 5°» (comprobación sin script en `analisis/`).
- **El corte no siempre se repite.** El XML bajado otra vez el 2026-10-26 trae completas 4 de las 15: el art. 31 de la LIR en las versiones 2004-02-09, 2006-02-10 y 2017-01-01, y el art. 20 en la 2008-02-08. Las otras 11 salen cortadas en el mismo carácter (comprobación sin script en `analisis/`). La causa sigue sin verificar.
- **Efecto sobre la bisección.** Un corte que aparece o desaparece cambia la huella, y bcn_versiones.py lo toma como un cambio de texto. Según el visor, 8 de los 21 límites de versión de `lir_art20_31_52_v*.txt` no cambian los artículos 20, 31 ni 52; ninguna fila de cambios.csv cita esas versiones ([issue 37](https://github.com/ingUANDES/donationstax/issues/37)).
- **Lecturas que esperaban el texto.** El número 7) del artículo 1° de la Ley 20.630 está en [ley20630_v2026-10-26_visor.txt, L94-L96](normativa/textos/ley20630_v2026-10-26_visor.txt#L94-L96), y el número 32) del artículo 1° de la Ley 20.780, que modifica el art. 52 de la LIR, en [ley20780_v2026-10-26_visor.txt, L1232](normativa/textos/ley20780_v2026-10-26_visor.txt#L1232). Las dos lecturas siguen pendientes en el [issue 29](https://github.com/ingUANDES/donationstax/issues/29).

Falta en este pendiente:

- Tasas transitorias de 2011 y 2012: leídas en [P-04](#p-04); la de 2012 sigue sin verificar ([issue 29](https://github.com/ingUANDES/donationstax/issues/29)).
- El texto de la Ley 20.565 en sí (se conoce por la versión consolidada de BCN y la Circular N° 49; no se encontró su `idNorma`), el reglamento (Decreto Supremo 77 de 2009) y las circulares del SII sobre la Ley 21.440.
- Mecánica del crédito bajo los regímenes de 2017 en adelante (semi integrado y atribuido; [issue 29](https://github.com/ingUANDES/donationstax/issues/29)) y la fecha de publicación de los reglamentos de la Ley 21.015.
- Los regímenes que el [artículo 10 de la Ley 19.885](normativa/textos/ley19885_v2024-08-24.txt) enumera y que [D-02 · Alcance](DECISIONES.md#d-02) deja fuera por ahora.
- Las versiones de la LIR que solo marcan un corte del XML y los tres U+FFFD del XML ([issue 37](https://github.com/ingUANDES/donationstax/issues/37)).

### P-02

**Estructura de los datos (E2).** Estado: perfil de esquema hecho el 2026-10-22, a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)): Python y R coinciden en los 18 años. Queda armonizar las planillas ([P-06](#p-06)).

Producto: [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv) (una fila por año, 2008 a 2025, con columnas, conteos por tipo de valor, rangos de fecha, sumas y duplicados), [datos/descargas_midesof.csv](datos/descargas_midesof.csv) (URL, tamaño, sha256 y `Last-Modified` de cada archivo, para detectar cambios) y la [instantánea de la página del Ministerio](datos/fuentes/midesof_pagina_2026-10-22.txt). Scripts: [descargar_midesof.py](analisis/descargar_midesof.py), y [esquema_datos.py](analisis/esquema_datos.py) y [esquema_datos.R](analisis/esquema_datos.R) con [contraste_datos.py](analisis/contraste_datos.py). `make verificar-datos` los corre (tarda varios minutos y necesita las planillas en `data_local/`). Las reglas de lectura comunes a los dos lenguajes están en la cabecera de esquema_datos.py; salieron del contraste: los espacios finales cuentan, las celdas solo con espacios son vacías, y las secuencias corruptas de los archivos de 2021 a 2025 pasan a U+FFFD porque `readxl` no puede abrir el de 2022 con ellas. El contraste fila a fila de [P-06](#p-06) agregó dos: los fines de línea se normalizan a «\n» y el escape `_x000D_` se decodifica.

Lo que los datos muestran (las cifras están en esquema_por_anio.csv; lo marcado como inferencia es del investigador):

- **Cobertura.** Hasta 2011 hay entre 5 y 656 filas por año; desde 2012, decenas de miles o más. Las planillas del Ministerio son [«información auto reportada» a través de los Certificados N° 25](datos/fuentes/midesof_pagina_2026-10-22.txt): no traen el Certificado N° 60 ni el régimen de la Ley 21.440. Inferencia: el salto de 2012 puede ser un cambio en quién reporta y no en las donaciones.
- **RUT del donante** solo en 2015 a 2017 y en 2020 a 2025; falta de 2008 a 2014 y en 2018 y 2019.
- **Fondo Mixto** como monto en la mayoría de los años, pero solo como «SI/NO» en 2019 y 2021 (y en parte de 2018): en esos años el 33 % no se puede medir por monto.
- **Cinco variantes de esquema**: ocho columnas (2008 a 2014, 2018 y 2019), con RUT (2015 y 2016), con folios (2017), con RUT de la institución y región (2020 a 2023) y la columna de fecha renombrada (2024 y 2025).
- **Fechas.** La mayoría de las de 2012 son texto, y también parte de las de 2015, 2017 y 2018; hay fechas fuera de rango (2003 en el archivo de 2013, 2028 en el de 2018) y filas cuyo `AÑO` no coincide con el año de la fecha (hasta 460 en 2021).
- **Montos.** 7.163 filas de 2014 tienen monto total no numérico y quedan fuera del conteo de filas de datos; el [P-06](#p-06) debe inspeccionarlas.
- **Duplicados exactos** entre 0 y 4,9 % de las filas por año (2022: 44.670). Sin verificar si son donaciones repetidas legítimas.
- **Texto corrupto** (acentos dañados en nombres de personas) en 2021 a 2025.
- **Las filas por año no son comparables.** El archivo de 2024 tiene 353 mil filas y 37 mil donantes; el de 2023, 981 mil filas y 113 mil donantes. Inferencia, con evidencia débil: 2024 podría estar incompleto o reportado distinto. La página dice «al 31 de diciembre del 2024» y aun así ofrece el archivo de 2025.

Consistencia con el extracto del repo: las filas por año de `donationstaxrpkg/data/d_2010_2014.rda` (2010: 42; 2011: 656; 2012: 35.586; 2013: 98.328; 2014: 1) coinciden con los archivos de 2010 a 2013 de hoy (salvo una fila de 2013 con monto no numérico); ese extracto no incluye las casi un millón de filas de 2014. Su `fecha2` sigue con el error de [P-03](#p-03).

Estabilidad: entre 2026-10-22 y 2026-10-23 ninguno de los 18 archivos cambió (misma huella, URL y `Last-Modified`; comparación sin script en `analisis/`, no verificada). El usuario indica que el Ministerio los actualiza y limpia con el tiempo; la consecuencia está en [D-09 · Fuente y completitud de los datos](DECISIONES.md#d-09). Volver a correr [descargar_midesof.py](analisis/descargar_midesof.py) y comparar con [datos/descargas_midesof.csv](datos/descargas_midesof.csv) detecta los cambios.

### P-03

**Corregir el origen de las fechas de Excel en `tidy_data.R`.** Estado: pendiente.

[tidy_data.R L25](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/donationstaxrpkg/dev/tidy_data.R#L25) convierte fechas seriales con `origin = "1900-12-30"`. Comprobación ad hoc sin script (no verificada), sobre `d_2010_2014.rda` en `ed3eb2e`: 101.660 de 134.613 filas tienen `fecha` serial de 5 dígitos; en todas, `fecha2` coincide con el origen 1900-12-30, y su año coincide con `ano` en 3 de 101.660; con origen 1899-12-30 coincide en 101.654 de 101.660. Ejemplo: `fecha` 40280, `ano` 2010, `fecha2` 2011-04-12. Con ventanas por fecha de ley, queda corrido un año el 75,5 % de las filas.

Hacer: cambiar el origen, regenerar `d_2010_2014`, escribir en `analisis/` un script que falle con `AssertionError` si `year(fecha2) != ano` fuera de una tolerancia fijada de antemano, e inspeccionar a mano las 6 filas seriales que no coinciden y las 4 con `fecha2` NA. Sin verificar si el [artículo](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L75) usa `fecha2`: carga archivos de `data_local/` que no están en el repo.

Nota del 2026-10-22: las planillas actuales de 2010 a 2013 traen el mismo número de filas que este extracto y la fecha ya como fecha de Excel ([P-02](#p-02)), de modo que [P-06](#p-06) puede reemplazar el extracto. Este pendiente solo vale si se conserva el paquete tal como está.

### P-04

**Precio fiscal por régimen (E3).** Estado: hecho el 2026-10-26 ([issue 21](https://github.com/ingUANDES/donationstax/issues/21)), a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)). Python, R y los valores calculados a mano coinciden en los 72 casos, y Python y R en las 52 comparaciones, con tolerancia 1e-12 (`make verificar`). Las lecturas que quedan sin verificar están en el [issue 29](https://github.com/ingUANDES/donationstax/issues/29).

Método. P = 1 − c − τg por peso donado: c es la fracción que se imputa como crédito, g la que se deduce de la base como gasto y τ la tasa marginal del impuesto que esa deducción reduce. Un caso es un régimen (ley, tramo, aporte al Fondo, límite) en un año comercial y para un tipo de contribuyente. El precio es por régimen y año, no por donante, porque las planillas no traen régimen ni tramo ([P-02](#p-02)). Cada c, g y τ sale de una fila verificada de [cambios.csv](normativa/cambios.csv), referida por norma, artículo y vigencia; los scripts no fijan parámetros. Que un caso no tenga beneficio (P = 1) es inferencia, y lo dice la nota del caso.

Producto:

- [normativa/precio_casos.csv](normativa/precio_casos.csv): los casos, con la fila de cambios.csv y la regla de cada c, g y τ (reglas en la cabecera de [precio_por_regimen.py](analisis/precio_por_regimen.py)). La columna `anio_tau` dice cómo se asignó τ al año comercial: «regla explícita» (una ley nombra el año comercial), «texto permanente» (el mismo texto rige con las dos lecturas del párrafo siguiente), «sin verificar», «contrafactual» o «no aplica».
- [normativa/precio_comparaciones.csv](normativa/precio_comparaciones.csv): el par de casos (antes, después) con que se mide cada fila de cambios.csv.
- [precio_por_regimen.py](analisis/precio_por_regimen.py) y [precio_por_regimen.R](analisis/precio_por_regimen.R), que usan [precio_fiscal.py](analisis/precio_fiscal.py) y [precio_fiscal.R](analisis/precio_fiscal.R). Salidas: [datos/precio_fiscal_por_caso.csv](datos/precio_fiscal_por_caso.csv) y [datos/precio_fiscal_por_cambio.csv](datos/precio_fiscal_por_cambio.csv). La segunda tiene una línea por comparación, con el signo calculado y si coincide con `efecto_precio`, y una por cada fila sin comparación. Valores a mano: [precio_casos_esperado.csv](analisis/fixtures/precio_casos_esperado.csv).

Tasa por año comercial. La tasa que reduce una donación es la de las rentas del año comercial en que se hace. Cuando la ley no nombra el año, hay dos lecturas: el texto vigente durante ese año, o el vigente al declarar, por el [Código Tributario, art. 3 inc. 2](normativa/cambios.csv#L62): «los impuestos que deban pagarse a contar de esa fecha quedarán afectos a la nueva ley».

- 2011: [Ley 20.455, art. 1 letra a)](normativa/cambios.csv#L57), «A 20% (veinte por ciento) para las rentas que se perciban o devenguen durante el año calendario 2011.»
- 2012 (sin verificar): la letra b), 18,5 % ([L58](normativa/cambios.csv#L58)), fue derogada por la [Ley 20.630, art. 4 N° 1](normativa/cambios.csv#L59), y se usa el 20 % del texto vigente desde 2013-01-01 ([L47](normativa/cambios.csv#L47)). Lo apoyan el art. 3 del Código Tributario, la vigencia general de la [Ley 20.630, art. 8](normativa/cambios.csv#L60) («regirá a partir del día primero de enero de 2013») y el factor 1,081 de los pagos provisionales de 2012 ([L61](normativa/cambios.csv#L61)), que es 20/18,5 (inferencia). La regla especial del número 7), letra a), del artículo 1° sigue sin leer: el XML de BCN corta ese artículo, y su texto completo está desde el [issue 28](https://github.com/ingUANDES/donationstax/issues/28) en [ley20630_v2026-10-26_visor.txt, L94-L96](normativa/textos/ley20630_v2026-10-26_visor.txt#L94-L96) ([Textos cortados del XML de BCN](#textos-cortados-del-xml-de-bcn)). Con 17 % el precio del tramo hasta 1.000 UTM sería 0,415 y no 0,40 ([datos/precio_fiscal_por_caso.csv, L19-L20](datos/precio_fiscal_por_caso.csv#L19-L20)).
- 2014 a 2017: tasas por año comercial de la [Ley 20.780, art. cuarto transitorio](normativa/cambios.csv#L63) y, para la letra B del art. 14, su [inciso 2](normativa/cambios.csv#L64). Desde 2020, el texto nuevo del art. 20 rige «únicamente a partir del año comercial 2020» ([Ley 21.210, art. octavo transitorio](normativa/cambios.csv#L65)).
- Pro Pyme: [Ley 21.256, art. 1](normativa/cambios.csv#L66) (2020 a 2024, texto modificado por la [Ley 21.578](normativa/cambios.csv#L67)) y [Ley 21.755, art. 25](normativa/cambios.csv#L68) (2025). Esta última exige una cotización de la Ley 21.735 al cierre del ejercicio, cuyo cumplimiento está sin verificar.
- Global complementario ([L53 a L56](normativa/cambios.csv#L53-L56)): con las dos lecturas, 2016 queda sin verificar (tramo superior de 40 % o 35 %; precio 0,30 o 0,325: [datos/precio_fiscal_por_caso.csv, L64-L65](datos/precio_fiscal_por_caso.csv#L64-L65)).

Signo y magnitud ([datos/precio_fiscal_por_cambio.csv](datos/precio_fiscal_por_cambio.csv)):

- Los cambios de la Ley 19.885 mueven el precio mucho más que las tasas. En 2010, la Ley 20.316 baja de 1 a 0,415 el precio de quien da menos de 1.000 UTM sin aportar al Fondo ([L6](datos/precio_fiscal_por_cambio.csv#L6)) y lo sube de 0,415 a 1 para quien da a una sola institución ([L7](datos/precio_fiscal_por_cambio.csv#L7)). En 2012, la Ley 20.565 lo baja de 1 a 0,40 para ese donante y de 0,52 a 0,40 en el tramo hasta 1.000 UTM de quien supera el umbral sin Fondo ([L19-L20](datos/precio_fiscal_por_cambio.csv#L19-L20)).
- En la Ley 19.885 (g = 0,5), cada punto de tasa de primera categoría baja el precio en medio punto: 0,415 en 2010, 0,40 de 2011 a 2013, 0,365 en la letra B desde 2018 y en la letra A desde 2020 ([L51-L56](datos/precio_fiscal_por_cambio.csv#L51-L56), [L64](datos/precio_fiscal_por_cambio.csv#L64), [L70-L74](datos/precio_fiscal_por_cambio.csv#L70-L74)). Las tasas Pro Pyme rebajadas lo suben: 0,45 con 10 % y 0,4375 con 12,5 %, frente a 0,375 con la tasa permanente ([L76-L79](datos/precio_fiscal_por_cambio.csv#L76-L79)).
- La Ley 21.440 deduce de la base (c = 0, g = 1). Para una empresa de la letra A en 2023 el precio es 0,73, el doble del 0,365 de la Ley 19.885 dentro de sus límites; sobre el límite del art. 10, que no se le aplica, baja de 1 a 0,73 ([L39-L40](datos/precio_fiscal_por_cambio.csv#L39-L40)). Para una persona en el tramo superior es 0,60 ([datos/precio_fiscal_por_caso.csv, L71](datos/precio_fiscal_por_caso.csv#L71)).
- Tres comparaciones dan un signo distinto del `efecto_precio` declarado, porque el cambio actúa fuera del margen medido: con el quiebre de 2012 el precio marginal sobre 1.000 UTM no cambia (0,52 y 0,48: [L22](datos/precio_fiscal_por_cambio.csv#L22), [L26](datos/precio_fiscal_por_cambio.csv#L26)); lo que baja es el tramo inferior. La escala de 2013 no toca el tramo superior de 40 % ([L61](datos/precio_fiscal_por_cambio.csv#L61)).

Crédito y límites en el margen. Para una empresa de la letra A en 2025 ([datos/precio_fiscal_por_caso.csv, L49-L52](datos/precio_fiscal_por_caso.csv#L49-L52)), la donación marginal cuesta:

- 0,365 mientras el crédito del año no llega al tope;
- 0,73 cuando llegó. El crédito no puede exceder 14.000 UTM al año ([art. 1 N° 12](normativa/cambios.csv#L13)), y «Aquella parte de las donaciones que no pueda ser utilizada como crédito, se considerará un gasto necesario para producir la renta» ([N° 14](normativa/cambios.csv#L14)). Si el N° 14 cubre también el crédito que excede el impuesto, el precio es el mismo (inferencia);
- 1 sobre el límite global del 5 % de la renta líquida imponible ([art. 10 inc. 1](normativa/cambios.csv#L15)), que se aplica «ya sea que el beneficio tributario consista en un crédito contra el impuesto de Primera Categoría, o bien en la posibilidad de deducir como gasto la donación». Si el exceso es gasto rechazado, el precio pasa de 1 (sin verificar).

El tope de crédito y el límite global son entonces quiebres que suben el precio marginal (inferencia). Los trabajadores del art. 43 N° 1 recuperan el exceso del crédito («si resultare un exceso, éste se devolverá»: [art. 1 bis inc. 2](normativa/cambios.csv#L71)), y su precio es 0,5 aunque el crédito supere el impuesto.

Evidencia débil y alcance:

- El precio es el de la empresa. No incluye cómo el crédito y el menor impuesto de primera categoría pasan a los impuestos finales del dueño, que desde 2017 dependen del régimen del art. 14 ([issue 29](https://github.com/ingUANDES/donationstax/issues/29)). Tampoco cubre el Pro Pyme transparente (art. 14 letra D N° 8) ni el art. 14 ter.
- El caso con pérdida tributaria de 2020 es una cota: supone que la pérdida se usa después, a la misma tasa y sin descontar.
- La vigencia de la Ley 21.015 sigue sin verificar ([L41](normativa/cambios.csv#L41)); su caso solo usa la tasa de 2025.
- Las personas se calculan en el primer tramo y en el superior. A qué contribuyentes del global complementario se aplicaba el límite del art. 10 antes de 2020 está sin verificar ([L17](normativa/cambios.csv#L17), [L28](normativa/cambios.csv#L28)).
- Las comparaciones de los programas de adicciones de 2010 son entre fines en un mismo año, no en el tiempo.

### P-05

**Simulaciones de efecto conocido (E4).** Estado: pendiente.

Antes de usar datos reales, un caso simulado por diseño, en Python y en R, con semilla fija, tolerancias fijadas antes de correr ([D-05 · Niveles de resultado](DECISIONES.md#d-05)) y el monto donado como resultado ([D-12 · Variable de resultado](DECISIONES.md#d-12)):

- Bunching en un quiebre del conjunto presupuestario: elasticidad conocida, polinomio de orden 3 a 6, ventana de exclusión, error estándar por remuestreo; que recupere la masa en exceso y la elasticidad; sensibilidad al orden y a la ventana; incluir donantes que cumplen la condición del Fondo Mixto.
- Estudio de eventos y diferencias en diferencias: efecto y tendencias paralelas conocidos; un caso con tratamiento definido por el nivel previo del resultado que muestre el sesgo.
- Variables instrumentales: un caso con efectos fijos de año que muestre que la alícuota por año no aporta variación.

Cada riesgo de [Riesgos de identificación a testear](#riesgos-de-identificación-a-testear) debe quedar cubierto por al menos un caso.

### P-06

**Armonizar las planillas (E2).** Estado: hecho el 2026-10-24 ([issue 9](https://github.com/ingUANDES/donationstax/issues/9)), a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)). Python y R coinciden en las tablas y celda por celda en las 7.877.362 filas. Se aplicaron las ocho reglas de [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10): la armonización (reglas 1 a 6 y 8) y la regla 7, completar el RUT por nombre. Los casos que [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10) no cubre están en [D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13), aprobada el 2026-10-26 con un cambio en el punto 1. El año de la donación sigue [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14); los scripts aplican las dos desde el [issue 20](https://github.com/ingUANDES/donationstax/issues/20).

Producto: [armonizar.py](analisis/armonizar.py) y [armonizar.R](analisis/armonizar.R), con la regla exacta de cada caso en la cabecera del de Python. Salidas:

- `data_local/armonizado/py/` y `data_local/armonizado/r/`: una fila por fila de cada planilla. Traen nombres, así que no se versionan.
- [datos/armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv): cuántas filas toca cada regla, por año del archivo (líneas 2 a 19: 2008 a 2025).
- [datos/armonizacion_anios_donacion.csv](datos/armonizacion_anios_donacion.csv): filas y monto por año del archivo y año de la donación. Desde [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14) los dos años coinciden en todas las filas.
- [datos/anio_evaluacion_por_anio.csv](datos/anio_evaluacion_por_anio.csv) y [datos/anio_conflictos.csv](datos/anio_conflictos.csv), de [evalua_anio.py](analisis/evalua_anio.py) y [evalua_anio.R](analisis/evalua_anio.R) (`make verificar-anio`). La primera dice de dónde puede salir el año (fecha, `AÑO` o archivo) y cuántas filas y qué monto le tocan a cada año con el año del archivo o con el de la fecha. La segunda trae las filas con fecha en otro año, por año del archivo, diferencia de años y mes.
- [datos/diccionario_columnas.csv](datos/diccionario_columnas.csv): encabezado original y normalizado, rol, tipo y variante de esquema de cada columna de cada año.

[contraste_armonizado.py](analisis/contraste_armonizado.py) compara las filas y [contraste_datos.py](analisis/contraste_datos.py), las tablas; `make verificar-armonizado` corre todo y `make verificar-datos` lo incluye.

El contraste fila a fila encontró dos diferencias entre lectores que el perfil de [P-02](#p-02) no veía, y las dos quedaron como reglas de lectura comunes en [esquema_datos.py](analisis/esquema_datos.py):

- Los archivos traen CRLF literales dentro del texto. calamine los normaliza a «\n», como manda la sección 2.11 de XML 1.0, y readxl no; ahora esquema_datos.R los normaliza antes de leer.
- El único escape `_xHHHH_` que no es mitad de par sustituto (`_x000D_`, una vez en 2024) lo decodifica calamine; la regla de [P-02 · Estructura de los datos](#p-02) que lo dejaba literal no se había puesto a prueba.

Lo que muestran las planillas (cifras de armonizacion_por_anio.csv salvo que se diga otra cosa):

- **Variantes de esquema:** las cinco de [P-02 · Estructura de los datos](#p-02). 1: 2008 a 2014, 2018 y 2019; 2: 2015 y 2016; 3: 2017; 4: 2020 a 2023; 5: 2024 y 2025 (diccionario_columnas.csv).
- **Regla 1, montos:**
  - En 2014 se recuperan 7.163 totales vacíos como Fondo Mixto (0) más Institución. Ese año, el total coincide con la suma en 955.091 de 955.093 filas válidas (línea 8).
  - Se excluyen 11 filas por monto: 1 de 2013 (Institución vacía), 1 de 2020 (la fila de instrucciones de la plantilla) y 9 de 2024, con montos escritos como «$ 20,000,000» ([D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13), punto 2).
  - La fila de 2014 sin `AÑO` ni fecha se incluye con el año del archivo ([D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13), punto 1), así que quedan excluidas 11 de las 7.877.362 filas.
- **Regla 2, Fondo Mixto:** el 33 % no es medible en las filas con SI/NO (38.222 en 2018, 470.431 en 2019 y 438.604 en 2021), vacías (32.510 en 2014, entre otras) u «otro» (25 en 2015).
- **Fechas y año de la donación (regla 3 y [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14)):**
  - 67.092 fechas de texto se interpretan.
  - 1.791 no son interpretables, 6 no existen en el calendario (como «31-11-2011»), 4 caen fuera de 2008 a 2025 y 24 están vacías. En esas filas el mes queda desconocido.
  - El año de la donación es siempre el del archivo.
  - 5.387 filas tienen la fecha válida en otro año: 4.518 de 2018, 460 de 2021, 340 de 2015, 65 de 2017 y 4 de 2012 a 2014 (columna `n_fecha_otro_anio`). Van marcadas, con el mes desconocido y el año de la fecha en `anio_fecha`.
  - Entre esas 5.387, en 5.343 la fecha cae en el año siguiente, 4.439 de ellas en febrero. `AÑO` coincide con el archivo en 5.352 y con la fecha en 35 (anio_evaluacion_por_anio.csv, fila «total»; anio_conflictos.csv).
  - Efecto de la regla en las filas con fecha válida: 2018 tiene 486.497 con el año del archivo y 482.043 con el de la fecha; 2019, 469.930 y 474.445 (anio_evaluacion_por_anio.csv, líneas 12 y 13). Las columnas `suma_monto_anio_archivo` y `suma_monto_anio_fecha` dan la sensibilidad del monto.
  - Hay 8 filas con la fecha en el año del archivo y `AÑO` distinto: 5 con otro año y 3 sin `AÑO`. Por [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14) toman el año del archivo.
  - Evidencia débil: 4.487 de las de 2018 son fechas de texto de febrero de 2019 de una sola donataria. No es una inversión día-mes (el 98,8 % tiene día mayor que 12), pero podrían ser fechas del certificado y no de la donación (inferencia; comprobación sin script).
- **Regla 5, duplicados exactos:** se marcan y no se excluyen; sus conteos son los de [P-02](#p-02). La sensibilidad sin duplicados está en `suma_monto_total_sin_duplicados`. Las mayores proporciones del monto en filas duplicadas son 2,81 % en 2013, 1,72 % en 2018 y 1,52 % en 2022 (cocientes de dos columnas, calculados en Python y en R sin script en `analisis/`).
- **Regla 6:** filas con U+FFFD en el donante: 53, 2.089, 487, 4 y 12, de 2021 a 2025.
- **RUT del donante (insumo de la regla 7):**
  - En 2022 los valores sin guion son el número del RUT sin dígito verificador. Solo 81.866 de 906.670 pasan Módulo 11 leídos como cuerpo + DV, el 9,0 %, cerca de 1/11, lo esperable por azar. En 2015 a 2017 pasa el 99,86 % o más (líneas 9 a 11 y 16).
  - Comprobación sin script: en 72.974 de 74.342 donantes de 2022 que aparecen con el mismo nombre en 2023, el valor de 2022 es el cuerpo del RUT de 2023.
  - Dígito verificador inválido: entre 1 (2022) y 798 (2016) filas por año. Mal formados: entre 0 y 204 (2023).

**Regla 7, completar el RUT** ([D-09 · Fuente y completitud de los datos](DECISIONES.md#d-09)). Scripts: [completar_rut.py](analisis/completar_rut.py) y [completar_rut.R](analisis/completar_rut.R), sobre las filas armonizadas de cada lenguaje; `make verificar-rut`.

Salidas:

- `data_local/rut/py/` y `data_local/rut/r/`: por fila, el nombre normalizado, de dónde sale el RUT, el RUT final y el identificador del donante. Lo que sigue sin RUT se identifica por su nombre y se marca como error de medición.
- [datos/rut_calibracion_identico.csv](datos/rut_calibracion_identico.csv) y [datos/rut_calibracion_similitud.csv](datos/rut_calibracion_similitud.csv): la calibración, ocultando el RUT de un año a la vez.
- [datos/rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv): el resultado por año.

Resultados:

- **Nombre idéntico.** Ocultando cada año con RUT, el nombre normalizado idéntico con un solo RUT en los demás años acierta en el 99,14 % (2025) a 99,98 % (2017) de los nombres a los que asigna RUT. Cumple el 99 % de [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10) en los nueve años (columna `precision` de rut_calibracion_identico.csv).
- **Similitud.** Ningún umbral de 0,80 a 0,99 llega al 99 %: el máximo es 98,04 % en 0,96 (1.753 de 1.788), y en 0,98 quedan 23 asignaciones con 82,6 % (rut_calibracion_similitud.csv, líneas 18 y 20). El paso por similitud no completa ningún RUT. Inferencia: un nombre parecido con otro RUT suele ser otra persona.
- **Filas completadas por nombre idéntico** (rut_completado_por_anio.csv):
  - 96,8 % de 2018, 95,0 % de 2019, 70,8 % de 2013, 67,6 % de 2012 y 43,6 % de 2014;
  - en los años con RUT, solo unas decenas de filas, salvo 823 en 2016.
  
  Siguen sin RUT, como error de medición, 561.105 filas de 2014 (56,4 %), 28.695 de 2013, 23.384 de 2019, 15.662 de 2018 y 11.519 de 2012. Los porcentajes se calcularon en Python y en R, sin script en `analisis/`.
- **Evidencia débil.** La precisión se mide por nombre distinto y no por fila ni por monto. En 2014 la mayoría de los donantes no aparece en ningún año con RUT (inferencia del pase exploratorio: ordenar las palabras del nombre no aumenta las coincidencias). Un donante de 2014 que tampoco aparece en los años con RUT solo se puede seguir por su nombre.

Los datos de [viantirreau/donaciones](https://github.com/viantirreau/donaciones) quedan para más adelante.

### P-07

**Datos del régimen de la Ley 21.440 (E2).** Estado: perfil de esquema y cruce por donataria hechos el 2026-10-23 ([issue 10](https://github.com/ingUANDES/donationstax/issues/10)), a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)): Python y R coinciden en los cuatro años y en el cruce. Responde al [issue 6](https://github.com/ingUANDES/donationstax/issues/6) en lo que toca a la Ley 21.440; el Certificado N° 60 de la Ley 21.015 sigue pedido allí.

Fuente: [donacionesley21440.gob.cl/donaciones](https://donacionesley21440.gob.cl/donaciones), con [instantánea del texto de la página](datos/fuentes/ley21440_donaciones_2026-10-23.txt). Cada página por año responde con un redireccionamiento a un `.xlsx` en `cdn.hacienda.cl` cuya URL va firmada y vence a los pocos minutos. [descargar_ley21440.py](analisis/descargar_ley21440.py) pide la página del año sin seguir el redireccionamiento, baja el archivo de inmediato y exige que su sha256 sea la huella que trae la ruta del CDN; deja el manifiesto [datos/descargas_ley21440.csv](datos/descargas_ley21440.csv) (página, ruta sin la firma, nombre original, tamaño, sha256, `Last-Modified`). Ambos hosts deben estar autorizados.

Producto: [datos/esquema_ley21440_por_anio.csv](datos/esquema_ley21440_por_anio.csv), una fila por año (líneas 2 a 5: 2022 a 2025), de [esquema_ley21440.py](analisis/esquema_ley21440.py) y [esquema_ley21440.R](analisis/esquema_ley21440.R); y [datos/cruce_donatarias_por_anio.csv](datos/cruce_donatarias_por_anio.csv), de [cruce_donatarias.py](analisis/cruce_donatarias.py) y [cruce_donatarias.R](analisis/cruce_donatarias.R). Los dos pares toman las reglas de lectura de [esquema_datos.py](analisis/esquema_datos.py) y [contraste_datos.py](analisis/contraste_datos.py) los compara; sus reglas propias (fila de datos, fila superior, RUT bien formado y Módulo 11, llave del cruce) están en la cabecera de cada script de Python. `make verificar-datos-21440` y `make verificar-cruce` los corren, y `make verificar-datos` incluye los dos.

Lo que muestran los archivos (cifras de esquema_ley21440_por_anio.csv salvo que se diga otra cosa; lo marcado como inferencia es del investigador). Las cifras exploratorias de la versión anterior de esta sección se reproducen, salvo la del tamaño frente a la Ley 19.885, que se corrige; la fila superior, los valores 0, el RUT con espacio y `Nac/Ext.` son nuevos:

- **Columnas:** `RUT Donataria`, `Donataria`, `Valor` y `Fecha`, más `Nac/Ext.` en 2024 y 2025. **No hay donante ni su RUT.** Una hoja por archivo y el encabezado en la fila 2.
- **Filas de datos:** 414, 14.291, 428.151 y 533.650, de 2022 a 2025; ninguna fila bajo el encabezado queda fuera.
- **Fila superior:** trae, sobre la columna `Valor`, un total igual a la suma de `Valor` en los cuatro años (8.073.239.854; 44.245.732.763; 66.629.506.401; 91.560.892.461; pesos nominales). La de 2025 dice además «Información actualizada al 07.07.26, en valores nominales.». Los nombres de 2022 a 2024 llevan «20.11.25» y el de 2025, «al 07.07.26» ([descargas_ley21440.csv](datos/descargas_ley21440.csv)).
- **Duplicados exactos:** 25, 10.302, 417.446 y 510.089 filas (6,0 %, 72,1 %, 97,5 % y 95,6 %). Sin donante ni identificador de donación no se distingue una donación repetida de un registro repetido. Inferencia: el total de la fila superior incluye los duplicados, así que quien publica los cuenta como donaciones.
- **Valores:** enteros y no negativos; 8 filas de 2023 valen 0. Máximos: 3.484.010.061, 9.714.000.000, 4.194.000.000 y 3.628.270.000.
- **Fechas:** todas son fechas de Excel y caen en el año del archivo. 7 de las 414 filas de 2022 son anteriores al 2022-07-01 (la primera, 2022-03-17), fecha en que empiezan los beneficios según el artículo primero transitorio de la Ley 21.440 ([normativa/cambios.csv](normativa/cambios.csv), línea 35).
- **RUT de la donataria:** 78, 283, 396 y 515 distintos; cada RUT tiene un solo nombre y cada nombre un solo RUT, y todos los bien formados pasan Módulo 11. En 2025, 48 filas de una donataria traen el RUT con un espacio final.
- **`Nac/Ext.`:** vacío salvo en 124 filas de 2024 y 212 de 2025, que dicen «Extranjero». Ni el archivo ni la página dicen qué significa (sin verificar).
- **Tamaño frente a la Ley 19.885 (corrige la versión anterior, que decía 2024 y 2025):** la suma de `Valor` supera a la de `MONTOTOTALDONACION` de las planillas de la Ley 19.885 en 2023, 2024 y 2025, no en 2022: 8.073.239.854 frente a 41.254.575.514, 44.245.732.763 frente a 40.889.692.902, 66.629.506.401 frente a 36.383.691.601 y 91.560.892.461 frente a 45.384.344.991 ([esquema_por_anio.csv](datos/esquema_por_anio.csv), líneas 16 a 19). Descriptivo y en pesos nominales. Evidencia débil: 2022 cubre medio año del régimen nuevo, la planilla de 2024 de la Ley 19.885 puede estar incompleta ([P-02](#p-02)) y no se verificó que los fines de las donatarias de los dos registros sean comparables.
- **Aviso de la propia página:** las cifras pueden variar por rectificaciones de las donatarias o por fiscalización, y la Subsecretaría de Hacienda no garantiza su exactitud ([instantánea](datos/fuentes/ley21440_donaciones_2026-10-23.txt)).

Estabilidad: el 2026-10-23 los cuatro archivos tienen la misma huella que en la instantánea del mismo día. El `Last-Modified` del CDN cambia hasta en 2 s entre pedidos (2024: 14:50:27 en la instantánea, 14:50:25 en el manifiesto), así que los cambios de archivo se detectan por sha256 y no por esa cabecera (comparación sin script en `analisis/`).

**Cruce por RUT con las planillas de la Ley 19.885** (cruce_donatarias_por_anio.csv, líneas 2 a 5 por año y línea 6 para la unión de 2022 a 2025). Mide si los registros se pueden enlazar, no la sustitución. Llave: el RUT sin puntos ni espacios y con K mayúscula; año: el del archivo. Ninguna fila de datos de los dos registros queda con llave no válida.

- **Donatarias en ambos registros:** 20 de 78, 35 de 283, 41 de 396 y 49 de 515 RUT de la Ley 21.440 están entre los RUT de institución de la planilla de la Ley 19.885 del mismo año; en la unión de 2022 a 2025, 95 de 659 (frente a 221 RUT de institución de la Ley 19.885).
- **Parte del monto que enlaza:** esos RUT reciben el 52,0 %, 34,5 %, 9,5 % y 18,6 % del `Valor` de la Ley 21.440 de cada año (32,2 % en la unión) y el 50,6 %, 72,5 %, 57,7 % y 79,4 % del monto de la Ley 19.885 (82,0 % en la unión). Los porcentajes son cocientes de columnas del CSV, calculados en Python y en R sin script en `analisis/`.
- La suma del monto de la Ley 19.885 en el cruce coincide con `suma_monto_total` de [esquema_por_anio.csv](datos/esquema_por_anio.csv) en los cuatro años: las dos salidas usan la misma regla de fila de datos.

Qué se puede medir con estos datos y qué no (inferencia del investigador a partir de las columnas y del cruce):

- **Se puede:** el monto y el número de filas por donataria (RUT) y por día desde 2022 en el régimen de la Ley 21.440, y su comparación con las planillas de la Ley 19.885 por RUT de institución y por fecha. La sustitución entre regímenes solo se mide así, agregada por donataria y período, y solo en las donatarias que están en ambos registros: en 2022 a 2025 son 95 RUT, que concentran el 82,0 % del monto de la Ley 19.885 y el 32,2 % del de la Ley 21.440. Para el resto de las donatarias de la Ley 21.440 no hay contraparte en las planillas de 2022 a 2025.
- **No se puede:** nada por donante (quién dona, si un mismo donante pasó de un régimen al otro, su tipo de contribuyente o su tramo); el número de donaciones, por los duplicados exactos; ni el beneficio tributario efectivamente usado.
- Comparar montos entre regímenes para medir la sustitución es una estimación sobre datos reales: espera a que [P-05 · Simulaciones de efecto conocido](#p-05) esté «verificada» ([D-05 · Niveles de resultado](DECISIONES.md#d-05)).

Queda: el diseño de la comparación de montos por donataria y período (en [P-05](#p-05)), y decidir si el año del cruce debe ser el de la fecha de cada donación ([D-10 · Criterio para completar o excluir](DECISIONES.md#d-10), regla 4) en vez del del archivo; eso depende de [P-06](#p-06).
