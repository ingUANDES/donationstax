# Blueprint

Pregunta de investigación: evaluar el impacto de los cambios regulatorios en las donaciones con fines sociales en Chile, con el alcance de [D-02 · Alcance](DECISIONES.md#d-02) y el monto donado como variable de resultado ([D-12 · Variable de resultado](DECISIONES.md#d-12)). Este archivo define el método y el backlog; el estado está en el [README](README.md#estado) y las decisiones en [DECISIONES.md](DECISIONES.md).

Serie **P-nn**: pendientes. Este es el único archivo donde se define el prefijo P- y donde viven sus ítems. Se toman en el orden que fije una persona ([D-06 · Canal de trabajo](DECISIONES.md#d-06)). Citar desde otro archivo: `[P-01 · Inventario normativo](BLUEPRINT.md#p-01)`.

## Etapas

| Etapa | Producto | Cómo se verifica | Backlog |
|---|---|---|---|
| E1 Inventario normativo | [normativa/cambios.csv](normativa/cambios.csv) y textos en `normativa/textos/` | [verifica_citas.py](analisis/verifica_citas.py) | [P-01](#p-01) |
| E2 Estructura de los datos | [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv), [datos/esquema_ley21440_por_anio.csv](datos/esquema_ley21440_por_anio.csv), [datos/cruce_donatarias_por_anio.csv](datos/cruce_donatarias_por_anio.csv), [datos/armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv), [datos/rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv), [datos/anio_evaluacion_por_anio.csv](datos/anio_evaluacion_por_anio.csv), [datos/duplicados_ley21440_por_anio.csv](datos/duplicados_ley21440_por_anio.csv) | esquema calculado en Python y en R, comparados ([contraste_datos.py](analisis/contraste_datos.py)) | [P-02 · Estructura de los datos](#p-02), [P-06 · Armonizar las planillas](#p-06), [P-07 · Datos del régimen de la Ley 21.440](#p-07) |
| E3 Precio fiscal por régimen | [datos/precio_fiscal_por_caso.csv](datos/precio_fiscal_por_caso.csv) y [datos/precio_fiscal_por_cambio.csv](datos/precio_fiscal_por_cambio.csv), de [precio_por_regimen.py](analisis/precio_por_regimen.py) y [precio_por_regimen.R](analisis/precio_por_regimen.R) | [contraste.py](analisis/contraste.py) contra valores calculados a mano | [P-04](#p-04) |
| E4 Diseños de estimación por cambio | [datos/simulacion_bunching.csv](datos/simulacion_bunching.csv), [datos/simulacion_eventos.csv](datos/simulacion_eventos.csv) y [datos/simulacion_vi.csv](datos/simulacion_vi.csv), de los scripts `sim_*` de `analisis/`, en Python y en R | simulación con efecto conocido, tolerancias fijadas antes de correr (aprobadas el 2026-10-27) y contraste ([contraste.py](analisis/contraste.py)) | [P-05](#p-05) |
| E5 Resultados y manuscrito | tablas y texto del artículo | solo cifras con nivel «verificada» ([D-05 · Niveles de resultado](DECISIONES.md#d-05)) | sin abrir |

## Verificación

El único punto de entrada es `make verificar` ([Makefile](Makefile)). Hoy comprueba que Python y R calculan el mismo precio fiscal sobre casos ficticios con valor esperado calculado a mano, que ambos contrastes fallan cuando se altera una salida, que cada fila normativa marcada `verificada` cita el texto de la norma literalmente, que la cabecera de cada texto del XML de BCN lista sus estructuras cortadas y que el texto completo de cada una, tomado del visor de LeyChile, empieza con el del XML ([P-01 · Inventario normativo](#p-01)), que el precio fiscal por régimen y año de [P-04 · Precio fiscal por régimen](#p-04) da lo mismo en Python, en R y en el cálculo a mano y coincide con la salida versionada, y que las simulaciones de efecto conocido de [P-05 · Simulaciones de efecto conocido](#p-05) dan lo mismo en Python y en R, coinciden con su salida versionada y no tienen más reglas incumplidas que las registradas como hallazgo, y que la serie de la UTM leída de las páginas del SII versionadas da lo mismo en Python y en R y coincide con la versionada ([Serie de la UTM](#serie-de-la-utm)). También corre [duplicados_ley21440.py](analisis/duplicados_ley21440.py) y [duplicados_ley21440.R](analisis/duplicados_ley21440.R) sobre una tabla ficticia ([duplicados_prueba.csv](analisis/fixtures/duplicados_prueba.csv)) y exige que den los conteos, montos y concentraciones calculados a mano ([duplicados_esperado_por_anio.csv](analisis/fixtures/duplicados_esperado_por_anio.csv) y sus pares de multiplicidad y concentración); esa parte no necesita python-calamine ni readxl. Una fila ficticia no es evidencia sobre la norma ni sobre los datos: los casos de [analisis/fixtures/](analisis/fixtures/precio_fiscal_prueba.csv) solo prueban la implementación.

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
- **Texto completo.** Sale del JSON del visor de LeyChile (`servicios-leychile.bcn.cl`, `get_norma_json`) para la misma `idVersion`, pasado a texto sin las notas de BCN. `bcn_versiones.py completar` lo escribe en `<texto>_visor.txt` solo si el texto del XML es su prefijo, y verifica_citas.py vuelve a comprobar ese prefijo. Que una cita tomada de esos archivos pueda quedar «verificada» depende de [D-21 · Texto completo de las estructuras que el XML de BCN corta](DECISIONES.md#d-21), aprobada el 2026-10-27. La edición electrónica del Diario Oficial no sirve para la Ley 20.630: la del 2012-09-27 dice «No existen publicaciones en esta edición en la fecha seleccionada» (consulta del 2026-10-26, no verificable desde el repo).
- **Contraste con el visor.** `python3 analisis/bcn_versiones.py contrastar` sobre los 44 textos del XML (2026-10-26, hora de Chile; usa la red y no está en `make verificar`) dio: «estructuras: 651 en 44 textos | igual: 633 | cortada: 15 | distinta: 3 | sin_visor: 0 | la regla marca 15 de 15 cortadas y 0 de 636 no cortadas». Las 15 cortadas están en los 10 textos que tienen `_visor.txt` en [normativa/textos/](normativa/textos/); siete no estaban en la tabla del [issue 28](https://github.com/ingUANDES/donationstax/issues/28). Las 3 distintas difieren por un U+FFFD del XML ([issue 37](https://github.com/ingUANDES/donationstax/issues/37)). Una regla de «termina a mitad de palabra» (en letra) habría marcado 73 estructuras que no están cortadas (40 títulos, 17 párrafos y 8 artículos que terminan en «Derogado» o «INCISO DEROGADO», entre otras) y habría perdido el corte en «4° y 5°» (comprobación sin script en `analisis/`).
- **El corte no siempre se repite.** El XML bajado otra vez el 2026-10-26 trae completas 4 de las 15: el art. 31 de la LIR en las versiones 2004-02-09, 2006-02-10 y 2017-01-01, y el art. 20 en la 2008-02-08. Las otras 11 salen cortadas en el mismo carácter (comprobación sin script en `analisis/`). La causa sigue sin verificar.
- **Efecto sobre la bisección.** Un corte que aparece o desaparece cambia la huella, y bcn_versiones.py lo toma como un cambio de texto. Según el visor, 8 de los 21 límites de versión de `lir_art20_31_52_v*.txt` no cambian los artículos 20, 31 ni 52; ninguna fila de cambios.csv cita esas versiones ([issue 37](https://github.com/ingUANDES/donationstax/issues/37)).
- **Lecturas que esperaban el texto.** El número 7) del artículo 1° de la Ley 20.630 está en [ley20630_v2026-10-26_visor.txt, L94-L96](normativa/textos/ley20630_v2026-10-26_visor.txt#L94-L96), y el número 32) del artículo 1° de la Ley 20.780, que modifica el art. 52 de la LIR, en [ley20780_v2026-10-26_visor.txt, L1232](normativa/textos/ley20780_v2026-10-26_visor.txt#L1232). Las dos lecturas se hicieron en el [issue 29](https://github.com/ingUANDES/donationstax/issues/29) y su resultado está en [P-04 · Precio fiscal por régimen](#p-04), «Tasa por año comercial».

Falta en este pendiente:

- El texto de la Ley 20.565 en sí (se conoce por la versión consolidada de BCN y la Circular N° 49; no se encontró su `idNorma`), el reglamento (Decreto Supremo 77 de 2009) y las circulares del SII sobre la Ley 21.440.
- La fecha de publicación de los reglamentos de la Ley 21.015. Cómo pasa el crédito a los impuestos finales del dueño desde 2017 (regímenes semi integrado y atribuido) queda fuera del precio según [D-23 · Precio fiscal a nivel del contribuyente que dona](DECISIONES.md#d-23).
- El impuesto del art. 21 de la LIR sobre la parte de las donaciones que excede el límite global del art. 10 ([issue 44](https://github.com/ingUANDES/donationstax/issues/44)).
- Los regímenes que el [artículo 10 de la Ley 19.885](normativa/textos/ley19885_v2024-08-24.txt) enumera y que [D-02 · Alcance](DECISIONES.md#d-02) deja fuera por ahora.
- Las versiones de la LIR que solo marcan un corte del XML y los tres U+FFFD del XML ([issue 37](https://github.com/ingUANDES/donationstax/issues/37)).
- La Circular N° 49 reemplaza la letra D-1 de la Circular N° 71: la vigencia de la fila L43, el numeral de la L45 y qué UTM mide el límite de 1.000 UTM desde 2012 ([issue 42](https://github.com/ingUANDES/donationstax/issues/42)).

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

**Precio fiscal por régimen (E3).** Estado: hecho el 2026-10-26 ([issue 21](https://github.com/ingUANDES/donationstax/issues/21)), a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)). Python, R y los valores calculados a mano coinciden en los 72 casos, y Python y R en las 52 comparaciones, con tolerancia 1e-12 (`make verificar`). Las cinco lecturas del [issue 29](https://github.com/ingUANDES/donationstax/issues/29) se hicieron el 2026-10-27 y ningún precio cambió. Queda sin calcular el impuesto del art. 21 sobre el exceso del límite global ([issue 44](https://github.com/ingUANDES/donationstax/issues/44)). El alcance (precio a nivel del contribuyente que dona) lo fija [D-23 · Precio fiscal a nivel del contribuyente que dona](DECISIONES.md#d-23).

Método. P = 1 − c − τg por peso donado: c es la fracción que se imputa como crédito, g la que se deduce de la base como gasto y τ la tasa marginal del impuesto que esa deducción reduce. Un caso es un régimen (ley, tramo, aporte al Fondo, límite) en un año comercial y para un tipo de contribuyente. El precio es por régimen y año, no por donante, porque las planillas no traen régimen ni tramo ([P-02](#p-02)). Cada c, g y τ sale de una fila verificada de [cambios.csv](normativa/cambios.csv), referida por norma, artículo y vigencia; los scripts no fijan parámetros. Que un caso no tenga beneficio (P = 1) es inferencia, y lo dice la nota del caso.

Producto:

- [normativa/precio_casos.csv](normativa/precio_casos.csv): los casos, con la fila de cambios.csv y la regla de cada c, g y τ (reglas en la cabecera de [precio_por_regimen.py](analisis/precio_por_regimen.py)). La columna `anio_tau` dice cómo se asignó τ al año comercial: «regla explícita» (una ley nombra el año comercial o el año tributario), «circular del SII» (la ley no nombra el año y una circular del SII lo fija), «texto permanente» (el mismo texto rige con las dos lecturas del párrafo siguiente), «sin verificar», «contrafactual» o «no aplica».
- [normativa/precio_comparaciones.csv](normativa/precio_comparaciones.csv): el par de casos (antes, después) con que se mide cada fila de cambios.csv.
- [precio_por_regimen.py](analisis/precio_por_regimen.py) y [precio_por_regimen.R](analisis/precio_por_regimen.R), que usan [precio_fiscal.py](analisis/precio_fiscal.py) y [precio_fiscal.R](analisis/precio_fiscal.R). Salidas: [datos/precio_fiscal_por_caso.csv](datos/precio_fiscal_por_caso.csv) y [datos/precio_fiscal_por_cambio.csv](datos/precio_fiscal_por_cambio.csv). La segunda tiene una línea por comparación, con el signo calculado y si coincide con `efecto_precio`, y una por cada fila sin comparación. Valores a mano: [precio_casos_esperado.csv](analisis/fixtures/precio_casos_esperado.csv).

Tasa por año comercial. La tasa que reduce una donación es la de las rentas del año comercial en que se hace. Cuando la ley no nombra el año, hay dos lecturas: el texto vigente durante ese año, o el vigente al declarar, por el [Código Tributario, art. 3 inc. 2](normativa/cambios.csv#L62): «los impuestos que deban pagarse a contar de esa fecha quedarán afectos a la nueva ley».

- 2011: [Ley 20.455, art. 1 letra a)](normativa/cambios.csv#L57), «A 20% (veinte por ciento) para las rentas que se perciban o devenguen durante el año calendario 2011.»
- 2012, 20 % (regla explícita). La letra b), 18,5 % ([L58](normativa/cambios.csv#L58)), fue derogada por la [Ley 20.630, art. 4 N° 1](normativa/cambios.csv#L59). El [art. 1 N° 7 letra a) de la Ley 20.630](normativa/cambios.csv#L74) cambia «17» por «20» en el art. 20 de la LIR «respecto del Impuesto de Primera Categoría que deba declararse y pagarse a contar del año tributario 2013». Lo hace desde la vigencia del [número 17)](normativa/cambios.csv#L75), «a partir del 1 de enero de 2013» (texto del visor, [ley20630_v2026-10-26_visor.txt, L96 y L340](normativa/textos/ley20630_v2026-10-26_visor.txt#L96), porque el XML de BCN corta el art. 1). La [Circular SII N° 48 de 2012](normativa/cambios.csv#L76) lo lee igual: las rentas «percibidas o devengadas en el año comercial 2012» se gravan con 20 %. El caso con 17 % (precio 0,415 en vez de 0,40, [datos/precio_fiscal_por_caso.csv, L19-L20](datos/precio_fiscal_por_caso.csv#L19-L20)) queda como contrafactual. La misma circular aplica 18,5 % a las rentas esporádicas que se declaran dentro de 2012. Inferencia: no tocan el crédito por donaciones, que se imputa en la declaración anual.
- 2014 a 2017: tasas por año comercial de la [Ley 20.780, art. cuarto transitorio](normativa/cambios.csv#L63) y, para la letra B del art. 14, su [inciso 2](normativa/cambios.csv#L64). Desde 2020, el texto nuevo del art. 20 rige «únicamente a partir del año comercial 2020» ([Ley 21.210, art. octavo transitorio](normativa/cambios.csv#L65)).
- Pro Pyme: [Ley 21.256, art. 1](normativa/cambios.csv#L66) (2020 a 2024, texto modificado por la [Ley 21.578](normativa/cambios.csv#L67)) y [Ley 21.755, art. 25](normativa/cambios.csv#L68) (2025). Esta última exige que, al cierre de 2025, la cotización del artículo cuarto transitorio de la Ley 21.735 sea de 1 %. Según el texto publicado en el Diario Oficial del 2025-03-26 ([ley21735_do_2025-03-26.txt](normativa/textos/ley21735_do_2025-03-26.txt)), la tasa es 1,0 % desde el primer día del quinto mes siguiente a la publicación ([letra a)](normativa/cambios.csv#L79)) y 3,5 % desde el décimo séptimo ([letra b)](normativa/cambios.csv#L80)). Contando los meses (inferencia), eso es del 2025-08-01 al 2026-07-31, así que la condición se cumple al cierre de 2025. BCN no sirvió esta ley (el XML respondió HTTP 500 el 2026-10-27 y el visor trae el texto vacío). Si se modificó después de publicada: sin verificar.
- Global complementario ([L53 a L56](normativa/cambios.csv#L53-L56)). El [art. 1 N° 32 de la Ley 20.780](normativa/textos/ley20780_v2026-10-26_visor.txt#L1232) deja el tramo superior del art. 52 en 35 % y rige «a contar del 1 de enero de 2017» (artículo primero transitorio, que no lo pone entre sus excepciones), sin nombrar el año comercial. El SII aplica 40 % a las rentas de 2016: la tabla del año tributario 2017 de la [Circular N° 1 de 2017](normativa/cambios.csv#L77) termina en «0,40», y la de 35 % aparece en la del año tributario 2018 ([Circular N° 1 de 2018](normativa/cambios.csv#L78)) (circular del SII). Que el año tributario 2017 corresponda al año comercial 2016 es inferencia, apoyada en la regla de declaración en abril del año siguiente que cita la [Circular N° 48 de 2012](normativa/cambios.csv#L76). El precio de 2016 en el tramo superior es 0,30, y el 0,325 con 35 % queda como contrafactual ([datos/precio_fiscal_por_caso.csv, L64-L65](datos/precio_fiscal_por_caso.csv#L64-L65)). La comparación de la escala de 2017 se mide ahora desde 2016 ([datos/precio_fiscal_por_cambio.csv, L62](datos/precio_fiscal_por_cambio.csv#L62)). La página «¿Cuáles son los impuestos directos?» del SII Educa (https://www.sii.cl/siieduca/aprende-con-nosotros/cuales-son-los-impuestos-directos.html, consulta del 2026-10-27; no versionada, no verificable desde el repo) dice lo mismo: la escala de 35 % «Rige a contar del año tributario 2018».

Signo y magnitud ([datos/precio_fiscal_por_cambio.csv](datos/precio_fiscal_por_cambio.csv)):

- Los cambios de la Ley 19.885 mueven el precio mucho más que las tasas. En 2010, la Ley 20.316 baja de 1 a 0,415 el precio de quien da menos de 1.000 UTM sin aportar al Fondo ([L6](datos/precio_fiscal_por_cambio.csv#L6)) y lo sube de 0,415 a 1 para quien da a una sola institución ([L7](datos/precio_fiscal_por_cambio.csv#L7)). En 2012, la Ley 20.565 lo baja de 1 a 0,40 para ese donante y de 0,52 a 0,40 en el tramo hasta 1.000 UTM de quien supera el umbral sin Fondo ([L19-L20](datos/precio_fiscal_por_cambio.csv#L19-L20)).
- En la Ley 19.885 (g = 0,5), cada punto de tasa de primera categoría baja el precio en medio punto: 0,415 en 2010, 0,40 de 2011 a 2013, 0,365 en la letra B desde 2018 y en la letra A desde 2020 ([L51-L56](datos/precio_fiscal_por_cambio.csv#L51-L56), [L64](datos/precio_fiscal_por_cambio.csv#L64), [L70-L74](datos/precio_fiscal_por_cambio.csv#L70-L74)). Las tasas Pro Pyme rebajadas lo suben: 0,45 con 10 % y 0,4375 con 12,5 %, frente a 0,375 con la tasa permanente ([L76-L79](datos/precio_fiscal_por_cambio.csv#L76-L79)).
- La Ley 21.440 deduce de la base (c = 0, g = 1). Para una empresa de la letra A en 2023 el precio es 0,73, el doble del 0,365 de la Ley 19.885 dentro de sus límites; sobre el límite del art. 10, que no se le aplica, baja de 1 a 0,73 ([L39-L40](datos/precio_fiscal_por_cambio.csv#L39-L40)). Para una persona en el tramo superior es 0,60 ([datos/precio_fiscal_por_caso.csv, L71](datos/precio_fiscal_por_caso.csv#L71)).
- Tres comparaciones dan un signo distinto del `efecto_precio` declarado, porque el cambio actúa fuera del margen medido: con el quiebre de 2012 el precio marginal sobre 1.000 UTM no cambia (0,52 y 0,48: [L22](datos/precio_fiscal_por_cambio.csv#L22), [L26](datos/precio_fiscal_por_cambio.csv#L26)); lo que baja es el tramo inferior. La escala de 2013 no toca el tramo superior de 40 % ([L61](datos/precio_fiscal_por_cambio.csv#L61)).

Crédito y límites en el margen. Para una empresa de la letra A en 2025 ([datos/precio_fiscal_por_caso.csv, L49-L52](datos/precio_fiscal_por_caso.csv#L49-L52)), la donación marginal cuesta:

- 0,365 mientras el crédito del año no llega al tope;
- 0,73 cuando llegó. El crédito no puede exceder 14.000 UTM al año ([art. 1 N° 12](normativa/cambios.csv#L13)), y «Aquella parte de las donaciones que no pueda ser utilizada como crédito, se considerará un gasto necesario para producir la renta» ([N° 14](normativa/cambios.csv#L14)). Si el N° 14 cubre también el crédito que excede el impuesto, el precio es el mismo (inferencia);
- al menos 1 sobre el límite global del 5 % de la renta líquida imponible ([art. 10 inc. 1](normativa/cambios.csv#L15)), que se aplica «ya sea que el beneficio tributario consista en un crédito contra el impuesto de Primera Categoría, o bien en la posibilidad de deducir como gasto la donación». La ley no dice qué pasa con el exceso. El SII lo califica como gasto rechazado: «constituye para los efectos tributarios un gasto rechazado» ([Circular N° 71 de 2010](normativa/cambios.csv#L81)), del N° 1 del art. 33 de la LIR y sujeto al art. 21 «en la medida que se cumplan los requisitos» ([Circular N° 49 de 2012](normativa/cambios.csv#L82)). El precio de esa parte es entonces 1 más el impuesto del art. 21 que recaiga sobre la empresa, y los casos usan 1 como cota inferior. Calcularlo pide leer las versiones del art. 21 ([issue 44](https://github.com/ingUANDES/donationstax/issues/44)). Para el texto de 2003 (caso `e2009_sobre_limite_4_5_rli`), las circulares no lo dicen: sin verificar.

El tope de crédito y el límite global son entonces quiebres que suben el precio marginal (inferencia). Los trabajadores del art. 43 N° 1 recuperan el exceso del crédito («si resultare un exceso, éste se devolverá»: [art. 1 bis inc. 2](normativa/cambios.csv#L71)), y su precio es 0,5 aunque el crédito supere el impuesto.

Evidencia débil y alcance:

- El precio es el de la empresa. No incluye cómo el crédito y el menor impuesto de primera categoría pasan a los impuestos finales del dueño, que desde 2017 dependen del régimen del art. 14. Tampoco cubre el Pro Pyme transparente (art. 14 letra D N° 8) ni el art. 14 ter. Es un límite del alcance, no una lectura pendiente, según [D-23 · Precio fiscal a nivel del contribuyente que dona](DECISIONES.md#d-23), que lista sus consecuencias. El costo para el dueño no se calculó y puede ser distinto.
- El caso con pérdida tributaria de 2020 es una cota: supone que la pérdida se usa después, a la misma tasa y sin descontar.
- La vigencia de la Ley 21.015 sigue sin verificar ([L41](normativa/cambios.csv#L41)); su caso solo usa la tasa de 2025.
- Las personas se calculan en el primer tramo y en el superior. A qué contribuyentes del global complementario se aplicaba el límite del art. 10 antes de 2020 está sin verificar ([L17](normativa/cambios.csv#L17), [L28](normativa/cambios.csv#L28)).
- Las comparaciones de los programas de adicciones de 2010 son entre fines en un mismo año, no en el tiempo.

### P-05

**Simulaciones de efecto conocido (E4).** Estado: hecho el 2026-10-27 ([issue 22](https://github.com/ingUANDES/donationstax/issues/22)), a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)). Python y R coinciden en las tres simulaciones con la tolerancia de [D-15 · Tolerancia entre Python y R para las simulaciones](DECISIONES.md#d-15); la diferencia relativa máxima es $9{,}9 \cdot 10^{-14}$. Las tolerancias de recuperación están en [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](DECISIONES.md#d-16), [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](DECISIONES.md#d-17) y [D-18 · Tolerancias de la simulación de variables instrumentales](DECISIONES.md#d-18). Se fijaron antes de correr (commit `faef689`) y el usuario las aprobó el 2026-10-27 sin cambios. Variables instrumentales cumple todas sus reglas; subirla a «verificada» se evalúa aparte, según [D-05 · Niveles de resultado](DECISIONES.md#d-05), y no se hace aquí. Bunching y estudio de eventos dejan siete reglas incumplidas, que son hallazgos ([issue 34](https://github.com/ingUANDES/donationstax/issues/34)). Ninguna estimación sobre datos reales. La serie de la UTM con que se aplicarían está en [Serie de la UTM](#serie-de-la-utm), y la regla para convertir cada donación es [D-26 · UTM que se aplica a cada donación](DECISIONES.md#d-26) ([issue 33](https://github.com/ingUANDES/donationstax/issues/33)).

Método común:

- Las dos implementaciones generan las mismas extracciones con [azar.py](analisis/azar.py) y [azar.R](analisis/azar.R), y la semilla de cada diseño está escrita en su script.
- El resultado es el logaritmo del monto en UTM ([D-12 · Variable de resultado](DECISIONES.md#d-12)). La unidad es la donación por donante, institución y año en bunching, y el donante-año en los otros dos diseños.
- Precios de [precio_fiscal_por_caso.csv](datos/precio_fiscal_por_caso.csv) ([P-04](#p-04)) y proporción de filas sin RUT de [rut_completado_por_anio.csv](datos/rut_completado_por_anio.csv).

Producto:

- Scripts: [sim_bunching.py](analisis/sim_bunching.py), [sim_eventos.py](analisis/sim_eventos.py) y [sim_vi.py](analisis/sim_vi.py), con sus contrapartes `.R` y las utilidades de [sim_comun.py](analisis/sim_comun.py) y [sim_comun.R](analisis/sim_comun.R).
- Salidas versionadas: [simulacion_bunching.csv](datos/simulacion_bunching.csv), [simulacion_eventos.csv](datos/simulacion_eventos.csv) y [simulacion_vi.csv](datos/simulacion_vi.csv). Traen una línea por cifra (clave, valor, objetivo, regla, cumple).
- Hallazgos: las reglas incumplidas quedan en [sim_hallazgos.csv](analisis/fixtures/sim_hallazgos.csv). Cada script falla con `AssertionError` si aparece otra o si una registrada deja de fallar. Registrar los hallazgos en vez de detener `make verificar` fue elección del investigador; el usuario la aceptó el 2026-10-27 (conversación de Claude Code, no verificable desde el repo): «Si acepto el registro de fallas esperadas de las simulaciones».

Riesgos de [Riesgos de identificación a testear](#riesgos-de-identificación-a-testear) y caso que cubre cada uno:

| Riesgo | Caso | Resultado |
|---|---|---|
| Variables instrumentales con la alícuota por año | [D-18 · Tolerancias de la simulación de variables instrumentales](DECISIONES.md#d-18), puntos 3 y 4 | Con efectos fijos de año, el residuo del instrumento es $4 \cdot 10^{-16}$: no identificado. Sin ellos, la estimación se aleja 48,3 del valor verdadero ([L7-L9](datos/simulacion_vi.csv#L7-L9)) |
| Diferencias en diferencias con tratamiento por el nivel previo | [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](DECISIONES.md#d-17), punto 2 | Sesgo $b = -0{,}458$ con un efecto de 0,15. Los coeficientes de 2012 y 2013 lo muestran, así que la prueba de tendencias previas lo detecta ([L18-L25](datos/simulacion_eventos.csv#L18-L25)). El de 2012 se sale de la tolerancia: hallazgo |
| Grupo de control de 200 a 900 UTM | [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](DECISIONES.md#d-17), punto 3 | $\hat\delta = 0{,}0700$ frente a $\beta - \pi_c\beta_c = 0{,}0789$, con $\pi_c = 0{,}142$: el sesgo hacia cero es de 0,071 en un efecto de 0,15 ([L26-L28](datos/simulacion_eventos.csv#L26-L28)) |
| Bunching en un quiebre con donantes del Fondo Mixto | [D-16 · Tolerancias de la simulación de bunching en el quiebre de 1.000 UTM](DECISIONES.md#d-16) | Sin los donantes del Fondo, $\hat\varepsilon = 0{,}5126$ (verdadero 0,5; error estándar 0,0310) y $\hat B$ = 10.880 frente a 10.696 ([L8-L12](datos/simulacion_bunching.csv#L8-L12)). Con todos, la fórmula del quiebre simple da 0,4025, cerca de $0{,}8\,\varepsilon$ ([L37-L39](datos/simulacion_bunching.csv#L37-L39)). Sensibilidad: 19 de 24 combinaciones dentro del 15 %; las 5 que no, con ventana desde $k - 0{,}10$ y orden 4 o más (hasta 0,628): hallazgo |
| Panel de donantes sin RUT | [D-17 · Tolerancias de la simulación de estudio de eventos y diferencias en diferencias](DECISIONES.md#d-17), punto 4; [D-18 · Tolerancias de la simulación de variables instrumentales](DECISIONES.md#d-18), punto 2 | Solo el 43,6 % de los donantes de 2014 conserva su identificador en 2015 ([L43](datos/simulacion_eventos.csv#L43)). Con marcas al azar, las estimaciones no cambian más allá del muestreo: estudio de eventos exógeno ([L29-L35](datos/simulacion_eventos.csv#L29-L35)) y MC2E de 0,946 frente a 0,934 sin error ([L5](datos/simulacion_vi.csv#L5), [L11](datos/simulacion_vi.csv#L11)) |
| Tasa marginal constante de 27 % | [D-18 · Tolerancias de la simulación de variables instrumentales](DECISIONES.md#d-18), punto 3 | El precio con 27 % para todos no tiene variación tras los efectos fijos (residuo 0) |

En variables instrumentales, MCO con efectos fijos sobrestima la elasticidad en 0,60, porque la pérdida sube el precio efectivo y baja la donación. MC2E con el precio legal por régimen y año la recupera (0,934; $F$ = 1.018). Inferencia: la variación entre regímenes desde 2017 (art. 14 letras A y B; Pro Pyme desde 2020) es la que identifica, y las planillas no traen el régimen ([P-02](#p-02)).

Evidencia débil:

- Una sola extracción por diseño. Una comprobación con otras 20 semillas, sin script versionado y por eso no verificada, da dos resultados. En bunching, la especificación principal se sale del 10 % en 5 de 20 semillas, y con orden 3 el estimador tiene un tercio de la dispersión que con orden 5 o 6. En el caso de nivel previo, ninguno de 140 coeficientes pasa de 0,03. Las cifras y opciones están en el [issue 34](https://github.com/ingUANDES/donationstax/issues/34). Inferencia: el bunching recuperó el efecto con la semilla fijada en parte por suerte, y la falla de estudio de eventos es un extremo de muestreo.
- El mecanismo del error por RUT es inferencia del investigador: marcas al azar, una fila por donante-año y sin homónimos. Si la falta de RUT depende del tamaño o del tipo de donante, podría sesgar, y eso no se simuló.
- Los parámetros de los casos los eligió el investigador; solo los precios y la proporción sin RUT vienen del repo. Tres de los precios que usan (`e2012_hasta_1000` y `e2012_exceso_sin_fondo` en bunching, `e2025_pro_pyme` en variables instrumentales) dependían de lecturas del [issue 29](https://github.com/ingUANDES/donationstax/issues/29). Esas lecturas se hicieron el 2026-10-27 y confirmaron los valores usados, así que las simulaciones no cambian.
- Solo el bunching tiene error estándar; los otros diseños se juzgan por la estimación puntual.

#### Serie de la UTM

Dato que pide el [issue 33](https://github.com/ingUANDES/donationstax/issues/33) para aplicar estos diseños a datos reales. La regla con que se convierte cada donación es [D-26 · UTM que se aplica a cada donación](DECISIONES.md#d-26).

Producto:

- [descargar_utm.py](analisis/descargar_utm.py) baja del SII una página por año: de 2008 a 2012, de `www.sii.cl/pagina/valores/utm/`, y de 2013 a 2025, de `www.sii.cl/valores_y_fechas/utm/`. Las guarda tal como llegan en [datos/fuentes/sii_utm/](datos/fuentes/sii_utm/), con el manifiesto [datos/descargas_utm.csv](datos/descargas_utm.csv) (URL, bytes, sha256, `Last-Modified` y fecha de descarga, 2026-10-27). Usa la red y no está en `make verificar`.
- [utm.py](analisis/utm.py) y [utm.R](analisis/utm.R) leen esas páginas con las reglas de la cabecera de utm.py. Escriben [datos/utm_mensual.csv](datos/utm_mensual.csv), con 216 meses (año, mes, UTM, UTA y la variación mensual del IPC publicada), y [datos/utm_por_anio.csv](datos/utm_por_anio.csv), con la UTM de diciembre, la mínima y la máxima de cada año y sus razones.
- `make verificar` exige que la serie de Python y la de R sean idénticas byte a byte, que la tabla por año coincida con tolerancia 1e-12 y que las dos sean las versionadas. Nivel: contrastado ([D-05 · Niveles de resultado](DECISIONES.md#d-05)).

Comprobaciones, con `AssertionError`: la huella de cada página es la del manifiesto; UTA = 12 × UTM en los 216 meses; y, desde marzo de 2008, cada UTM es la del mes anterior reajustada por la variación del IPC publicada para el segundo mes anterior, con un margen de 0,05 % más 1 peso por el redondeo. Las tres se cumplen. Que esa sea la regla legal del reajuste es inferencia de la serie: el Código Tributario no está en `normativa/textos/` (sin verificar).

Lo que muestra la serie:

- La UTM baja en algunos meses. En 2009, la de diciembre (36.863) es menor que la de enero (37.614) ([utm_por_anio.csv, L3](datos/utm_por_anio.csv#L3)); en 2025, la de febrero (67.294) es menor que la de enero (67.429).
- En los años de la ventana empírica, la UTM de diciembre supera a la mínima del año entre un 1,8 % (2017) y un 12,3 % (2022). Su efecto sobre la regla está en [D-26 · UTM que se aplica a cada donación](DECISIONES.md#d-26).
- Dos variaciones no siguen el formato del resto: «0» en enero de 2008 y «0.6», con punto, en diciembre de 2011. La cabecera de utm.py las admite.
- El `Last-Modified` cambia entre pedidos con la misma huella (2015: 11:45:10 en una descarga exploratoria del mismo día y 11:22:43 en el manifiesto; comparación sin script). Los cambios de página se detectan por sha256.

Evidencia débil:

- La fuente son páginas HTML del SII, no una norma ni el Diario Oficial. Que sean el valor oficial para efectos tributarios está sin verificar fuera del SII.
- El IPC en puntos no se leyó, porque cambia de base entre años (nota (6) de las páginas).

### P-06

**Armonizar las planillas (E2).** Estado: hecho el 2026-10-24 ([issue 9](https://github.com/ingUANDES/donationstax/issues/9)), a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)). Python y R coinciden en las tablas y celda por celda en las 7.877.362 filas. Se aplicaron las ocho reglas de [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10): la armonización (reglas 1 a 6 y 8) y la regla 7, completar el RUT por nombre. Los casos que [D-10 · Criterio para completar o excluir](DECISIONES.md#d-10) no cubre están en [D-13 · Casos que el criterio para completar o excluir no cubre](DECISIONES.md#d-13), aprobada el 2026-10-26 con un cambio en el punto 1. El año de la donación sigue [D-14 · Año de la donación cuando la fecha cae en otro año](DECISIONES.md#d-14); los scripts aplican las dos desde el [issue 20](https://github.com/ingUANDES/donationstax/issues/20).

Producto: [armonizar.py](analisis/armonizar.py) y [armonizar.R](analisis/armonizar.R), con la regla exacta de cada caso en la cabecera del de Python. Salidas:

- `data_local/armonizado/py/` y `data_local/armonizado/r/`: una fila por fila de cada planilla. Traen nombres, así que no se versionan.
- [datos/armonizacion_por_anio.csv](datos/armonizacion_por_anio.csv): cuántas filas toca cada regla, por año del archivo (líneas 2 a 19: 2008 a 2025).
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

**Datos del régimen de la Ley 21.440 (E2).** Estado: perfil de esquema y cruce por donataria hechos el 2026-10-23 ([issue 10](https://github.com/ingUANDES/donationstax/issues/10)), a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)): Python y R coinciden en los cuatro años y en el cruce. Los duplicados exactos se describieron el 2026-10-27 ([issue 16](https://github.com/ingUANDES/donationstax/issues/16)), también a nivel *contrastado*, y su regla, aprobada el 2026-10-27, está en [D-20 · Duplicados exactos de la nómina de la Ley 21.440](DECISIONES.md#d-20). Responde al [issue 6](https://github.com/ingUANDES/donationstax/issues/6) en lo que toca a la Ley 21.440; el Certificado N° 60 de la Ley 21.015 sigue pedido allí.

Fuente: [donacionesley21440.gob.cl/donaciones](https://donacionesley21440.gob.cl/donaciones), con [instantánea del texto de la página](datos/fuentes/ley21440_donaciones_2026-10-23.txt). Cada página por año responde con un redireccionamiento a un `.xlsx` en `cdn.hacienda.cl` cuya URL va firmada y vence a los pocos minutos. [descargar_ley21440.py](analisis/descargar_ley21440.py) pide la página del año sin seguir el redireccionamiento, baja el archivo de inmediato y exige que su sha256 sea la huella que trae la ruta del CDN; deja el manifiesto [datos/descargas_ley21440.csv](datos/descargas_ley21440.csv) (página, ruta sin la firma, nombre original, tamaño, sha256, `Last-Modified`). Ambos hosts deben estar autorizados.

Producto: [datos/esquema_ley21440_por_anio.csv](datos/esquema_ley21440_por_anio.csv), una fila por año (líneas 2 a 5: 2022 a 2025), de [esquema_ley21440.py](analisis/esquema_ley21440.py) y [esquema_ley21440.R](analisis/esquema_ley21440.R); y [datos/cruce_donatarias_por_anio.csv](datos/cruce_donatarias_por_anio.csv), de [cruce_donatarias.py](analisis/cruce_donatarias.py) y [cruce_donatarias.R](analisis/cruce_donatarias.R). Los dos pares toman las reglas de lectura de [esquema_datos.py](analisis/esquema_datos.py) y [contraste_datos.py](analisis/contraste_datos.py) los compara; sus reglas propias (fila de datos, fila superior, RUT bien formado y Módulo 11, llave del cruce) están en la cabecera de cada script de Python. `make verificar-datos-21440` y `make verificar-cruce` los corren, y `make verificar-datos` incluye los dos.

Lo que muestran los archivos (cifras de esquema_ley21440_por_anio.csv salvo que se diga otra cosa; lo marcado como inferencia es del investigador). Las cifras exploratorias de la versión anterior de esta sección se reproducen, salvo la del tamaño frente a la Ley 19.885, que se corrige; la fila superior, los valores 0, el RUT con espacio y `Nac/Ext.` son nuevos:

- **Columnas:** `RUT Donataria`, `Donataria`, `Valor` y `Fecha`, más `Nac/Ext.` en 2024 y 2025. **No hay donante ni su RUT.** Una hoja por archivo y el encabezado en la fila 2.
- **Filas de datos:** 414, 14.291, 428.151 y 533.650, de 2022 a 2025; ninguna fila bajo el encabezado queda fuera.
- **Fila superior:** trae, sobre la columna `Valor`, un total igual a la suma de `Valor` en los cuatro años (8.073.239.854; 44.245.732.763; 66.629.506.401; 91.560.892.461; pesos nominales). La de 2025 dice además «Información actualizada al 07.07.26, en valores nominales.». Los nombres de 2022 a 2024 llevan «20.11.25» y el de 2025, «al 07.07.26» ([descargas_ley21440.csv](datos/descargas_ley21440.csv)).
- **Duplicados exactos:** 25, 10.302, 417.446 y 510.089 filas (6,0 %, 72,1 %, 97,5 % y 95,6 %). Sin donante ni identificador de donación no se distingue una donación repetida de un registro repetido. El total de la fila superior es la suma con duplicados en los cuatro años (lo comprueba [duplicados_ley21440.py](analisis/duplicados_ley21440.py)); inferencia: quien publica los cuenta como donaciones. Detalle en [«Duplicados exactos»](#p-07-duplicados), más abajo.
- **Valores:** enteros y no negativos; 8 filas de 2023 valen 0. Máximos: 3.484.010.061, 9.714.000.000, 4.194.000.000 y 3.628.270.000.
- **Fechas:** todas son fechas de Excel y caen en el año del archivo. 7 de las 414 filas de 2022 son anteriores al 2022-07-01 (la primera, 2022-03-17), fecha en que empiezan los beneficios según el artículo primero transitorio de la Ley 21.440 ([normativa/cambios.csv](normativa/cambios.csv), línea 35).
- **RUT de la donataria:** 78, 283, 396 y 515 distintos; cada RUT tiene un solo nombre y cada nombre un solo RUT, y todos los bien formados pasan Módulo 11. En 2025, 48 filas de una donataria traen el RUT con un espacio final.
- **`Nac/Ext.`:** vacío salvo en 124 filas de 2024 y 212 de 2025, que dicen «Extranjero». Ni el archivo ni la página dicen qué significa (sin verificar).
- **Tamaño frente a la Ley 19.885 (corrige la versión anterior, que decía 2024 y 2025):** la suma de `Valor` supera a la de `MONTOTOTALDONACION` de las planillas de la Ley 19.885 en 2023, 2024 y 2025, no en 2022: 8.073.239.854 frente a 41.254.575.514, 44.245.732.763 frente a 40.889.692.902, 66.629.506.401 frente a 36.383.691.601 y 91.560.892.461 frente a 45.384.344.991 ([esquema_por_anio.csv](datos/esquema_por_anio.csv), líneas 16 a 19). Descriptivo y en pesos nominales. Evidencia débil: 2022 cubre medio año del régimen nuevo, la planilla de 2024 de la Ley 19.885 puede estar incompleta ([P-02](#p-02)) y no se verificó que los fines de las donatarias de los dos registros sean comparables.
- **Aviso de la propia página:** las cifras pueden variar por rectificaciones de las donatarias o por fiscalización, y la Subsecretaría de Hacienda no garantiza su exactitud ([instantánea](datos/fuentes/ley21440_donaciones_2026-10-23.txt)).

<a id="p-07-duplicados"></a>**Duplicados exactos** ([issue 16](https://github.com/ingUANDES/donationstax/issues/16)). [duplicados_ley21440.py](analisis/duplicados_ley21440.py) y [duplicados_ley21440.R](analisis/duplicados_ley21440.R), contrastados (`make verificar-duplicados-21440`, dentro de `make verificar-datos`), escriben [datos/duplicados_ley21440_por_anio.csv](datos/duplicados_ley21440_por_anio.csv) (líneas 2 a 5 por año, 6 para 2022 a 2025), [datos/duplicados_ley21440_multiplicidad.csv](datos/duplicados_ley21440_multiplicidad.csv) (por año y número de copias) y [datos/duplicados_ley21440_concentracion.csv](datos/duplicados_ley21440_concentracion.csv) (por año y dimensión). Las definiciones están en la cabecera del script de Python: copia extra es cada fila igual a una anterior del mismo archivo, y multiplicidad, el número de filas iguales del archivo, ella incluida. No se excluye nada. La sensibilidad del monto y la regla están en [D-20 · Duplicados exactos de la nómina de la Ley 21.440](DECISIONES.md#d-20). Lo que muestran las tablas, de 2022 a 2025:

- **Multiplicidad:** las filas repetidas forman 14, 546, 3.296 y 6.491 grupos de filas idénticas. La fila más repetida tiene 9, 1.545, 4.539 y 9.545 copias, y la mitad de las filas repetidas está en grupos de al menos 2, 1.429, 2.520 y 2.320 (columnas `n_grupos_repetidos`, `multiplicidad_max` y `multiplicidad_mediana_filas_repetidas`). La distribución completa está en la tabla de multiplicidad: líneas 2 a 6 (2022), 7 a 56 (2023), 57 a 425 (2024) y 426 a 874 (2025).
- **Donatarias:** tienen copias extra 3 de 78 donatarias, 36 de 283, 70 de 396 y 91 de 515. La que más tiene reúne el 68,0 %, 92,0 %, 99,7 % y 79,2 % de las copias, y una o dos donatarias reúnen el 90 % cada año. Como referencia, la donataria con más filas reúne el 25,6 %, 70,1 %, 98,2 % y 76,6 % de todas las filas (tabla de concentración, líneas 2, 6, 10 y 14).
- **Fechas:** en 2022 y 2023 las copias caen en pocas fechas: el 2022-12-31 reúne el 68,0 % de las de 2022, y dos fechas, la mitad de las de 2023 (la primera, 2023-12-05, el 37,4 %). En 2024 y 2025 hay copias en 360 de las 366 fechas y en las 365, y hacen falta 15 y 13 fechas para reunir la mitad de las copias, tantas como para la mitad de todas las filas. En 2025, el 2025-12-31 reúne el 20,5 % de las copias, y un solo par donataria-fecha, el 18,4 % (líneas 3, 7, 11, 15 y 16).
- **Valores:** tres valores reúnen la mitad de las copias en 2022, 2024 y 2025, y dos en 2023. El valor con más copias es 30.000 pesos en 2022, 120.000 en 2023 y 5.000 en 2024 y 2025, con el 32,0 %, 36,0 %, 31,7 % y 28,0 % de ellas (líneas 5, 9, 13 y 17).

Estabilidad: el 2026-10-23 los cuatro archivos tienen la misma huella que en la instantánea del mismo día. El 2026-10-27, al volver a descargarlos para el [issue 16](https://github.com/ingUANDES/donationstax/issues/16), los cuatro tienen la misma huella y la misma ruta del CDN que el [manifiesto](datos/descargas_ley21440.csv) (comparación sin script en `analisis/`). El `Last-Modified` del CDN cambia hasta en 2 s entre pedidos (2024: 14:50:27 en la instantánea, 14:50:25 en el manifiesto), así que los cambios de archivo se detectan por sha256 y no por esa cabecera (comparación sin script en `analisis/`).

**Cruce por RUT con las planillas de la Ley 19.885** (cruce_donatarias_por_anio.csv, líneas 2 a 5 por año y línea 6 para la unión de 2022 a 2025). Mide si los registros se pueden enlazar, no la sustitución. Llave: el RUT sin puntos ni espacios y con K mayúscula; año: el del archivo. Ninguna fila de datos de los dos registros queda con llave no válida.

- **Donatarias en ambos registros:** 20 de 78, 35 de 283, 41 de 396 y 49 de 515 RUT de la Ley 21.440 están entre los RUT de institución de la planilla de la Ley 19.885 del mismo año; en la unión de 2022 a 2025, 95 de 659 (frente a 221 RUT de institución de la Ley 19.885).
- **Parte del monto que enlaza:** esos RUT reciben el 52,0 %, 34,5 %, 9,5 % y 18,6 % del `Valor` de la Ley 21.440 de cada año (32,2 % en la unión) y el 50,6 %, 72,5 %, 57,7 % y 79,4 % del monto de la Ley 19.885 (82,0 % en la unión). Los porcentajes son cocientes de columnas del CSV, calculados en Python y en R sin script en `analisis/`.
- La suma del monto de la Ley 19.885 en el cruce coincide con `suma_monto_total` de [esquema_por_anio.csv](datos/esquema_por_anio.csv) en los cuatro años: las dos salidas usan la misma regla de fila de datos.

Qué se puede medir con estos datos y qué no (inferencia del investigador a partir de las columnas y del cruce):

- **Se puede:** el monto y el número de filas por donataria (RUT) y por día desde 2022 en el régimen de la Ley 21.440, y su comparación con las planillas de la Ley 19.885 por RUT de institución y por fecha. La sustitución entre regímenes solo se mide así, agregada por donataria y período, y solo en las donatarias que están en ambos registros: en 2022 a 2025 son 95 RUT, que concentran el 82,0 % del monto de la Ley 19.885 y el 32,2 % del de la Ley 21.440. Para el resto de las donatarias de la Ley 21.440 no hay contraparte en las planillas de 2022 a 2025.
- **No se puede:** nada por donante (quién dona, si un mismo donante pasó de un régimen al otro, su tipo de contribuyente o su tramo); el número de donaciones, por los duplicados exactos (a lo más, las cotas de [D-20 · Duplicados exactos de la nómina de la Ley 21.440](DECISIONES.md#d-20)); ni el beneficio tributario efectivamente usado.
- Comparar montos entre regímenes para medir la sustitución es una estimación sobre datos reales: espera a que [P-05 · Simulaciones de efecto conocido](#p-05) esté «verificada» ([D-05 · Niveles de resultado](DECISIONES.md#d-05)).

Queda: el diseño de la comparación de montos por donataria y período (en [P-05](#p-05)), y decidir si el año del cruce debe ser el de la fecha de cada donación ([D-10 · Criterio para completar o excluir](DECISIONES.md#d-10), regla 4) en vez del del archivo; eso depende de [P-06](#p-06).
