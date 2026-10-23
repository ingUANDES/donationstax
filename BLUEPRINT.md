# Blueprint

Pregunta de investigación: evaluar el impacto de los cambios regulatorios en las donaciones con fines sociales en Chile, con el alcance de [D-02 · Alcance](DECISIONES.md#d-02). Este archivo define el método y el backlog; el estado está en el [README](README.md#estado) y las decisiones en [DECISIONES.md](DECISIONES.md).

Serie **P-nn**: pendientes. Este es el único archivo donde se define el prefijo P- y donde viven sus ítems. Se toman en el orden que fije una persona ([D-06 · Canal de trabajo](DECISIONES.md#d-06)). Citar desde otro archivo: `[P-01 · Inventario normativo](BLUEPRINT.md#p-01)`.

## Etapas

| Etapa | Producto | Cómo se verifica | Backlog |
|---|---|---|---|
| E1 Inventario normativo | [normativa/cambios.csv](normativa/cambios.csv) y textos en `normativa/textos/` | [verifica_citas.py](analisis/verifica_citas.py) | [P-01](#p-01) |
| E2 Estructura de los datos | [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv) | esquema calculado en Python y en R, comparados ([contraste_datos.py](analisis/contraste_datos.py)) | [P-02](#p-02), [P-06](#p-06) |
| E3 Precio fiscal por régimen | salidas de [precio_fiscal.py](analisis/precio_fiscal.py) y [precio_fiscal.R](analisis/precio_fiscal.R) | [contraste.py](analisis/contraste.py) | [P-04](#p-04) |
| E4 Diseños de estimación por cambio | scripts en `analisis/`, Python y R | simulación con efecto conocido y contraste | [P-05](#p-05) |
| E5 Resultados y manuscrito | tablas y texto del artículo | solo cifras con nivel «verificada» ([D-05](DECISIONES.md#d-05)) | sin abrir |

## Verificación

El único punto de entrada es `make verificar` ([Makefile](Makefile)). Hoy comprueba que Python y R calculan el mismo precio fiscal sobre casos ficticios con valor esperado calculado a mano, que ambos contrastes fallan cuando se altera una salida, y que cada fila normativa marcada `verificada` cita el texto de la norma literalmente. Una fila ficticia no es evidencia sobre la norma: los casos de [analisis/fixtures/](analisis/fixtures/precio_fiscal_prueba.csv) solo prueban la implementación.

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

Inferencia del investigador a partir de [cambios.csv](normativa/cambios.csv) y [esquema_por_anio.csv](datos/esquema_por_anio.csv); la aprueba una persona, y cambia si cambian los datos.

| Cambio | Fecha del tratamiento | ¿Evaluable con las planillas? |
|---|---|---|
| Ley 20.316 | donaciones desde 2009 | No: 2008 y 2009 tienen 5 y 12 filas |
| Ley 20.565, de salto a quiebre | 2012-02-08 | El bunching posterior en el quiebre sí; el efecto del cambio no, porque el período previo (2011) tiene 656 filas |
| Cambios de tasa del impuesto a la renta | por fecha (pendiente en [P-01](#p-01)) | Pendiente de P-01 |
| Ley 21.210, artículo 10 | 2020-03-01 | En principio sí; el RUT del donante falta en 2019 |
| Ley 21.440 | 2022-07-01 | Solo la salida del régimen de la Ley 19.885: las planillas no traen el régimen nuevo (faltan datos) |
| Ley 21.015 | por verificar | No: sus donaciones usan otro certificado y no dan crédito |

Con esto, la ventana empírica realista son 2012 a 2025 ([D-08 · Ventana empírica](DECISIONES.md#d-08)).

## Pendientes

### P-01

**Inventario normativo (E1).** Estado: hecho el 2026-10-22, salvo lo de «Falta dentro de P-01».

Producto: [normativa/cambios.csv](normativa/cambios.csv), una fila por cambio de parámetro con su cita literal, que [verifica_citas.py](analisis/verifica_citas.py) comprueba contra los textos de [normativa/textos/](normativa/textos/). Los textos de leyes salen de BCN con [bcn_versiones.py](analisis/bcn_versiones.py) y los de circulares, del SII. La descarga y la comprobación de citas son solo en Python: el contraste Python y R de [D-03 · Python y R en paralelo](DECISIONES.md#d-03) se aplica a resultados numéricos. La columna `efecto_precio` es el signo a priori frente al régimen inmediatamente anterior para un donante que no cambia su conducta; el cálculo está en [P-04](#p-04).

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

Falta dentro de P-01:

- Tasas transitorias de 2011 y 2012. El texto del artículo 20 de la Ley sobre Impuesto a la Renta muestra una sola tasa hasta 2012-12-31, y las de esos años pueden estar en disposiciones transitorias de otras leyes: no se leyeron (sin verificar).
- El texto de la Ley 20.565 en sí (se conoce por la versión consolidada de BCN y la Circular N° 49; no se encontró su `idNorma`), el reglamento (Decreto Supremo 77 de 2009) y las circulares del SII sobre la Ley 21.440.
- Mecánica del crédito bajo los regímenes de 2017 en adelante (semi integrado y atribuido) y la fecha de publicación de los reglamentos de la Ley 21.015.
- Los regímenes que el [artículo 10 de la Ley 19.885](normativa/textos/ley19885_v2024-08-24.txt) enumera y que [D-02](DECISIONES.md#d-02) deja fuera por ahora.

### P-02

**Estructura de los datos (E2).** Estado: perfil de esquema hecho el 2026-10-22, a nivel *contrastado* ([D-05 · Niveles de resultado](DECISIONES.md#d-05)): Python y R coinciden en los 18 años. Queda armonizar las planillas ([P-06](#p-06)).

Producto: [datos/esquema_por_anio.csv](datos/esquema_por_anio.csv) (una fila por año, 2008 a 2025, con columnas, conteos por tipo de valor, rangos de fecha, sumas y duplicados), [datos/descargas_midesof.csv](datos/descargas_midesof.csv) (URL, tamaño, sha256 y `Last-Modified` de cada archivo, para detectar cambios) y la [instantánea de la página del Ministerio](datos/fuentes/midesof_pagina_2026-10-22.txt). Scripts: [descargar_midesof.py](analisis/descargar_midesof.py), y [esquema_datos.py](analisis/esquema_datos.py) y [esquema_datos.R](analisis/esquema_datos.R) con [contraste_datos.py](analisis/contraste_datos.py). `make verificar-datos` los corre (tarda varios minutos y necesita las planillas en `data_local/`). Las reglas de lectura comunes a los dos lenguajes están en la cabecera de esquema_datos.py; salieron del contraste: los espacios finales cuentan, las celdas solo con espacios son vacías, y las secuencias corruptas de los archivos de 2021 a 2025 pasan a U+FFFD porque `readxl` no puede abrir el de 2022 con ellas.

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

### P-03

**Corregir el origen de las fechas de Excel en `tidy_data.R`.** Estado: pendiente.

[tidy_data.R L25](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/donationstaxrpkg/dev/tidy_data.R#L25) convierte fechas seriales con `origin = "1900-12-30"`. Comprobación ad hoc sin script (no verificada), sobre `d_2010_2014.rda` en `ed3eb2e`: 101.660 de 134.613 filas tienen `fecha` serial de 5 dígitos; en todas, `fecha2` coincide con el origen 1900-12-30, y su año coincide con `ano` en 3 de 101.660; con origen 1899-12-30 coincide en 101.654 de 101.660. Ejemplo: `fecha` 40280, `ano` 2010, `fecha2` 2011-04-12. Con ventanas por fecha de ley, queda corrido un año el 75,5 % de las filas.

Hacer: cambiar el origen, regenerar `d_2010_2014`, escribir en `analisis/` un script que falle con `AssertionError` si `year(fecha2) != ano` fuera de una tolerancia fijada de antemano, e inspeccionar a mano las 6 filas seriales que no coinciden y las 4 con `fecha2` NA. Sin verificar si el [artículo](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L75) usa `fecha2`: carga archivos de `data_local/` que no están en el repo.

Nota del 2026-10-22: las planillas actuales de 2010 a 2013 traen el mismo número de filas que este extracto y la fecha ya como fecha de Excel ([P-02](#p-02)), de modo que [P-06](#p-06) puede reemplazar el extracto. Este pendiente solo vale si se conserva el paquete tal como está.

### P-04

**Precio fiscal por régimen (E3).** Estado: pendiente; depende de [P-01](#p-01).

Calcular P = 1 − c − τg para cada fila de [normativa/cambios.csv](normativa/cambios.csv) con las funciones de [precio_fiscal.py](analisis/precio_fiscal.py) y [precio_fiscal.R](analisis/precio_fiscal.R), con τ por año y por tipo de contribuyente en vez de una tasa fija. Determinar, con cita, si el crédito y el tope global del artículo 10 hacen que el precio marginal sea otro para quien llega al tope (inferencia; sin verificar contra la norma). Salida: signo y magnitud del cambio de precio de cada cambio regulatorio, comparados Python contra R.

### P-05

**Simulaciones de efecto conocido (E4).** Estado: pendiente.

Antes de usar datos reales, un caso simulado por diseño, en Python y en R, con semilla fija y tolerancias fijadas antes de correr ([D-05](DECISIONES.md#d-05)):

- Bunching en un quiebre del conjunto presupuestario: elasticidad conocida, polinomio de orden 3 a 6, ventana de exclusión, error estándar por remuestreo; que recupere la masa en exceso y la elasticidad; sensibilidad al orden y a la ventana; incluir donantes que cumplen la condición del Fondo Mixto.
- Estudio de eventos y diferencias en diferencias: efecto y tendencias paralelas conocidos; un caso con tratamiento definido por el nivel previo del resultado que muestre el sesgo.
- Variables instrumentales: un caso con efectos fijos de año que muestre que la alícuota por año no aporta variación.

Cada riesgo de [Riesgos de identificación a testear](#riesgos-de-identificación-a-testear) debe quedar cubierto por al menos un caso.

### P-06

**Armonizar las planillas (E2).** Estado: pendiente; sigue a [P-02](#p-02).

Hacer: un diccionario de columnas para las cinco variantes de esquema, tipos y fechas únicos (serial, texto e ISO) y una tabla de reglas para lo que el perfil marca: las 7.163 filas de 2014 con monto no numérico, los duplicados exactos, las fechas fuera de rango, las filas con `AÑO` distinto del año de la fecha y los caracteres corruptos. La salida va a `data_local/` (los nombres de personas naturales no se suben); lo versionado son las reglas y los conteos, calculados en Python y en R y contrastados. Después, la identificación de donantes: por RUT donde existe y por nombre normalizado donde no, midiendo el error de unión (el encargo propone Módulo 11, Jaro-Winkler y agrupamiento; sin verificar que hagan falta).
