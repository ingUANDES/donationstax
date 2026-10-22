# Blueprint

Pregunta de investigación: evaluar el impacto de los cambios regulatorios en las donaciones con fines sociales en Chile, con el alcance de [D-02 · Alcance](DECISIONES.md#d-02). Este archivo define el método y el backlog; el estado está en el [README](README.md#estado) y las decisiones en [DECISIONES.md](DECISIONES.md).

Serie **P-nn**: pendientes. Este es el único archivo donde se define el prefijo P- y donde viven sus ítems. Se toman en el orden que fije una persona ([D-06 · Canal de trabajo](DECISIONES.md#d-06)). Citar desde otro archivo: `[P-01 · Inventario normativo](BLUEPRINT.md#p-01)`.

## Etapas

| Etapa | Producto | Cómo se verifica | Backlog |
|---|---|---|---|
| E1 Inventario normativo | [normativa/cambios.csv](normativa/cambios.csv) y textos en `normativa/textos/` | [verifica_citas.py](analisis/verifica_citas.py) | [P-01](#p-01) |
| E2 Estructura de los datos | `datos/esquema_por_anio.csv` | esquema calculado en Python y en R, comparados | [P-02](#p-02) |
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

## Pendientes

### P-01

**Inventario normativo (E1).** Estado: hecho en lo central el 2026-10-22; queda lo de «Falta dentro de P-01».

Producto: [normativa/cambios.csv](normativa/cambios.csv), una fila por cambio de parámetro con su cita literal, que [verifica_citas.py](analisis/verifica_citas.py) comprueba contra los textos de [normativa/textos/](normativa/textos/). Los textos de leyes salen de BCN con [bcn_versiones.py](analisis/bcn_versiones.py) y los de circulares, del SII. La descarga y la comprobación de citas son solo en Python: el contraste Python y R de [D-03 · Python y R en paralelo](DECISIONES.md#d-03) se aplica a resultados numéricos. La columna `efecto_precio` es el signo a priori frente al régimen inmediatamente anterior para un donante que no cambia su conducta; el cálculo está en [P-04](#p-04).

Lo que la norma dice y cambia el diseño de [P-05](#p-05) (cada punto tiene su fila en cambios.csv; lo que va más allá de la fila es inferencia del investigador):

- **El tratamiento rige por la fecha de la donación, no por la de la ley.** El artículo 2 transitorio de la Ley 20.316 la hace regir para donaciones desde 2009, aunque BCN muestra el texto nuevo desde 2010; dos de sus numerales rigen desde 2010 y se ven en BCN desde 2011. El evento de la Ley 20.316 es 2009.
- **La forma de la regla en 1.000 UTM cambió.** Entre la versión de 2010 y la de 2012 la tasa menor se aplicaba a la donación completa (un salto); desde 2012-02-08, solo a la parte que excede (un quiebre). Son dos diseños distintos de bunching; las planillas del Ministerio solo cubren con volumen el segundo ([P-02](#p-02)).
- **La unidad del umbral cambió.** Hasta 2012-02-08 se sumaban las donaciones a cualquier institución; desde entonces, las hechas a una misma institución en el año (N° 9). La Circular N° 71 del SII dice que la relación con el Fondo se aprecia por institución. Inferencia: la variable de ejecución debe definirse por donante, institución y año, y contrastarse con el total por donante.
- **Hubo una regla de dos instituciones y 700 UTM** entre 2010-01-01 y 2012-02-07, y los 25 % y 40 % para adicciones existen desde 2010; la Ley 20.565 los volvió marginales.
- **Lo que no cambió en 2016:** la Ley 20.900 solo derogó el título de donaciones políticas.
- **Dos regímenes paralelos desde 2018 y 2022.** Las donaciones de la medida alternativa de la Ley 21.015 no dan crédito, solo gasto necesario, y su vigencia sigue sin verificar. La Ley 21.440 permite deducir de la base, con topes en UTM y en porcentaje, fuera del límite global del artículo 10, y desde 2022-07-01.

Contradicciones del encargo y del artículo del repo, resueltas con la norma:

- Sentido de la Ley 20.316: [D-07 · Sentido del cambio de la Ley 20.316](DECISIONES.md#d-07) (el artículo del repo acierta; el encargo no).
- Vigencias: el encargo acierta en las fechas de las Leyes 19.885, 20.316 (entrada en vigencia), 20.565 y 21.440. La tabla del [artículo, L369-L375](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L369-L375) está mal en la Ley 19.885 (aparece «2006») y en la «Ley 20.900(1)»: la modificación del 2012-02-08 es la Ley 20.565 (el D.O. de esa fecha lo dice la [Circular N° 49](normativa/textos/circular_sii_49_2012.txt)).
- La regla de dos instituciones y 700 UTM sí existió (el artículo acierta; el encargo la omitía) y los 25 % y 40 % no son de la Ley 20.565 (el encargo se equivoca).
- El encargo dice que las donaciones de la Ley 21.015 «aplican el esquema vigente de la Ley 19.885»: no dan crédito.
- Fuentes del encargo: no se revisaron una por una. La fuente 1 es un documento cuyo nombre de archivo dice 2006 y se cita para leyes de 2009 a 2022 (sigue sin leer), varias referencias repiten la misma URL y hay afirmaciones con las nueve fuentes en bloque.

Falta dentro de P-01:

- Tasa del impuesto de primera categoría y escala del global complementario por fecha (artículos 20 y 52 de la Ley sobre Impuesto a la Renta): ver abajo.
- El texto de la Ley 20.565 en sí (se conoce por la versión consolidada de BCN y la Circular N° 49; no se encontró su `idNorma`), el reglamento (Decreto Supremo 77 de 2009) y las circulares del SII sobre la Ley 21.440.
- Mecánica del crédito bajo los regímenes de 2017 en adelante (semi integrado y atribuido) y la fecha de publicación de los reglamentos de la Ley 21.015.
- Los regímenes que el [artículo 10 de la Ley 19.885](normativa/textos/ley19885_v2024-08-24.txt) enumera y que [D-02](DECISIONES.md#d-02) deja fuera por ahora.

### P-02

**Estructura de los datos (E2).** Estado: pendiente; depende del acceso a `midesof.gob.cl`.

Evidencia previa, comprobación ad hoc sin script (no verificada), sobre el commit [ed3eb2e](https://github.com/ingUANDES/donationstax/tree/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c):

- El extracto empaquetado `donationstaxrpkg/data/d_2010_2014.rda` tiene 134.613 filas y las columnas `ano, fecha, nombre_del_proyecto, donatario, donante, monto_fondo_mixto, monto_institucion, monto_total_donacion, fecha2`: sin RUT de donante ni de donatario, sin código de proyecto y sin región.
- Filas por `ano`: 2010: 42; 2011: 656; 2012: 35.586; 2013: 98.328; 2014: 1. Es decir, 99,5 % en 2012-2013; qué significa `ano` no está documentado.
- 14.420 nombres de donante distintos, sin normalizar.
- El esquema de 2020 que nombra [code/ETL.R, L69-L73](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/code/ETL.R#L69-L73) incluye `donorRUT`, `institutionRUT` y `region`, y no incluye columna de certificado.

Hacer: listar todos los archivos que publica hoy [la página del Ministerio](https://www.midesof.gob.cl/sociedad-civil/donaciones-sociales/donaciones-recibidas-ley-n19-885/), descargarlos a `data_local/`, y registrar por año columnas, tipos, número de filas y rango de fechas en `datos/esquema_por_anio.csv`, calculado de forma independiente en Python y en R. Responder con evidencia: qué años existen (el encargo dice desde 2008; sin verificar), si hay tipo de certificado que separe Certificado N° 25 y N° 60, si el RUT del donante existe por año, qué cambió de columnas entre años, y si hay duplicados o rectificaciones. Los nombres de personas naturales no se suben al repo.

### P-03

**Corregir el origen de las fechas de Excel en `tidy_data.R`.** Estado: pendiente.

[tidy_data.R L25](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/donationstaxrpkg/dev/tidy_data.R#L25) convierte fechas seriales con `origin = "1900-12-30"`. Comprobación ad hoc sin script (no verificada), sobre `d_2010_2014.rda` en `ed3eb2e`: 101.660 de 134.613 filas tienen `fecha` serial de 5 dígitos; en todas, `fecha2` coincide con el origen 1900-12-30, y su año coincide con `ano` en 3 de 101.660; con origen 1899-12-30 coincide en 101.654 de 101.660. Ejemplo: `fecha` 40280, `ano` 2010, `fecha2` 2011-04-12. Con ventanas por fecha de ley, queda corrido un año el 75,5 % de las filas.

Hacer: cambiar el origen, regenerar `d_2010_2014`, escribir en `analisis/` un script que falle con `AssertionError` si `year(fecha2) != ano` fuera de una tolerancia fijada de antemano, e inspeccionar a mano las 6 filas seriales que no coinciden y las 4 con `fecha2` NA. Sin verificar si el [artículo](https://github.com/ingUANDES/donationstax/blob/ed3eb2ec26b14897e6742e8e52aaf83d9b82d50c/article-elsevier/article-elsevier.Rmd#L75) usa `fecha2`: carga archivos de `data_local/` que no están en el repo.

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
