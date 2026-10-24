PY ?= python3

.PHONY: verificar
# Contrasta Python y R sobre un caso con valor esperado a mano y comprueba las
# citas normativas. Falla (código distinto de 0) si algo no se cumple.
verificar:
	mkdir -p salidas
	$(PY) -I analisis/precio_fiscal.py analisis/fixtures/precio_fiscal_prueba.csv salidas/precio_fiscal_py.csv
	Rscript analisis/precio_fiscal.R analisis/fixtures/precio_fiscal_prueba.csv salidas/precio_fiscal_R.csv
	$(PY) -I analisis/contraste.py salidas/precio_fiscal_py.csv salidas/precio_fiscal_R.csv analisis/fixtures/precio_fiscal_esperado.csv
	$(PY) -I analisis/verifica_citas.py

.PHONY: verificar-datos verificar-datos-19885 verificar-datos-21440 verificar-cruce verificar-armonizado
# Corre los cuatro bloques de abajo. Tarda varios minutos y necesita los archivos ya descargados en
# data_local/ (ignorado por git), python-calamine (analisis/requirements.txt) y el paquete readxl de R.
verificar-datos: verificar-datos-19885 verificar-datos-21440 verificar-cruce verificar-armonizado

# Planillas de la Ley 19.885 en data_local/midesof/ (python3 analisis/descargar_midesof.py data_local/midesof
# datos/descargas_midesof.csv). Perfila el esquema de los 18 años en Python y en R, exige que coincidan y que el
# perfil versionado en datos/esquema_por_anio.csv sea el que sale hoy de los archivos.
verificar-datos-19885:
	mkdir -p salidas
	$(PY) -I analisis/esquema_datos.py data_local/midesof salidas/esquema_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/esquema_datos.R data_local/midesof salidas/esquema_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/esquema_py.csv salidas/esquema_R.csv
	cmp salidas/esquema_py.csv datos/esquema_por_anio.csv

# Nómina de la Ley 21.440 en data_local/ley21440/ (python3 analisis/descargar_ley21440.py data_local/ley21440
# datos/descargas_ley21440.csv; la URL del archivo vence en minutos y el script la sigue de inmediato). Lo mismo
# que el bloque anterior, contra datos/esquema_ley21440_por_anio.csv.
verificar-datos-21440:
	mkdir -p salidas
	$(PY) -I analisis/esquema_ley21440.py data_local/ley21440 salidas/esquema_ley21440_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/esquema_ley21440.R data_local/ley21440 salidas/esquema_ley21440_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/esquema_ley21440_py.csv salidas/esquema_ley21440_R.csv
	cmp salidas/esquema_ley21440_py.csv datos/esquema_ley21440_por_anio.csv

# Cruce por RUT de las donatarias de la Ley 21.440 con las instituciones de la Ley 19.885 en los años que
# están en las dos carpetas, contra datos/cruce_donatarias_por_anio.csv.
verificar-cruce:
	mkdir -p salidas
	$(PY) -I analisis/cruce_donatarias.py data_local/midesof data_local/ley21440 salidas/cruce_donatarias_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/cruce_donatarias.R data_local/midesof data_local/ley21440 salidas/cruce_donatarias_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/cruce_donatarias_py.csv salidas/cruce_donatarias_R.csv
	cmp salidas/cruce_donatarias_py.csv datos/cruce_donatarias_por_anio.csv

# Armonización de las planillas de la Ley 19.885 según D-10 (P-06). Escribe una fila por fila de cada
# planilla en data_local/armonizado/py/ y data_local/armonizado/r/ (trae nombres: no se versiona), exige
# que Python y R coincidan en las tablas y celda por celda, y que las tablas sean las versionadas en datos/.
verificar-armonizado:
	mkdir -p salidas
	$(PY) -I analisis/armonizar.py data_local/midesof data_local/armonizado/py salidas/armonizacion_py.csv salidas/armonizacion_anios_py.csv salidas/diccionario_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/armonizar.R data_local/midesof data_local/armonizado/r salidas/armonizacion_R.csv salidas/armonizacion_anios_R.csv salidas/diccionario_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/armonizacion_py.csv salidas/armonizacion_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/armonizacion_anios_py.csv salidas/armonizacion_anios_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/diccionario_py.csv salidas/diccionario_R.csv
	$(PY) -I analisis/contraste_armonizado.py data_local/armonizado/py data_local/armonizado/r
	cmp salidas/armonizacion_py.csv datos/armonizacion_por_anio.csv
	cmp salidas/armonizacion_anios_py.csv datos/armonizacion_anios_donacion.csv
	cmp salidas/diccionario_py.csv datos/diccionario_columnas.csv
