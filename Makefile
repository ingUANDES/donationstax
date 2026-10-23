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

.PHONY: verificar-datos
# Necesita las planillas en data_local/midesof/ (python3 analisis/descargar_midesof.py data_local/midesof
# datos/descargas_midesof.csv), python-calamine (analisis/requirements.txt) y el paquete readxl de R.
# Tarda varios minutos. Perfila el esquema de los 18 años en Python y en R, exige que coincidan y que el
# perfil versionado en datos/esquema_por_anio.csv sea el que sale hoy de los archivos.
verificar-datos:
	mkdir -p salidas
	$(PY) -I analisis/esquema_datos.py data_local/midesof salidas/esquema_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/esquema_datos.R data_local/midesof salidas/esquema_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/esquema_py.csv salidas/esquema_R.csv
	cmp salidas/esquema_py.csv datos/esquema_por_anio.csv
