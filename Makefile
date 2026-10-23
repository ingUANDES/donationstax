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
