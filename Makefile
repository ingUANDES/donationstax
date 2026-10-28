PY ?= python3

.PHONY: verificar
# Contrasta Python y R sobre un caso con valor esperado a mano, comprueba las citas normativas y calcula el
# precio fiscal por régimen y año (P-04): Python contra R contra los valores calculados a mano, y la salida
# versionada en datos/ contra la que sale hoy. Clasifica, sin red, los límites de versión de las series de
# textos de BCN (analisis/limites_bcn.py, https://github.com/ingUANDES/donationstax/issues/37): falla si uno de la
# serie solo marca un corte del XML (DECISIONES.md#d-21, punto 4) o si una versión de normativa/textos/retiradas/
# no es de esa clase; antes prueba la bisección de bcn_versiones.py con XML y JSON del visor ficticios.
# Corre además las simulaciones de efecto conocido
# (BLUEPRINT.md#p-05): Python contra R y contra la copia versionada en datos/, con la tolerancia relativa de
# DECISIONES.md#d-15; las reglas incumplidas deben ser las registradas en $(SIM_H). Corre también
# duplicados_ley21440.py y .R sobre una tabla ficticia (analisis/fixtures/duplicados_prueba.csv) y exige que
# coincidan entre sí y con los valores calculados a mano; esa parte no necesita python-calamine ni readxl.
# Lee la UTM mensual de las páginas del SII versionadas en $(UTM_HTML) (https://github.com/ingUANDES/donationstax/issues/33;
# las baja analisis/descargar_utm.py, con red): Python y R deben dar la misma serie, byte a byte, y la misma
# tabla por año, y las dos deben ser las versionadas en datos/.
# Corre también la lista corta (10 semillas) de los Monte Carlo de bunching y de estudio de eventos
# (DECISIONES.md#d-19, d-24 y d-25; https://github.com/ingUANDES/donationstax/issues/34): Python contra R y contra
# la copia versionada, con la tolerancia de DECISIONES.md#d-15; las reglas incumplidas deben ser las registradas
# en $(SIM_H). La lista larga (200 semillas) va en verificar-mc.
# Falla (código distinto de 0) si algo no se cumple.
PRECIO_IN = normativa/cambios.csv normativa/precio_casos.csv normativa/precio_comparaciones.csv
SIM_H = analisis/fixtures/sim_hallazgos.csv
DUP_PRUEBA = analisis/fixtures/duplicados_prueba.csv
UTM_HTML = datos/fuentes/sii_utm
UTM_IGUALES = anio,utm_diciembre,utm_min,mes_utm_min,utm_max,mes_utm_max
SIM_MC_B = datos/precio_fiscal_por_caso.csv datos/utm_mensual.csv $(SIM_H)
SIM_MC_E = datos/rut_completado_por_anio.csv $(SIM_H)
# $(call sim_mc,<lista>): corre los dos Monte Carlo con la lista corta o larga y contrasta Python, R y datos/.
define sim_mc
	$(PY) -I analisis/sim_bunching_mc.py $(SIM_MC_B) $(1) salidas/sim_bunching_mc_$(1)_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/sim_bunching_mc.R $(SIM_MC_B) $(1) salidas/sim_bunching_mc_$(1)_R.csv
	$(PY) -I analisis/sim_eventos_mc.py $(SIM_MC_E) $(1) salidas/sim_eventos_mc_$(1)_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/sim_eventos_mc.R $(SIM_MC_E) $(1) salidas/sim_eventos_mc_$(1)_R.csv
	for s in bunching_mc_$(1) eventos_mc_$(1); do for c in valor objetivo; do \
	  $(PY) -I analisis/contraste.py salidas/sim_$${s}_py.csv salidas/sim_$${s}_R.csv --clave clave --columna $$c --tol-rel 1e-8 --iguales clave,regla,cumple || exit 1; \
	  $(PY) -I analisis/contraste.py salidas/sim_$${s}_py.csv datos/simulacion_$${s}.csv --clave clave --columna $$c --tol-rel 1e-8 --iguales clave,regla,cumple || exit 1; \
	done; done
endef
verificar:
	mkdir -p salidas
	$(PY) -I analisis/precio_fiscal.py analisis/fixtures/precio_fiscal_prueba.csv salidas/precio_fiscal_py.csv
	Rscript analisis/precio_fiscal.R analisis/fixtures/precio_fiscal_prueba.csv salidas/precio_fiscal_R.csv
	$(PY) -I analisis/contraste.py salidas/precio_fiscal_py.csv salidas/precio_fiscal_R.csv analisis/fixtures/precio_fiscal_esperado.csv
	$(PY) -I analisis/verifica_citas.py
	$(PY) -I analisis/limites_bcn.py
	$(PY) -I analisis/precio_por_regimen.py $(PRECIO_IN) salidas/precio_casos_py.csv salidas/precio_cambios_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/precio_por_regimen.R $(PRECIO_IN) salidas/precio_casos_R.csv salidas/precio_cambios_R.csv
	$(PY) -I analisis/contraste.py salidas/precio_casos_py.csv salidas/precio_casos_R.csv analisis/fixtures/precio_casos_esperado.csv --iguales caso,anio_comercial,contribuyente,regimen
	$(PY) -I analisis/contraste.py salidas/precio_cambios_py.csv salidas/precio_cambios_R.csv --clave comparacion --columna delta --iguales fila,norma,articulo_inciso,vigencia_desde,efecto_declarado,comparacion,caso_antes,caso_despues,efecto_calculado,coincide,n_casos,nota
	cmp salidas/precio_casos_py.csv datos/precio_fiscal_por_caso.csv
	cmp salidas/precio_cambios_py.csv datos/precio_fiscal_por_cambio.csv
	$(PY) -I analisis/sim_bunching.py datos/precio_fiscal_por_caso.csv $(SIM_H) salidas/sim_bunching_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/sim_bunching.R datos/precio_fiscal_por_caso.csv $(SIM_H) salidas/sim_bunching_R.csv
	$(PY) -I analisis/sim_eventos.py datos/rut_completado_por_anio.csv $(SIM_H) salidas/sim_eventos_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/sim_eventos.R datos/rut_completado_por_anio.csv $(SIM_H) salidas/sim_eventos_R.csv
	$(PY) -I analisis/sim_vi.py datos/precio_fiscal_por_caso.csv datos/rut_completado_por_anio.csv $(SIM_H) salidas/sim_vi_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/sim_vi.R datos/precio_fiscal_por_caso.csv datos/rut_completado_por_anio.csv $(SIM_H) salidas/sim_vi_R.csv
	for s in bunching eventos vi; do for c in valor objetivo; do \
	  $(PY) -I analisis/contraste.py salidas/sim_$${s}_py.csv salidas/sim_$${s}_R.csv --clave clave --columna $$c --tol-rel 1e-8 --iguales clave,regla,cumple || exit 1; \
	  $(PY) -I analisis/contraste.py salidas/sim_$${s}_py.csv datos/simulacion_$${s}.csv --clave clave --columna $$c --tol-rel 1e-8 --iguales clave,regla,cumple || exit 1; \
	done; done
	$(PY) -I analisis/duplicados_ley21440.py --csv $(DUP_PRUEBA) salidas/dup_prueba_anio_py.csv salidas/dup_prueba_mult_py.csv salidas/dup_prueba_conc_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/duplicados_ley21440.R --csv $(DUP_PRUEBA) salidas/dup_prueba_anio_R.csv salidas/dup_prueba_mult_R.csv salidas/dup_prueba_conc_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/dup_prueba_anio_py.csv salidas/dup_prueba_anio_R.csv analisis/fixtures/duplicados_esperado_por_anio.csv
	$(PY) -I analisis/contraste_datos.py salidas/dup_prueba_mult_py.csv salidas/dup_prueba_mult_R.csv analisis/fixtures/duplicados_esperado_multiplicidad.csv
	$(PY) -I analisis/contraste_datos.py salidas/dup_prueba_conc_py.csv salidas/dup_prueba_conc_R.csv analisis/fixtures/duplicados_esperado_concentracion.csv
	$(PY) -I analisis/utm.py $(UTM_HTML) datos/descargas_utm.csv salidas/utm_mensual_py.csv salidas/utm_por_anio_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/utm.R $(UTM_HTML) datos/descargas_utm.csv salidas/utm_mensual_R.csv salidas/utm_por_anio_R.csv
	cmp salidas/utm_mensual_py.csv salidas/utm_mensual_R.csv
	cmp salidas/utm_mensual_py.csv datos/utm_mensual.csv
	for c in razon_dic_sobre_min razon_dic_sobre_max log_dic_sobre_min; do \
	  $(PY) -I analisis/contraste.py salidas/utm_por_anio_py.csv salidas/utm_por_anio_R.csv --clave anio --columna $$c --iguales $(UTM_IGUALES) || exit 1; \
	done
	cmp salidas/utm_por_anio_py.csv datos/utm_por_anio.csv
	$(call sim_mc,corta)

.PHONY: verificar-mc
# Lista larga (200 semillas) de los Monte Carlo de bunching y de estudio de eventos (DECISIONES.md#d-19): es la
# que decide el nivel de esas simulaciones. Tarda varios minutos y no necesita data_local/.
verificar-mc:
	mkdir -p salidas
	$(call sim_mc,larga)

.PHONY: verificar-datos verificar-datos-19885 verificar-datos-21440 verificar-duplicados-21440 verificar-cruce verificar-armonizado verificar-anio verificar-rut
# Corre los siete bloques de abajo, en orden. Tarda varios minutos y necesita los archivos ya descargados en
# data_local/ (ignorado por git), los paquetes de analisis/requirements.txt y los paquetes readxl y
# stringdist de R.
verificar-datos: verificar-datos-19885 verificar-datos-21440 verificar-duplicados-21440 verificar-cruce verificar-armonizado verificar-anio verificar-rut

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

# Duplicados exactos de la nómina de la Ley 21.440 (https://github.com/ingUANDES/donationstax/issues/16): copias extra, multiplicidades y
# concentración por año, en Python y en R, contra las tablas versionadas en datos/. El de Python exige además que filas, copias y
# suma de Valor sean las del perfil versionado. No excluye ninguna fila.
verificar-duplicados-21440:
	mkdir -p salidas
	$(PY) -I analisis/duplicados_ley21440.py data_local/ley21440 salidas/duplicados_anio_py.csv salidas/duplicados_mult_py.csv salidas/duplicados_conc_py.csv --perfil datos/esquema_ley21440_por_anio.csv
	LC_ALL=C.UTF-8 Rscript analisis/duplicados_ley21440.R data_local/ley21440 salidas/duplicados_anio_R.csv salidas/duplicados_mult_R.csv salidas/duplicados_conc_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/duplicados_anio_py.csv salidas/duplicados_anio_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/duplicados_mult_py.csv salidas/duplicados_mult_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/duplicados_conc_py.csv salidas/duplicados_conc_R.csv
	cmp salidas/duplicados_anio_py.csv datos/duplicados_ley21440_por_anio.csv
	cmp salidas/duplicados_mult_py.csv datos/duplicados_ley21440_multiplicidad.csv
	cmp salidas/duplicados_conc_py.csv datos/duplicados_ley21440_concentracion.csv

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
	$(PY) -I analisis/armonizar.py data_local/midesof data_local/armonizado/py salidas/armonizacion_py.csv salidas/diccionario_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/armonizar.R data_local/midesof data_local/armonizado/r salidas/armonizacion_R.csv salidas/diccionario_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/armonizacion_py.csv salidas/armonizacion_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/diccionario_py.csv salidas/diccionario_R.csv
	$(PY) -I analisis/contraste_armonizado.py data_local/armonizado/py data_local/armonizado/r
	cmp salidas/armonizacion_py.csv datos/armonizacion_por_anio.csv
	cmp salidas/diccionario_py.csv datos/diccionario_columnas.csv

# Año de la donación (D-13, punto 1, y D-14): de dónde puede salir el año de cada fila y los conflictos
# entre el año de la fecha y el del archivo, sobre las filas de verificar-armonizado; tablas contra datos/.
verificar-anio:
	mkdir -p salidas
	$(PY) -I analisis/evalua_anio.py data_local/armonizado/py salidas/anio_por_anio_py.csv salidas/anio_conflictos_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/evalua_anio.R data_local/armonizado/r salidas/anio_por_anio_R.csv salidas/anio_conflictos_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/anio_por_anio_py.csv salidas/anio_por_anio_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/anio_conflictos_py.csv salidas/anio_conflictos_R.csv
	cmp salidas/anio_por_anio_py.csv datos/anio_evaluacion_por_anio.csv
	cmp salidas/anio_conflictos_py.csv datos/anio_conflictos.csv

# Regla 7 de D-10 (P-06): completa el RUT del donante por nombre sobre las filas de verificar-armonizado,
# calibrando con los RUT conocidos. Filas en data_local/rut/py/ y data_local/rut/r/; tablas contra datos/.
verificar-rut:
	mkdir -p salidas
	$(PY) -I analisis/completar_rut.py data_local/armonizado/py data_local/rut/py salidas/rut_identico_py.csv salidas/rut_similitud_py.csv salidas/rut_por_anio_py.csv
	LC_ALL=C.UTF-8 Rscript analisis/completar_rut.R data_local/armonizado/r data_local/rut/r salidas/rut_identico_R.csv salidas/rut_similitud_R.csv salidas/rut_por_anio_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/rut_identico_py.csv salidas/rut_identico_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/rut_similitud_py.csv salidas/rut_similitud_R.csv
	$(PY) -I analisis/contraste_datos.py salidas/rut_por_anio_py.csv salidas/rut_por_anio_R.csv
	$(PY) -I analisis/contraste_armonizado.py data_local/rut/py data_local/rut/r
	cmp salidas/rut_identico_py.csv datos/rut_calibracion_identico.csv
	cmp salidas/rut_similitud_py.csv datos/rut_calibracion_similitud.csv
	cmp salidas/rut_por_anio_py.csv datos/rut_completado_por_anio.csv
