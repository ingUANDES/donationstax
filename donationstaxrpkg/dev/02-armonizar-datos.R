library(readxl)
library(dplyr)
library(stringr)

rutero <- readRDS("~/github/donationstax/donationstaxrpkg/dev/rutero.rds")

# 2015 ----

d2015 <- read_excel("~/github/donationstax/data_local/d2015.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "text", "text", "text", "numeric",
                                  "numeric", "numeric")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  rename(rut_donante = rut) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut_donante = rut10_rutnum(rut_rut10(rut_donante)),
    dv_donante = rutnum_dv(rut_donante)
  ) %>%
  select(ano:rut_donante, dv_donante, everything())

d2015 <- d2015 %>%
  left_join(
    rutero %>%
      rename(donante = razon_social, rut_donante = rut,
             dv_donante = dv, nombre_sii_donante = nombre_sii),
    by = c("rut_donante", "dv_donante")
  ) %>%
  select(ano:donante.x, donante.y, nombre_sii_donante, everything())

# 2016 ----

d2016 <- read_excel("~/github/donationstax/data_local/d2016.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "text", "text", "text", "numeric",
                                  "numeric", "numeric")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  rename(rut_donante = rut) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut_donante = rut10_rutnum(rut_rut10(rut_donante)),
    dv_donante = rutnum_dv(rut_donante)
  ) %>%
  select(ano:rut_donante, dv_donante, everything())

d2016 <- d2016 %>%
  left_join(
    rutero %>%
      rename(donante = razon_social, rut_donante = rut,
             dv_donante = dv, nombre_sii_donante = nombre_sii),
    by = c("rut_donante", "dv_donante")
  ) %>%
  select(ano:donante.x, donante.y, nombre_sii_donante, everything())

# 2017 ----

d2017 <- read_excel("~/github/donationstax/data_local/d2017.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "numeric", "text", "text", "text",
                                  "text", "numeric", "numeric", "numeric",
                                  "numeric")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  rename(rut_donante = rut) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut_donante = rut10_rutnum(rut_rut10(rut_donante)),
    dv_donante = rutnum_dv(rut_donante)
  ) %>%
  select(ano:rut_donante, dv_donante, everything())

d2017 <- d2017 %>%
  left_join(
    rutero %>%
      rename(donante = razon_social, rut_donante = rut,
             dv_donante = dv, nombre_sii_donante = nombre_sii),
    by = c("rut_donante", "dv_donante")
  ) %>%
  select(ano:donante.x, donante.y, nombre_sii_donante, everything())

# 2018 ----

d2018 <- read_excel("~/github/donationstax/data_local/d2018.xlsx",
                    sheet = "Hoja1",
                    col_types = c("numeric",
                                  "text", "text", "text", "text", "text",
                                  "numeric", "numeric")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " "))

d2018 <- d2018 %>%
  left_join(
    rutero %>%
      rename(donante = razon_social, rut_donante = rut,
             dv_donante = dv, nombre_sii_donante = nombre_sii),
    by = "donante"
  )

d2018 <- d2018 %>%
  select(ano:donante, rut_donante, dv_donante, nombre_sii_donante, everything())

# 2019 ----

d2019 <- read_excel("~/github/donationstax/data_local/d2019.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "text", "text", "text", "numeric",
                                  "numeric")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " "))

d2019 <- d2019 %>%
  left_join(
    rutero %>%
      rename(donante = razon_social, rut_donante = rut,
             dv_donante = dv, nombre_sii_donante = nombre_sii),
    by = "donante"
  )

d2019 <- d2019 %>%
  select(ano:donante, rut_donante, dv_donante, nombre_sii_donante, everything())

# 2020 ----

d2020 <- read_excel("~/github/donationstax/data_local/d2020.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "text", "text", "text", "text", "text",
                                  "text", "text", "text", "text", "text")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  rename(rut_donante = rut) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut_donante = rut10_rutnum(rut_rut10(rut_donante)),
    dv_donante = rutnum_dv(rut_donante)
  ) %>%
  select(ano:rut_donante, dv_donante, everything())

d2020 <- d2020 %>%
  left_join(
    rutero %>%
      rename(donante = razon_social, rut_donante = rut,
             dv_donante = dv, nombre_sii_donante = nombre_sii),
    by = c("rut_donante", "dv_donante")
  ) %>%
  select(ano:donante.x, donante.y, nombre_sii_donante, everything())

# todo ----

d <- d2015 %>%
  bind_rows(d2016) %>%
  bind_rows(
    d2017 %>%
      select(ano, fecha, nombre_del_proyecto, donatario,
             donante.x, donante.y, nombre_sii_donante,
             rut_donante, dv_donante, monto_fondo_mixto,
             monto_institucion, monto_total_donacion)
  ) %>%
  bind_rows(
    d2018 %>%
      select(ano, fecha, nombre_del_proyecto, donatario,
             donante.x = donante, nombre_sii_donante,
             rut_donante, dv_donante, monto_fondo_mixto,
             monto_institucion, monto_total_donacion) %>%
      mutate(
        monto_fondo_mixto = as.numeric(monto_fondo_mixto),
        monto_institucion = as.numeric(monto_institucion),
        monto_total_donacion = as.numeric(monto_total_donacion)
      )
  ) %>%
  bind_rows(
    d2019 %>%
      select(ano, fecha, nombre_del_proyecto, donatario,
             donante.x = donante, nombre_sii_donante,
             rut_donante, dv_donante, monto_fondo_mixto,
             monto_institucion, monto_total_donacion) %>%
      mutate(
        monto_fondo_mixto = as.numeric(monto_fondo_mixto),
        monto_institucion = as.numeric(monto_institucion),
        monto_total_donacion = as.numeric(monto_total_donacion)
      )
  ) %>%
  bind_rows(
    d2020 %>%
      select(ano, fecha, nombre_del_proyecto, donatario,
             donante.x, donante.y, nombre_sii_donante,
             rut_donante, dv_donante, monto_fondo_mixto,
             monto_institucion, monto_total_donacion) %>%
      mutate(
        monto_fondo_mixto = as.numeric(monto_fondo_mixto),
        monto_institucion = as.numeric(monto_institucion),
        monto_total_donacion = as.numeric(monto_total_donacion)
      )
  )

d <- d %>%
  filter(!is.na(monto_total_donacion))

d <- d %>%
  mutate(
    fecha = openxlsx::convertToDate(fecha),
    ano = ifelse(is.na(ano), year(fecha), ano)
  )

d %>%
  mutate(
    donante.x = ifelse(donante.x == "NULO", NA, donante.x),
    marca = ifelse(donante.x == donante.y, 1L, 0L)
  ) %>%
  filter(is.na(marca) | marca == 0L) %>%
  distinct(donante.x)

d %>%
  mutate(
    donante.x = ifelse(donante.x == "NULO", NA, donante.x),
    marca = ifelse(donante.x == donante.y, 1L, 0L)
  ) %>%
  group_by(ano, marca) %>%
  count() %>%
  View()
