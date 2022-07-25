library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(lubridate)
library(stringr)
library(glue)
library(janitor)
library(readr)
library(stringr)
library(clrutr)

# 2015 ----

d15 <- read_excel("~/github/donationstax/data_local/d2015.xlsx",
           col_types = c("numeric", "text", "text",
                         "text", "text", "text", "numeric",
                         "numeric", "numeric")) %>%
  clean_names() %>%
  select(donante, rut_donante = rut) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut2_donante = rut10_rutnum(rut_rut10(rut_donante)),
    rut2_donante_dv = rutnum_dv(rut2_donante)
  ) %>%
  select(donante, rut_donante, rut2_donante, rut2_donante_dv) %>%
  distinct(rut_donante, .keep_all = T) %>%
  mutate(anio = 2015) %>%
  select(anio, everything()) %>%
  mutate(marca_dv = ifelse(str_to_upper(str_sub(rut_donante, -1, -1)) == rut2_donante_dv, 1, 0))

# 2016 ----

d16 <- read_excel("~/github/donationstax/data_local/d2016.xlsx",
                  col_types = c("numeric", "text", "text",
                                "text", "text", "text", "numeric",
                                "numeric", "numeric")) %>%
  clean_names() %>%
  select(donante, rut_donante = rut) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut2_donante = rut10_rutnum(rut_rut10(rut_donante)),
    rut2_donante_dv = rutnum_dv(rut2_donante)
  ) %>%
  select(donante, rut_donante, rut2_donante, rut2_donante_dv) %>%
  distinct(rut_donante, .keep_all = T) %>%
  mutate(anio = 2016) %>%
  select(anio, everything()) %>%
  mutate(marca_dv = ifelse(str_to_upper(str_sub(rut_donante, -1, -1)) == rut2_donante_dv, 1, 0))

# 2017 ----

d17 <- read_excel("~/github/donationstax/data_local/d2017.xlsx",
                  col_types = c("numeric", "text", "text",
                                "numeric", "text", "text", "text",
                                "text", "numeric", "numeric", "numeric",
                                "numeric")) %>%
  clean_names() %>%
  select(donante, rut_donante = rut) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut2_donante = rut10_rutnum(rut_rut10(rut_donante)),
    rut2_donante_dv = rutnum_dv(rut2_donante)
  ) %>%
  select(donante, rut_donante, rut2_donante, rut2_donante_dv) %>%
  distinct(rut_donante, .keep_all = T) %>%
  mutate(anio = 2017) %>%
  select(anio, everything()) %>%
  mutate(marca_dv = ifelse(str_to_upper(str_sub(rut_donante, -1, -1)) == rut2_donante_dv, 1, 0))

# 2020 ----

d20 <- read_excel("~/github/donationstax/data_local/d2020.xlsx",
                  col_types = c("numeric", "text", "text",
                                "text", "text", "text", "text", "text",
                                "text", "text", "text", "text", "text")) %>%
  clean_names() %>%
  select(donante, rut_donante = rut) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  mutate(rut_donante = str_remove_all(rut_donante, "\\.|-")) %>%
  mutate(
    rut2_donante = rut10_rutnum(rut_rut10(rut_donante)),
    rut2_donante_dv = rutnum_dv(rut2_donante)
  ) %>%
  select(donante, rut_donante, rut2_donante, rut2_donante_dv) %>%
  distinct(rut_donante, .keep_all = T) %>%
  mutate(anio = 2017) %>%
  select(anio, everything()) %>%
  mutate(marca_dv = ifelse(str_to_upper(str_sub(rut_donante, -1, -1)) == rut2_donante_dv, 1, 0))

# bind_rows(d15, d16, d17, d20) %>%
#   group_by(anio, marca_dv) %>%
#   count() %>%
#   write_csv("dev/cuenta_ruts_malos.csv")

rutero <- bind_rows(d15, d16, d17, d20) %>%
  distinct(rut2_donante, .keep_all = T) %>%
  drop_na(rut2_donante_dv)

rm(d15,d16,d17,d20)

# datos sii ----

# https://www.sii.cl/sobre_el_sii/nominapersonasjuridicas.html

url <- "https://www.sii.cl//estadisticas/nominas/PUB_EMPRESAS_PJ_2010a2014.zip"
zip <- "dev/PUB_EMPRESAS_PJ_2010a2014.zip"

if (!file.exists(zip)) {
  try(download.file(url, zip, method = "wget"))
}

if (!file.exists("dev/PUB_EMPRESAS_PJ_2010a2014.txt")) {
  unzip(zip, exdir = "dev")
}

url <- "https://www.sii.cl//estadisticas/nominas/PUB_EMPRESAS_PJ_2015a2019.zip"
zip <- "dev/PUB_EMPRESAS_PJ_2015a2019.zip"

if (!file.exists(zip)) {
  try(download.file(url, zip, method = "wget"))
}

if (!file.exists("dev/PUB_EMPRESAS_PJ_2015a2019.txt")) {
  unzip(zip, exdir = "dev")
}

url <- "https://www.sii.cl/estadisticas/nominas/PUB_EMPRESAS_PJ_2020.zip"
zip <- "dev/PUB_EMPRESAS_PJ_2020.zip"

if (!file.exists("dev/PUB_EMPRESAS_PJ_2015a2019.txt")) {
  try(download.file(url, zip, method = "wget"))
}

if (!file.exists("dev/PUB_EMPRESAS_PJ_2020.txt")) {
  unzip(zip, exdir = "dev")
}

sii <- data.table::fread("dev/PUB_EMPRESAS_PJ_2010a2014.txt", sep = "\t") %>%
  clean_names() %>%
  as_tibble() %>%
  select(rut, dv, razon_social = raz_n_social) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  distinct(rut, .keep_all = T) %>%
  bind_rows(
    data.table::fread("dev/PUB_EMPRESAS_PJ_2015a2019.txt", sep = "\t") %>%
      clean_names() %>%
      as_tibble() %>%
      select(rut, dv, razon_social = raz_n_social) %>%
      mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
      distinct(rut, .keep_all = T)
  ) %>%
  bind_rows(
    data.table::fread("dev/PUB_EMPRESAS_PJ_2020.txt", sep = "\t") %>%
      clean_names() %>%
      as_tibble() %>%
      select(rut, dv, razon_social = raz_n_social) %>%
      mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
      distinct(rut, .keep_all = T)
  ) %>%
  distinct(rut, .keep_all = T) %>%
  mutate(
    razon_social = str_to_upper(str_trim(
      ifelse(razon_social == "", NA, razon_social)))
  ) %>%
  drop_na()

# ruts ok, dv esta bien
# sii %>%
#   mutate(
#     dv2 = rutnum_dv(rut),
#     marca = ifelse(dv == dv2, 1, 0)
#   ) %>%
#   filter(marca == 1)

# unir ----

rutero <- rutero %>%
  select(donante, rut2_donante) %>%
  distinct(rut2_donante, .keep_all = TRUE) %>%
  mutate(rut2_donante = as.character(rut2_donante)) %>%
  left_join(
    sii %>%
      select(rut, dv, razon_social) %>%
      mutate(rut = as.character(rut)),
    by = c("rut2_donante" = "rut")
  ) %>%
  mutate(
    nombre_sii = ifelse(is.na(razon_social), 0L, 1L)
  ) %>%
  distinct(rut2_donante, .keep_all = T) %>%
  mutate(
    rut2_donante = as.integer(rut2_donante),

    razon_social = case_when(
      is.na(razon_social) ~ str_to_upper(str_trim(donante)),
      TRUE ~ razon_social
    ),

    dv = case_when(
      is.na(dv) ~ rutnum_dv(rut2_donante),
      TRUE ~ dv
    )
  ) %>%
  select(razon_social, rut = rut2_donante, dv, nombre_sii)

saveRDS(rutero, "dev/rutero.rds")
