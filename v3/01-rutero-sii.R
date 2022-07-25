source("dev/00-pkgs.R")

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

saveRDS(sii, "dev/rutero_sii.rds")
