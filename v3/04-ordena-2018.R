source("00-pkgs.R")
source("99-casos-especiales.R")

rutero_sii <- readRDS("~/github/donationstax/v3/rutero_sii.rds") %>%
  mutate_if(is.character, function(x) iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))

d2020 <- readRDS("d2020.rds") %>%
  select(donante, donante_sii) %>%
  distinct()

d2020 <- d2020 %>%
  bind_rows(
    readRDS("d2019.rds") %>%
      select(donante, donante_sii) %>%
      distinct()
  )

d2018 <- read_excel("~/github/donationstax/data_local/d2018.xlsx",
                    sheet = "Hoja1",
                    col_types = c("numeric",
                                  "text", "text", "text", "text", "text",
                                  "numeric", "numeric")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  mutate_if(is.character, function(x) iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))

d2018 <- d2018 %>%
  filter(nombre_del_proyecto != "NOMBRE DEL PROYECTO APROBADO")

d2018 <- d2018 %>%
  left_join(d2020)

d2018 <- d2018 %>%
  left_join(
    rutero_sii %>%
      rename(donante_sii = razon_social, rut_donante = rut,
             dv_donante = dv),
    by = c("donante_sii")
  )

d2018 %>%
  filter(is.na(donante_sii)) %>%
  distinct(donante)

d2018 <- d2018 %>%
  mutate(
    donante_sii = case_when(
      is.na(donante_sii) & !is.na(rut_donante) & !is.na(dv_donante) ~ "PERSONA NATURAL"
    )
  )

d2018 <- d2018 %>%
  donante_perdido()

d2018 %>%
  filter(is.na(donante_sii))

d2020 <- readRDS("d2020.rds") %>%
  select(donatario, donatario_sii) %>%
  distinct() %>%
  bind_rows(
    readRDS("d2019.rds") %>%
      select(donatario, donatario_sii) %>%
      distinct()
  ) %>%
  distinct()

d2018 <- d2018 %>%
  left_join(d2020)

d2018 <- d2018 %>%
  left_join(
    rutero_sii %>%
      rename(donatario_sii = razon_social, rut_donatario = rut,
             dv_donatario = dv),
    by = c("donatario_sii")
  )

d2018 %>%
  filter(is.na(donatario_sii)) %>%
  select(donatario, donatario_sii) %>%
  distinct()

d2018 <- d2018 %>%
  select(ano, fecha, nombre_del_proyecto, matches("donatario"),
         matches("donante"),
         matches("monto"))

saveRDS(d2018, "d2018.rds")
