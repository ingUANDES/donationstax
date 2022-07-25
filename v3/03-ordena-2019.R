source("00-pkgs.R")
source("99-casos-especiales.R")

rutero_sii <- readRDS("~/github/donationstax/v3/rutero_sii.rds") %>%
  mutate_if(is.character, function(x) iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))

d2020 <- readRDS("d2020.rds") %>%
  select(donante, donante_sii) %>%
  distinct()

d2019 <- read_excel("~/github/donationstax/data_local/d2019.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "text", "text", "text", "numeric",
                                  "numeric")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  mutate_if(is.character, function(x) iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))

d2019 <- d2019 %>%
  filter(nombre_del_proyecto != "NOMBRE DEL PROYECTO APROBADO")

d2019 <- d2019 %>%
  left_join(d2020)

d2019 <- d2019 %>%
  left_join(
    rutero_sii %>%
      rename(donante_sii = razon_social, rut_donante = rut,
             dv_donante = dv),
    by = c("donante_sii")
  )

d2019 %>%
  filter(is.na(donante_sii)) %>%
  distinct(donante)

d2019 <- d2019 %>%
  mutate(
    donante_sii = case_when(
      is.na(donante_sii) & !is.na(rut_donante) & !is.na(dv_donante) ~ "PERSONA NATURAL"
    )
  )

d2019 <- d2019 %>%
  donante_perdido()

d2019 %>%
  filter(is.na(donante_sii))

d2020 <- readRDS("d2020.rds") %>%
  select(donatario, donatario_sii) %>%
  distinct()

d2019 <- d2019 %>%
  left_join(d2020)

d2019 <- d2019 %>%
  left_join(
    rutero_sii %>%
      rename(donatario_sii = razon_social, rut_donatario = rut,
             dv_donatario = dv),
    by = c("donatario_sii")
  )

d2019 %>%
  filter(is.na(donatario_sii)) %>%
  select(donatario, donatario_sii) %>%
  distinct()

d2019 <- d2019 %>%
  select(ano, fecha, nombre_del_proyecto, matches("donatario"),
         matches("donante"),
         matches("monto"))

saveRDS(d2019, "d2019.rds")
