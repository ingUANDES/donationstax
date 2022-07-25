source("00-pkgs.R")
source("99-casos-especiales.R")

rutero_sii <- readRDS("~/github/donationstax/v3/rutero_sii.rds") %>%
  mutate_if(is.character, function(x) iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))

d2020 <- read_excel("~/github/donationstax/data_local/d2020.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "text", "text", "text", "text", "text",
                                  "text", "text", "text", "text", "text")) %>%
  clean_names() %>%
  mutate_if(is.character, function(x) str_replace_all(x, '\\"', "")) %>%
  mutate_if(is.character, function(x) str_to_upper(str_trim(x))) %>%
  mutate_if(is.character, function(x) str_replace_all(x, "\\s+", " ")) %>%
  mutate_if(is.character, function(x) iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))

d2020 <- d2020 %>%
  rename(rut_donante = rut, rut_donatario = rut_institucion) %>%
  mutate(
    rut_donante = str_remove_all(rut_donante, "\\.|-"),
    rut_donatario = str_remove_all(rut_donatario, "\\.|-")
  ) %>%
  mutate(
    rut_donante = rut10_rutnum(rut_rut10(rut_donante)),
    dv_donante = rutnum_dv(rut_donante),

    rut_donatario = rut10_rutnum(rut_rut10(rut_donatario)),
    dv_donatario = rutnum_dv(rut_donatario)
  )

d2020 <- d2020 %>%
  filter(nombre_del_proyecto != "NOMBRE DEL PROYECTO APROBADO")

d2020 <- d2020 %>%
  left_join(
    rutero_sii %>%
      rename(donante_sii = razon_social, rut_donante = rut,
             dv_donante = dv),
    by = c("rut_donante", "dv_donante")
  )

d2020 <- d2020 %>%
  mutate(
    donante_sii = case_when(
      is.na(donante_sii) & !is.na(rut_donante) & !is.na(dv_donante) ~ "PERSONA NATURAL"
    )
  )

d2020 <- d2020 %>%
  donante_perdido()

d2020 %>%
  filter(is.na(donante_sii))

d2020 <- d2020 %>%
  left_join(
    rutero_sii %>%
      rename(donatario_sii = razon_social, rut_donatario = rut,
             dv_donatario = dv),
    by = c("rut_donatario", "dv_donatario")
  )

d2020 %>%
  filter(is.na(donatario_sii)) %>%
  select(donatario, donatario_sii) %>%
  distinct()

d2020 <- d2020 %>%
  select(ano, fecha, nombre_del_proyecto, folio_proyecto, matches("donatario"),
         folio_donatario = folio_institucion, matches("donante"),
         region_donde_se_ejecuta_el_proyecto,
         matches("monto"))

saveRDS(d2020, "d2020.rds")
