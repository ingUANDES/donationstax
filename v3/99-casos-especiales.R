donante_perdido <- function(d) {
  d %>%
    mutate(
      rut_donante = case_when(
        # 2020
        donante == "FALABELLA RETAIL S.A." ~ 77261280L,
        donante == "SERVICIO DE INFORMATICA Y REDES LINUXIT SPA" ~ 76310051L,
        TRUE ~ rut_donante
      ),
      dv_donante = case_when(
        # 2020
        donante == "FALABELLA RETAIL S.A." ~ "K",
        donante == "SERVICIO DE INFORMATICA Y REDES LINUXIT SPA" ~ "0",
        TRUE ~ dv_donante
      ),
      donante_sii = case_when(
        # 2020
        donante == "FALABELLA RETAIL S.A." ~ "FALABELLA RETAIL S.A.",
        donante == "SERVICIO DE INFORMATICA Y REDES LINUXIT SPA" ~ "SERVICIOS DE INFORMATICA Y REDES LINUXIT SPA",
        TRUE ~ donante_sii
      )
    )
}
