library(dplyr)
library(readxl)
library(purrr)
library(lubridate)
library(stringr)
library(glue)
library(janitor)

finp <- sprintf("~/github/donationstax/data_local/d%s.xlsx", 2010:2020)

d_2010_2014 <- map_df(
  finp[1:4],
  function(x) {
    message(x)
    read_excel(x,
               col_types = c("numeric", "text", "text",
                             "text", "text", "numeric", "numeric",
                             "numeric")) %>%
      clean_names()
  })

d_2010_2014 <- d_2010_2014 %>%
  mutate(
    fecha = case_when(
      nchar(fecha) == 5 ~ as.character(as.Date(as.numeric(fecha), origin = "1900-12-30")),
      nchar(fecha) == 10 ~ paste(str_sub(fecha, 7, 10), str_sub(fecha, 4, 5), str_sub(fecha, 1, 2), sep = "-")
    )
  )

d_2010_2014 <- d_2010_2014 %>%
  mutate(fecha = as_date(fecha))
