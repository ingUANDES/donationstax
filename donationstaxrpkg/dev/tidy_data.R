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
    fecha2 = case_when(
      nchar(fecha) == 5 ~ as.character(as.Date(as.numeric(fecha), origin = "1900-12-30")),
      nchar(fecha) == 10 ~ paste(str_sub(fecha, 7, 10), str_sub(fecha, 4, 5), str_sub(fecha, 1, 2), sep = "-")
    )
  )

d_2010_2014 <- d_2010_2014 %>%
  mutate(
    fecha2 = ifelse(fecha == "31-11-2011", "2011-11-30", fecha2),
    fecha2 = ifelse(fecha == "31/09/2011", "2011-09-30", fecha2),
    fecha2 = ifelse(fecha == "31/11/2011", "2011-11-30", fecha2),
    fecha2 = ifelse(fecha == "Nov. 2012", "2012-11-30", fecha2),
    fecha2 = ifelse(fecha == "Dic. 2012", "2012-12-31", fecha2),
    fecha2 = ifelse(fecha == "2301-2013", "2013-01-23", fecha2)
  )

d_2010_2014 <- d_2010_2014 %>%
  mutate(fecha2 = as_date(fecha2))

d_2010_2014 %>%
  filter(is.na(fecha2))

use_data(d_2010_2014, overwrite = T)
