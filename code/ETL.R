# ETL
try(dir.create("data_local"))
library(tidyverse)
library(readxl)

# Origen
## datasocial.ministeriodesarrollosocial.gob.cl/donaciones/proyectos/3

# TODO: PARA EL DICCIONARIO, MATCH NO PERFECTO
projectsInfo <- read_delim("data_local/Donaciones_26112021105453.csv",
  delim = ";", escape_double = FALSE,
  col_types = cols(`FECHA APROBACIÓN` = col_date(format = "%d/%m/%Y")),
  trim_ws = TRUE, skip = 5
)

# XLS files path
urls <- c(
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2010.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2011.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2012.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2013.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2014.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2015.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/09/Donaciones_2016.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2017.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2018.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/03/Donaciones_2019.xlsx",
  "http://sociedadcivil.ministeriodesarrollosocial.gob.cl/wp-content/uploads/2021/09/Donaciones_2020.xlsx"
)

for (year in 2010:2020) {
  fout <- paste0("data_local/d", year, ".xlsx")
  if (!file.exists(fout)) {
    try(download.file(urls[year - 2009], fout))
    message(paste("Year", year, "DOWNLOADED"))
  } else {
    message(paste("Year", year, "Exists"))
  }
}

# In order to avoid downloading each time it is possible to save a backup as follows
fout2020 <- "data_local/d2020.rds"
if (!file.exists(fout2020)) {
  d2020 <- read_excel("data_local/d2020.xlsx",
                      # Sheet name
                      sheet = "Donaciones 2020", col_types = c(
                        "numeric",
                        "date", "text", "text", "text", "text",
                        "text", "text", "text", "numeric", "numeric",
                        "numeric", "text"
                      ),
                      skip = 1
  )
  
  saveRDS(d2020, file = "data_local/d2020.rds")
} else {
  d2020 <- readRDS(fout2020)
}

# Constructing data dictionary
## Reference Vignette:
## https://cran.r-project.org/web/packages/dataMeta/vignettes/dataMeta_Vignette.html

# Extract description
var_desc <- names(d2020)

# Define English variables names
names(d2020) <- c(
  "year", "date", "projectName", "projectMDSid",
  "institutionName", "institutionMDSid", "institutionRUT",
  "donorName", "donorRUT", "amountFM", "amountInstitution", "amountTotal",
  "region"
)

# Mark variables to describe their levels in detail
var_type <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

# Build Data Dictionary
linker <- build_linker(d2020, variable_description = var_desc, variable_type = var_type)

## The following does not work
## dict <- build_dict(d2020, linker = linker, option_description = NULL,
##                   prompt_varopts = FALSE)
## Error in FUN(X[[i]], ...) :
## only defined on a data frame with all numeric variables

# Standarization of variables
## Institution names
### 2020: Looks good!

# REVISAR ESTO: DICCIONARIO INSTITUCION-PROYECTO

institutionName <- d2020 %>%
  select(institutionName) %>%
  distinct()

## Project names
projectName <- d2020 %>%
  select(projectName, institutionName) %>%
  distinct() # TODO: DICCIONARIO

names(projectName) <- c("nameOLD", "institution")

projectName$name <- NA
