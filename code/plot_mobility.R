library(tidyverse)
library(dplyr)
library(tidyr)

# Directorio
setwd("~/Research/data/covid/")

# Columnas a eliminar
drop_column_names <- c(
  "country_region_code", # Para todas las filas es GT 
  "country_region",      # Para todas las filas es Guatemala
  "metro_area",          # Para todas es vacía
  "iso_3166_2_code",     # No nos interesa
  "census_fips_code")    # No nos interesa

data2020 <- read_csv("data/2020_GT_Region_Mobility_Report.csv")
data2021 <- read_csv("data/2021_GT_Region_Mobility_Report.csv")

# Unir los dataframes de ambos años
# Eliminar las columnas que no nos interesan
# Renombrar las categorías
guatemala <- rbind(data2020, data2021) %>%
  select(-all_of(drop_column_names)) %>% rename(
    retail_recreation = retail_and_recreation_percent_change_from_baseline,
    grocery_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
    parks = parks_percent_change_from_baseline,
    transit_stations = transit_stations_percent_change_from_baseline,
    workplaces = workplaces_percent_change_from_baseline,
    residential = residential_percent_change_from_baseline
  )

summary(guatemala)

# Convertir el campo de date a formato fecha y separarlo en año-mes y día
guatemala$date = as.Date(guatemala$date)
guatemala <- guatemala %>% 
  separate(date, sep="-", into = c("year", "month", "day")) %>% 
  unite(date, year, month, sep="-")
