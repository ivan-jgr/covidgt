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

# Agrupar por año-mes
summarized_data <- group_by(guatemala, date) %>% summarize(
  retail_recreation_mean = mean(retail_recreation, na.rm=TRUE),
  retail_recreation_max = max(retail_recreation, na.rm=TRUE),
  retail_recreation_min = min(retail_recreation, na.rm=TRUE),
  
  grocery_pharmacy_mean = mean(grocery_pharmacy, na.rm=TRUE),
  grocery_pharmacy_max = max(grocery_pharmacy, na.rm=TRUE),
  grocery_pharmacy_min = min(grocery_pharmacy, na.rm=TRUE),
  
  parks_mean = mean(parks, na.rm=TRUE),
  parks_max = max(parks, na.rm=TRUE),
  parks_min = min(parks, na.rm=TRUE),
  
  transit_stations_mean = mean(transit_stations, na.rm=TRUE),
  transit_stations_max = max(transit_stations, na.rm=TRUE),
  transit_stations_min = min(transit_stations, na.rm=TRUE),
  
  workplaces_mean = mean(workplaces, na.rm=TRUE),
  workplaces_max = max(workplaces, na.rm=TRUE),
  workplaces_min = min(workplaces, na.rm=TRUE),
  
  residential_mean = mean(residential, na.rm=TRUE),
  residential_max = max(residential, na.rm=TRUE),
  residential_min = min(residential, na.rm=TRUE)
)

plot_department <- function(department){
  
}