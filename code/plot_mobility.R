library(tidyverse)
library(dplyr)
library(tidyr)
library(plotly)
library(comprehenr)

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
summarized_data_by_month <- group_by(guatemala, date) %>% summarize(
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


get_summarized_data <- function(grouped_data){
  summarize(grouped_data,
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
}

plot_by_department <- function(department, show_legend=F){
  # Filtrar por departamento y agrupar por año-mes
  plot_data <- get_summarized_data(
    group_by(
      filter(guatemala, sub_region_1 == department), 
      date)
  )
  
  ## Residential
  fig <- plot_ly(plot_data, x=~date, y=~residential_max, type='scatter', mode='lines',
                 line = list(color='transparent'), 
                 showlegend=F, name='Max. Residential')
  fig <- fig %>% add_trace(y=~residential_min, type='scatter', mode='lines', 
                           fill='tonexty', fillcolor='rgba(200, 0, 0, 0.2)', 
                           line=list(color='transparent'), 
                           showlegend=F, name='Min. Residential')
  fig <- fig %>% add_trace(x=~date, y=~residential_mean, type='scatter', mode='lines', 
                           line=list(color='rgb(200, 0, 0)'), showlegend=show_legend, name='Residential', legendgroup=~date)
  
  ## retail_recreation
  fig <- fig %>% add_trace(plot_data, x=~date, y=~retail_recreation_max, type='scatter', mode='lines',
                 line = list(color='transparent'), 
                 showlegend=F, name='Max. Comercio y Recreación')
  fig <- fig %>% add_trace(y=~retail_recreation_min, type='scatter', mode='lines', 
                           fill='tonexty', fillcolor='rgba(0, 100, 80, 0.2)', 
                           line=list(color='transparent'), 
                           showlegend=F, name='Min. Comercio y Recreación')
  fig <- fig %>% add_trace(x=~date, y=~retail_recreation_mean, type='scatter', mode='lines', 
                           line=list(color='rgb(0, 100, 80)'), showlegend=show_legend, name='Comercio y Recreación', legendgroup=~date)
  
  ## Layout
  title <- list(
    text = sub(" Department", "", department),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.8,
    y = 0.1,
    showarrow = FALSE
  )
  
  fig <- fig %>% layout(title="Movilidad Promedio por Departamento", xaxis = list(title="Año-Mes", ticks='outside'),
                        yaxis=list(title="Movilidad (%)", ticks='outside'),  
                        annotations=title, showlegend=T)
  
  fig
}

## Todos los departamentos por día
plot_by_month <- function(month, show_legend=F){
  meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
             "Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  # Filtrar por date (año-mes) y grupar por day
  plot_data <- get_summarized_data(
    group_by(
      filter(guatemala, date == month), 
      day)
  )
  
  ## Residential
  fig <- plot_ly(plot_data, x=~day, y=~residential_max, type='scatter', mode='lines',
                 line = list(color='transparent'), 
                 showlegend=F, name='Max. Residential')
  fig <- fig %>% add_trace(y=~residential_min, type='scatter', mode='lines', 
                           fill='tonexty', fillcolor='rgba(200, 0, 0, 0.2)', 
                           line=list(color='transparent'), 
                           showlegend=F, name='Min. Residential')
  fig <- fig %>% add_trace(x=~day, y=~residential_mean, type='scatter', mode='lines', 
                           line=list(color='rgb(200, 0, 0)'), showlegend=show_legend, name='Residential', legendgroup=~day)
  
  ## retail_recreation
  fig <- fig %>% add_trace(plot_data, x=~day, y=~retail_recreation_max, type='scatter', mode='lines',
                           line = list(color='transparent'), 
                           showlegend=F, name='Max. Comercio y Recreación')
  fig <- fig %>% add_trace(y=~retail_recreation_min, type='scatter', mode='lines', 
                           fill='tonexty', fillcolor='rgba(0, 100, 80, 0.2)', 
                           line=list(color='transparent'), 
                           showlegend=F, name='Min. Comercio y Recreación')
  fig <- fig %>% add_trace(x=~day, y=~retail_recreation_mean, type='scatter', mode='lines', 
                           line=list(color='rgb(0, 100, 80)'), showlegend=show_legend, name='Comercio y Recreación', legendgroup=~day)
  
  ## Layout
  title <- list(
    text = paste(meses[as.numeric(str_sub(month, -2, -1))], str_sub(month, 1, -4), sep=" "),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.8,
    y = 0.1,
    showarrow = FALSE
  )
  
  fig <- fig %>% layout(title="Movilidad Promedio por Día", xaxis = list(title="Día", ticks='outside'),
                        yaxis=list(title="Movilidad (%)", ticks='outside'),  
                        annotations=title, showlegend=T)
  
  fig
}

departments <- tail(unique(guatemala$sub_region_1), 22)

all_plots <- to_list(
  for (d in departments)
    if (d != tail(departments, 1))
    plot_by_department(d)
  else
    plot_by_department(d, show_legend = T)
    )

months <- unique(guatemala$date)

all_plots <- to_list(
  for (m in months)
    if (m != tail(months, 1))
      plot_by_month(m)
  else
    plot_by_month(m, show_legend = T)
)

subplot(all_plots, nrows=4, shareX=T, shareY=T)


