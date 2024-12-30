# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(plotly) # Para gráficos interactivos
library(lubridate)
library(knitr)

# Cargar el dataset
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
datos <- read.csv(ruta_csv)

# Filtrar datos y aplicar validaciones
datos_filtrados <- datos %>%
  filter(
    Charging.Rate..kW. >= 0,
    Charging.Rate..kW. <= 100,
    !is.na(Charging.Rate..kW.), # Eliminar registros con NA en Rate
    State.of.Charge..Start... <= State.of.Charge..End..., # Validar consistencia en State of Charge
    !is.na(Energy.Consumed..kWh.) & !is.na(Charging.Cost..USD.) & !is.na(Battery.Capacity..kWh.) # Validar valores faltantes
  )

# Crear las columnas calculadas
datos_filtrados <- datos_filtrados %>%
  mutate(
    Charge.Loaded = State.of.Charge..End... - State.of.Charge..Start..., # Calcular la carga realizada
    Theoretical.Energy.Consumed = (Charge.Loaded / 100) * Battery.Capacity..kWh., # Energía teórica consumida
    Energy.Difference = abs(Theoretical.Energy.Consumed - Energy.Consumed..kWh.), # Diferencia teórica y real
    Valid.Data = Energy.Difference < 10 & Energy.Consumed..kWh. >= Theoretical.Energy.Consumed, # Validar energía adecuada
    
    # Crear nuevos rangos de rate según lo indicado
    Rate.Range = case_when(
      Charging.Rate..kW. >= 0 & Charging.Rate..kW. < 10 ~ "0-10 kW",
      Charging.Rate..kW. >= 10 & Charging.Rate..kW. < 35 ~ "10-35 kW",
      Charging.Rate..kW. >= 35 & Charging.Rate..kW. <= 100 ~ "35-100 kW",
      TRUE ~ "Fuera de rango"
    )
  ) %>%
  filter(
    Valid.Data, # Filtrar solo datos válidos según la energía consumida
    Charge.Loaded <= 100 # Filtrar registros con carga máxima del 100%
  )

# Crear tabla HTML con columnas específicas ordenadas por Charging.Rate..kW.
datos_resumidos <- datos_filtrados %>%
  select(Rate.Range, Charger.Type, Charging.Rate..kW.) %>%
  arrange(Charging.Rate..kW.) # Ordenar en orden ascendente por Charging.Rate..kW.

# Exportar datos_resumidos como tabla HTML
tabla_html <- kable(datos_resumidos, format = "html", caption = "Resumen de Charging Rates y Tipo de Cargador")

# Guardar la tabla HTML en un archivo
ruta_salida <- "~/ev_ad_strategy/html_report/regression_summary_table.html"
writeLines(tabla_html, con = ruta_salida)

# Mensaje de confirmación
cat("La tabla HTML ordenada se ha generado correctamente en:", ruta_salida, "\n")