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
    !is.na(Charging.Rate..kW.),
    State.of.Charge..Start... <= State.of.Charge..End...,
    !is.na(Energy.Consumed..kWh.) & !is.na(Charging.Cost..USD.) & !is.na(Battery.Capacity..kWh.)
  )

# Crear las columnas calculadas
datos_filtrados <- datos_filtrados %>%
  mutate(
    Charge.Loaded = State.of.Charge..End... - State.of.Charge..Start..., 
    Calculated.Duration..hours. = Energy.Consumed..kWh. / Charging.Rate..kW.,
    Rate.Range = case_when(
      Charging.Rate..kW. >= 0 & Charging.Rate..kW. < 10 ~ "0-10 kW",
      Charging.Rate..kW. >= 10 & Charging.Rate..kW. < 35 ~ "10-35 kW",
      Charging.Rate..kW. >= 35 & Charging.Rate..kW. <= 100 ~ "35-100 kW",
      TRUE ~ "Fuera de rango"
    )
  ) %>%
  filter(
    !is.na(Calculated.Duration..hours.), 
    Charge.Loaded <= 100
  )

# Analítica descriptiva y regresión por rango
cat("\n--- Analítica Descriptiva y Regresión Lineal por Rangos de Rate ---\n")

# Agrupar datos por Rate.Range
datos_por_rango <- datos_filtrados %>% group_by(Rate.Range) %>% group_split()
nombres_rango <- datos_filtrados %>% distinct(Rate.Range) %>% pull(Rate.Range)

# Bucle para calcular estadísticas y regresión
resultados <- list()
for (i in seq_along(datos_por_rango)) {
  grupo <- datos_por_rango[[i]]
  nombre_rango <- nombres_rango[i]
  
  cat("\nRango de Rate:", nombre_rango, "\n")
  
  # Estadísticas descriptivas
  cat("Estadísticas descriptivas:\n")
  print(summary(grupo$Charging.Rate..kW.))
  
  # Correlación entre variables
  correlacion <- cor(grupo$Charging.Rate..kW., grupo$Calculated.Duration..hours., use = "complete.obs")
  cat("Coeficiente de correlación entre Rate y Duración:", round(correlacion, 4), "\n")
  
  # Modelo de regresión lineal
  modelo <- lm(Calculated.Duration..hours. ~ Charging.Rate..kW., data = grupo)
  resumen_modelo <- summary(modelo)
  
  cat("Resumen del modelo de regresión lineal:\n")
  print(resumen_modelo)
  
  # Guardar resultados
  resultados[[nombre_rango]] <- list(
    Rango = nombre_rango,
    Correlacion = correlacion,
    R_squared = resumen_modelo$r.squared,
    Coeficientes = resumen_modelo$coefficients
  )
}

# Convertir resultados a una tabla resumida
tabla_resultados <- do.call(rbind, lapply(resultados, function(x) {
  data.frame(
    Rango = x$Rango,
    Correlacion = x$Correlacion,
    R_squared = x$R_squared,
    Intercept = x$Coeficientes[1, "Estimate"],
    Slope = x$Coeficientes[2, "Estimate"]
  )
}))

# Mostrar la tabla final con los resultados
print(tabla_resultados)

# Exportar los resultados a HTML
tabla_html <- kable(tabla_resultados, format = "html", caption = "Resumen de Regresión Lineal por Rango")
writeLines(tabla_html, con = "~/ev_ad_strategy/html_report/regression_analysis_summary.html")