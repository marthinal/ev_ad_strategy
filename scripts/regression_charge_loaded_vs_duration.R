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
    Calculated.Duration..hours. = Energy.Consumed..kWh. / Charging.Rate..kW., # Duración teórica basada en energía y rate
    
    # Normalizar antes del logaritmo
    Normalized.Calculated.Duration = Calculated.Duration..hours. - min(Calculated.Duration..hours., na.rm = TRUE) + 1,
    Normalized.Charge.Loaded = Charge.Loaded - min(Charge.Loaded, na.rm = TRUE) + 1,
    
    # Logaritmo de variables normalizadas
    Log.Calculated.Duration = log(Normalized.Calculated.Duration),
    Log.Charge.Loaded = log(Normalized.Charge.Loaded),
    
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
    !is.na(Calculated.Duration..hours.), # Validar cálculos realizados
    Charge.Loaded <= 100 # Filtrar registros con carga máxima del 100%
  )

# Crear una nueva variable con las columnas seleccionadas
datos_resumidos <- datos_filtrados %>%
  select(Temperature...C., Charging.Rate..kW., Charger.Type, Charging.Duration..hours.)

# Exportar datos_resumidos como tabla HTML
tabla_html <- kable(datos_resumidos, format = "html", caption = "Tabla de Datos Resumidos")

# Guardar la tabla HTML en un archivo
writeLines(tabla_html, con = "~/ev_ad_strategy/html_report/regression_summary_table.html")

# Validación de Rangos (como estaba en el script original)
cat("\n--- Validación de Rangos ---\n")
rango_sin_asignar <- datos_filtrados %>% filter(Rate.Range == "Fuera de rango")
if (nrow(rango_sin_asignar) > 0) {
  cat("Advertencia: Existen valores fuera de los rangos definidos.\n")
  print(rango_sin_asignar)
} else {
  cat("Todos los valores tienen un rango asignado correctamente.\n")
}

# Análisis por rango (como estaba en el script original)
cat("\n--- Análisis Estadístico por Rango ---\n")
resultados_por_rango <- datos_filtrados %>%
  group_by(Rate.Range) %>%
  group_map(~ {
    modelo <- lm(Log.Calculated.Duration ~ Log.Charge.Loaded, data = .x)
    resumen_modelo <- summary(modelo)
    anova_modelo <- anova(modelo)
    
    # Imprimir los resultados por rango
    cat("\nRango:", unique(.x$Rate.Range), "\n")
    cat("Resumen del modelo:\n")
    print(resumen_modelo)
    cat("\nAnálisis de Varianza (ANOVA):\n")
    print(anova_modelo)
    
    # Retornar resultados clave
    list(
      Rango = unique(.x$Rate.Range),
      R_squared = resumen_modelo$r.squared,
      Adjusted_R_squared = resumen_modelo$adj.r.squared,
      F_statistic = resumen_modelo$fstatistic[1],
      P_value = anova_modelo["Log.Charge.Loaded", "Pr(>F)"]
    )
  })

# Análisis por rango de porcentaje de carga
cat("\n--- Análisis por Rangos de Porcentaje de Carga ---\n")

# Dividir en rangos de porcentaje de carga (0-25%, 25-50%, 50-75%, 75-100%)
datos_rango_porcentaje <- datos_filtrados %>%
  mutate(Charge.Loaded.Range = cut(Charge.Loaded, breaks = c(0, 25, 50, 75, 100), include.lowest = TRUE)) %>%
  group_by(Charge.Loaded.Range) %>%
  summarise(
    Correlation_Temp_Duration = cor(Temperature...C., Calculated.Duration..hours., use = "complete.obs"),
    n = n() # Número de observaciones en cada rango
  )

# Mostrar resultados
print(datos_rango_porcentaje)

# Crear un gráfico para visualizar la correlación por rango
ggplot(datos_filtrados %>%
         mutate(Charge.Loaded.Range = cut(Charge.Loaded, breaks = c(0, 25, 50, 75, 100), include.lowest = TRUE)),
       aes(x = Temperature...C., y = Calculated.Duration..hours., color = Charge.Loaded.Range)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) + # Ajustar una línea de regresión para cada rango
  labs(
    title = "Relación entre Temperatura y Tiempo de Carga por Rangos de Porcentaje",
    x = "Temperatura (°C)",
    y = "Duración de Carga (horas)",
    color = "Rango de Carga (%)"
  ) +
  theme_minimal()




# Crear una nueva tabla con las columnas seleccionadas y ordenarla por Rate
tabla_ordenada <- datos_filtrados %>%
  select(
    Temperature...C., 
    Calculated.Duration..hours., 
    Charging.Rate..kW., 
    Charger.Type, 
    Rate.Range
  ) %>%
  arrange(Charging.Rate..kW.) # Ordenar por Rate de menor a mayor

# Exportar la tabla ordenada como HTML
tabla_ordenada_html <- kable(
  tabla_ordenada, 
  format = "html", 
  col.names = c("Temperatura (°C)", "Duración de Carga (horas)", "Rate (kW)", "Tipo de Cargador", "Rango de Rate"), 
  caption = "Tabla Ordenada por Rate"
)

# Guardar la tabla HTML en un archivo
writeLines(tabla_ordenada_html, con = "~/ev_ad_strategy/html_report/tabla_ordenada_por_rate.html")



# Crear gráfico interactivo con líneas segmentadas por rango
plot <- ggplot(datos_filtrados, aes(x = Log.Charge.Loaded, y = Log.Calculated.Duration)) +
  geom_point(aes(color = Rate.Range), alpha = 0.7) + # Puntos diferenciados por rango
  geom_smooth(method = "lm", aes(color = Rate.Range, group = Rate.Range), se = FALSE, linewidth = 1.5) + # Líneas de regresión segmentadas
  labs(
    title = "Log(Calculated Duration) vs Log(Charge Loaded) (Regresión por Rate Range)",
    x = "Log(Charge Loaded)",
    y = "Log(Calculated Duration) (hours)",
    color = "Rate Range"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convertir a gráfico interactivo
interactive_plot <- ggplotly(plot) %>%
  layout(
    legend = list(title = list(text = "Rate Range")),
    hovermode = "closest"
  )

# Mostrar el gráfico interactivo
interactive_plot

# Guardar el gráfico interactivo en un archivo HTML
htmlwidgets::saveWidget(interactive_plot, "~/ev_ad_strategy/html_report/regression_interactive_plot.html", selfcontained = TRUE)



datos_filtrados <- datos_filtrados %>%
  mutate(Temperature.Range = cut(
    Temperature...C.,
    breaks = c(-Inf, 0, 15, 30, Inf), # Nuevos rangos: -10 a 0, 0 a 15, 15 a 30, y 30 en adelante
    labels = c("-10 to 0", "0 to 15", "15 to 30", "30 and above"),
    include.lowest = TRUE
  ))

# Calcular estadísticas por rango de Rate y temperatura con los nuevos rangos
datos_estadisticas_temperatura <- datos_filtrados %>%
  group_by(Rate.Range, Temperature.Range) %>%
  summarise(
    n = n(), # Número de observaciones
    Correlation_Temp_Duration = cor(Temperature...C., Calculated.Duration..hours., use = "complete.obs"),
    Model = list(lm(Calculated.Duration..hours. ~ Temperature...C., data = cur_data()))
  ) %>%
  filter(n > 5) # Filtrar combinaciones con pocos datos

# Mostrar resultados detallados por cada combinación de Rate y Temperatura
datos_estadisticas_temperatura %>%
  rowwise() %>%
  mutate(Model_Summary = list(summary(Model))) %>%
  group_walk(~ {
    cat("\nRango de Rate:", .x$Rate.Range, 
        "| Rango de Temperatura:", as.character(.x$Temperature.Range), "\n")
    cat("Número de observaciones:", .x$n, "\n")
    cat("Correlación entre Temperatura y Duración:", .x$Correlation_Temp_Duration, "\n")
    print(.x$Model_Summary)
  })





# Crear la columna Temperature.Squared antes de agrupar
datos_filtrados <- datos_filtrados %>%
  mutate(Temperature.Squared = Temperature...C. ^ 2)

# Comparar modelos lineales y cuadráticos
datos_estadisticas_no_lineales <- datos_filtrados %>%
  group_by(Rate.Range, Temperature.Range) %>%
  summarise(
    n = n(),
    Correlation_Spearman = cor(Temperature...C., Calculated.Duration..hours., method = "spearman"),
    Model_Linear = list(lm(Calculated.Duration..hours. ~ Temperature...C., data = cur_data())),
    Model_Quadratic = list(lm(Calculated.Duration..hours. ~ Temperature...C. + Temperature.Squared, data = cur_data())),
    Linear_AIC = AIC(lm(Calculated.Duration..hours. ~ Temperature...C., data = cur_data())),
    Quadratic_AIC = AIC(lm(Calculated.Duration..hours. ~ Temperature...C. + Temperature.Squared, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(
    AIC_Difference = Linear_AIC - Quadratic_AIC, # Diferencia de AIC entre modelos
    Best_Model = ifelse(AIC_Difference > 2, "Quadratic", "Linear") # Seleccionar el mejor modelo
  )

# Mostrar resultados detallados
datos_estadisticas_no_lineales %>%
  rowwise() %>%
  mutate(
    Linear_R2 = summary(Model_Linear)$r.squared,
    Quadratic_R2 = summary(Model_Quadratic)$r.squared
  ) %>%
  group_walk(~ {
    cat("\nRango de Rate:", .x$Rate.Range, 
        "| Rango de Temperatura:", as.character(.x$Temperature.Range), "\n")
    cat("Número de observaciones:", .x$n, "\n")
    cat("Spearman Correlation:", .x$Correlation_Spearman, "\n")
    cat("Linear AIC:", .x$Linear_AIC, "| Quadratic AIC:", .x$Quadratic_AIC, "\n")
    cat("Mejor Modelo:", .x$Best_Model, "\n")
    cat("Linear R-squared:", .x$Linear_R2, "| Quadratic R-squared:", .x$Quadratic_R2, "\n")
  })