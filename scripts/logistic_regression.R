# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Cargar el dataset
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
datos <- read.csv(ruta_csv)

# Filtrar los datos iniciales
datos_filtrados <- datos %>%
  filter(
    Charging.Rate..kW. >= 0,
    Charging.Rate..kW. <= 100,
    !is.na(Charging.Rate..kW.),
    State.of.Charge..Start... <= State.of.Charge..End..., # Validar consistencia en State of Charge
    !is.na(Energy.Consumed..kWh.) & !is.na(Charging.Cost..USD.) & !is.na(Battery.Capacity..kWh.) # Validar valores faltantes
  ) %>%
  mutate(
    Charge.Loaded = State.of.Charge..End... - State.of.Charge..Start..., # Carga realizada
    Theoretical.Energy.Consumed = (Charge.Loaded / 100) * Battery.Capacity..kWh., # Energía teórica consumida
    Energy.Difference = abs(Theoretical.Energy.Consumed - Energy.Consumed..kWh.), # Diferencia teórica y real
    Valid.Data = Energy.Difference < 10 & Energy.Consumed..kWh. >= Theoretical.Energy.Consumed, # Validar energía
    Calculated.Duration..hours. = Energy.Consumed..kWh. / Charging.Rate..kW., # Duración estimada en horas
    Rate.Range = case_when(
      Charging.Rate..kW. >= 0 & Charging.Rate..kW. < 10 ~ "0-10 kW",
      Charging.Rate..kW. >= 10 & Charging.Rate..kW. < 35 ~ "10-35 kW",
      Charging.Rate..kW. >= 35 & Charging.Rate..kW. <= 100 ~ "35-100 kW",
      TRUE ~ "Fuera de rango"
    )
  ) %>%
  filter(
    Valid.Data, # Filtrar datos válidos
    !is.na(Calculated.Duration..hours.), # Validar cálculos realizados
    Charge.Loaded <= 100 # Filtrar registros con carga máxima del 100%
  )

# Filtrar los datos del segmento de rate "35-100 kW"
datos_segmento <- datos_filtrados %>%
  filter(Rate.Range == "35-100 kW") %>%
  mutate(
    # Crear la variable binaria Action.Commercial basada en tiempo estimado
    Action.Commercial = ifelse(Calculated.Duration..hours. > 1, 1, 0)
  ) %>%
  filter(!is.na(Action.Commercial)) # Filtrar cualquier NA en la variable dependiente

# Verificar que Action.Commercial es binaria
cat("Distribución de Action.Commercial:\n")
print(table(datos_segmento$Action.Commercial, useNA = "ifany"))

# Ajustar el modelo logístico
modelo_logistico <- glm(Action.Commercial ~ Charge.Loaded,
                        family = "binomial",
                        data = datos_segmento)

# Resumen del modelo
cat("\n--- Resumen del Modelo Logístico ---\n")
print(summary(modelo_logistico))

# Agregar las predicciones al dataset
datos_segmento <- datos_segmento %>%
  mutate(
    Predicted_Prob = predict(modelo_logistico, newdata = ., type = "response"),
    Predicted_Class = ifelse(Predicted_Prob > 0.6, 1, 0) # Clasificar según el nuevo umbral
  )

# Crear el gráfico mejorado
grafico <- ggplot(datos_segmento, aes(x = Charge.Loaded, y = Predicted_Prob)) +
  geom_point(aes(color = as.factor(Predicted_Class)), alpha = 0.5, size = 3) + # Aumentar el tamaño y ajustar la transparencia de los puntos
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, linewidth = 1) + # Reducir el grosor de la línea
  labs(
    title = "Modelo de Regresión Logística (Umbral 0.6)",
    x = "Carga Realizada (%)",
    y = "Probabilidad Predicha de Acción Comercial",
    color = "Predicción"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top", # Mover la leyenda a la parte superior
    plot.title = element_text(hjust = 0.5) # Centrar el título
  )

# Guardar el gráfico como imagen PNG
ruta_imagen <- "~/ev_ad_strategy/html_report/logistic_regression_plot.png"
ggsave(ruta_imagen, plot = grafico, width = 10, height = 6, dpi = 300)

# Mensaje de confirmación
cat("\nGráfico guardado en:", ruta_imagen, "\n")