# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Cargar el dataset
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
ruta_imagen <- "~/ev_ad_strategy/images/charge_loaded_vs_duration.png"
datos <- read.csv(ruta_csv)

# Filtrar y procesar los datos
datos_filtrados <- datos %>%
  filter(
    !is.na(State.of.Charge..Start...) & !is.na(State.of.Charge..End...), # Eliminar NA en columnas necesarias
    !is.na(Charging.Duration..hours.), # Eliminar NA en Charging Duration
    State.of.Charge..Start... >= 0 & State.of.Charge..End... >= 0 # Valores positivos
  ) %>%
  mutate(
    Charge.Loaded = State.of.Charge..End... - State.of.Charge..Start... # Calcular Charge Loaded
  ) %>%
  filter(
    Charge.Loaded > 0 & Charging.Duration..hours. > 0 # Filtrar valores positivos
  )

# Estadística descriptiva
cat("\n--- Estadística descriptiva: Charging Duration vs Charge Loaded ---\n")
summary(datos_filtrados$Charge.Loaded)
summary(datos_filtrados$Charging.Duration..hours.)

# Calcular correlación de Pearson
correlacion <- cor(datos_filtrados$Charge.Loaded, datos_filtrados$Charging.Duration..hours.)
cat("\nCoeficiente de correlación de Pearson:", correlacion, "\n")

# Crear el modelo de regresión lineal
modelo <- lm(Charging.Duration..hours. ~ Charge.Loaded, data = datos_filtrados)
summary(modelo)

# Crear y guardar el gráfico
plot <- ggplot(datos_filtrados, aes(x = Charge.Loaded, y = Charging.Duration..hours.)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Charging Duration vs Charge Loaded",
    x = "Charge Loaded (%)",
    y = "Charging Duration (hours)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Guardar como imagen
ggsave(filename = ruta_imagen, plot = plot, width = 8, height = 6, bg = "white")
cat("\nGráfico guardado en:", ruta_imagen, "\n")