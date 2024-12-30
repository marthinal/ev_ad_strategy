# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Cargar el dataset
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
datos <- read.csv(ruta_csv)

# Filtrar y procesar los datos
datos_filtrados <- datos %>%
  filter(
    !is.na(Charging.Rate..kW.), # Eliminar NA en Charging Rate
    !is.na(Charging.Duration..hours.), # Eliminar NA en Charging Duration
    Charging.Rate..kW. > 0 & Charging.Duration..hours. > 0 # Valores positivos
  )

# Estadística descriptiva
cat("\n--- Estadística descriptiva: Charging Duration vs Charging Rate ---\n")
summary(datos_filtrados$Charging.Rate..kW.)
summary(datos_filtrados$Charging.Duration..hours.)

# Calcular correlación de Pearson
correlacion <- cor(datos_filtrados$Charging.Rate..kW., datos_filtrados$Charging.Duration..hours.)
cat("\nCoeficiente de correlación de Pearson:", correlacion, "\n")

# Crear el modelo de regresión lineal
modelo <- lm(Charging.Duration..hours. ~ Charging.Rate..kW., data = datos_filtrados)
summary(modelo)

# Gráfico de dispersión con línea de regresión
ggplot(datos_filtrados, aes(x = Charging.Rate..kW., y = Charging.Duration..hours.)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Charging Duration vs Charging Rate",
    x = "Charging Rate (kW)",
    y = "Charging Duration (hours)"
  ) +
  theme_minimal()