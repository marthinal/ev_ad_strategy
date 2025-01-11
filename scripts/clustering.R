# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(lubridate)

# Configurar rutas
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
ruta_imagen_clustering <- "~/ev_ad_strategy/images/clustering_plot.png"
ruta_imagen_barras <- "~/ev_ad_strategy/images/clustering_bar_chart.png"

# Cargar el dataset
datos <- read.csv(ruta_csv)

# Filtrar datos y aplicar validaciones
datos_filtrados <- datos %>%
  filter(
    !is.na(Charging.Rate..kW.), # Eliminar registros con NA en Rate
    State.of.Charge..Start... <= State.of.Charge..End..., # Validar consistencia en State of Charge
    !is.na(Energy.Consumed..kWh.) & !is.na(Charging.Cost..USD.) & !is.na(Battery.Capacity..kWh.) # Validar valores faltantes
  ) %>%
  mutate(
    Charge.Loaded = State.of.Charge..End... - State.of.Charge..Start..., # Calcular la carga realizada
    Theoretical.Energy.Consumed = (Charge.Loaded / 100) * Battery.Capacity..kWh., # Energía teórica consumida
    Energy.Difference = abs(Theoretical.Energy.Consumed - Energy.Consumed..kWh.), # Diferencia teórica y real
    Valid.Data = Energy.Difference < 10 & Energy.Consumed..kWh. >= Theoretical.Energy.Consumed, # Validar energía adecuada
    Calculated.Duration..hours. = Energy.Consumed..kWh. / Charging.Rate..kW., # Duración teórica basada en energía y rate
    
    # Normalizar antes del logaritmo
    Normalized.Calculated.Duration = Calculated.Duration..hours. - min(Calculated.Duration..hours., na.rm = TRUE) + 1,
    Normalized.Charge.Loaded = Charge.Loaded - min(Charge.Loaded, na.rm = TRUE) + 1,
    
    # Extraer la hora de la carga
    Hour = lubridate::hour(Charging.End.Time), # Extraer la hora de finalización
    Estimated.Start.Time = pmax(Hour - Calculated.Duration..hours., 0) # Hora estimada de inicio
  ) %>%
  filter(
    Valid.Data, # Filtrar solo datos válidos según la energía consumida
    !is.na(Calculated.Duration..hours.), # Validar cálculos realizados
    Charge.Loaded <= 100 # Filtrar registros con carga máxima del 100%
  )

# Clustering con Charge.Loaded y Estimated.Start.Time
# Escalar las variables seleccionadas
scaled_data <- datos_filtrados %>%
  select(Charge.Loaded, Estimated.Start.Time) %>%
  scale() # Escalamos los datos para dar igualdad de peso

# Aplicar K-Means con 2 clusters
set.seed(123) # Para resultados reproducibles
clustering <- kmeans(scaled_data, centers = 2) # Cambiamos a 2 clusters

# Agregar los resultados de los clusters al dataset
datos_filtrados$Cluster <- clustering$cluster

# Plot 1: Visualización del clustering
clustering_plot <- ggplot(datos_filtrados, aes(x = Estimated.Start.Time, y = Charge.Loaded, color = as.factor(Cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Clustering: Hora estimada de inicio por Cluster",
    x = "Hora estimada de inicio",
    y = "Carga Realizada (Charge.Loaded)",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"), # Fondo blanco
    panel.background = element_rect(fill = "white")
  )

# Guardar el primer plot
ggsave(ruta_imagen_clustering, plot = clustering_plot, width = 8, height = 6, dpi = 300, bg = "white")

# Agrupar horas en intervalos de 2 horas y calcular frecuencias por cluster
analisis_clusters <- datos_filtrados %>%
  mutate(Hour_Group = cut(Estimated.Start.Time, breaks = seq(0, 24, by = 2), include.lowest = TRUE)) %>%
  group_by(Cluster, Hour_Group) %>%
  summarize(Frequency = n(), .groups = "drop") %>%
  arrange(Cluster, desc(Frequency))

# Mostrar todos los resultados
cat("\n--- Frecuencia de horas agrupadas por Cluster (Todas las filas) ---\n")
print(analisis_clusters, n = Inf) # Mostrar todas las filas sin truncar

# Plot 2: Frecuencia de horas agrupadas
barras_plot <- ggplot(analisis_clusters, aes(x = Hour_Group, y = Frequency, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frecuencia de Horas Agrupadas por Cluster",
    x = "Intervalo de Hora Estimada de Inicio",
    y = "Frecuencia",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = "white"), # Fondo blanco
    panel.background = element_rect(fill = "white")
  )

# Guardar el segundo plot
ggsave(ruta_imagen_barras, plot = barras_plot, width = 8, height = 6, dpi = 300, bg = "white")

# Resumen del análisis estadístico
cat("\n--- Análisis Estadístico de Clustering ---\n")
cat("Clustering aplicado a las variables Charge.Loaded y Estimated.Start.Time\n")
cat("Resultados del clustering agrupados por intervalos de 2 horas:\n")
print(analisis_clusters, n = Inf) # Mostrar todas las filas