# Instalar y cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require("htmltools")) install.packages("htmltools", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(plotly)
library(htmltools)

# Leer el archivo CSV
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
data <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Paleta de colores
day_colors <- c("Sunday" = "#B0C4DE", "Monday" = "#778899", "Tuesday" = "#6A5ACD",
                "Wednesday" = "#8B4513", "Thursday" = "#CD853F", "Friday" = "#556B2F", "Saturday" = "#8FBC8F")
charger_colors <- c("Level 1" = "#D73027", "Level 2" = "#1A9850", "DC Fast Charger" = "#FFD700")

# Análisis bivariado: ChargerType vs Day of Week (Heatmap)
heatmap_data <- data %>%
  group_by(Charger.Type, Day.of.Week) %>%
  summarise(Count = n())

heatmap <- ggplot(heatmap_data, aes(x = Day.of.Week, y = Charger.Type, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FFFFCC", high = "#006837") +
  labs(title = "Charger Type vs Day of Week",
       x = "Day of Week", y = "Charger Type",
       fill = "Number of Charges") +
  theme_minimal()

# Convertir el gráfico a objeto interactivo
interactive_heatmap <- ggplotly(heatmap)

# Crear el contenido HTML con el gráfico
html_content_day <- tags$div(
  tags$h1("Charger Type vs Day of Week"),  # Solo un título principal
  interactive_heatmap
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content_day, file = "bivariate_charger_dayofweek.html")
cat("The report has been saved as 'bivariate_charger_dayofweek.html'\n")