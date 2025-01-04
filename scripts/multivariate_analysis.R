# Instalar y cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require("htmltools")) install.packages("htmltools", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(plotly)
library(htmltools)

# Función para cargar datos
load_data <- function(ruta_csv) {
  data <- read.csv(ruta_csv, stringsAsFactors = FALSE)
  data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data <- data %>%
    mutate(End.Hour = as.numeric(format(Charging.End.Time, "%H")),
           Day.of.Week = weekdays(Charging.End.Time, abbreviate = TRUE)) %>%  # Abreviaturas de los días
    mutate(Day.of.Week = factor(Day.of.Week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
  return(data)
}

# Función para generar el gráfico multivariado con iniciales de los días de la semana
generate_multivariate_plot <- function(data) {
  ggplot(data, aes(x = Day.of.Week, y = End.Hour, color = User.Type)) +
    geom_point(alpha = 0.7) +
    facet_grid(Charger.Type ~ Charging.Station.Location) +
    labs(title = "Facet Grid of User Type, Charger Type, End Time, Day of Week, and Location",
         x = "Day of Week", y = "End Hour of Charging") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas para mejor legibilidad
    )
}

# Exportar las funciones
multivariate_analysis <- list(
  load_data = load_data,
  generate_multivariate_plot = generate_multivariate_plot
)