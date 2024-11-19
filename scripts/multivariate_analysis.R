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
ruta_csv <- "~/ev_ad_strategy/ev_charging_patterns.csv"
data <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Convertir Charging.End.Time a formato de fecha y hora
data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Crear las columnas necesarias
data <- data %>%
  mutate(End.Hour = as.numeric(format(Charging.End.Time, "%H")),
         Day.of.Week = weekdays(Charging.End.Time))

# Verificar las columnas creadas
print(head(data))

# Crear el gráfico multivariado con Facet Grid
facet_grid_plot <- ggplot(data, aes(x = Day.of.Week, y = End.Hour, color = User.Type)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Charging.Station.Location) +
  labs(title = "Facet Grid of User Type, End Time, Day of Week, and Location",
       x = "Day of Week", y = "End Hour of Charging") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convertir a gráfico interactivo
facet_grid_plot_interactive <- ggplotly(facet_grid_plot)

# Crear el contenido HTML
html_content <- tags$div(
  tags$h1("Multivariate Analysis - User Type, End Time, Day of Week, and Location"),
  ggplotly(facet_grid_plot_interactive)
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content, file = "multivariate_analysis.html")
cat("El reporte ha sido guardado como 'multivariate_analysis.html'\n")