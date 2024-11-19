# Instalar y cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require("htmltools")) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if (!require("reshape2")) install.packages("reshape2", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(plotly)
library(htmltools)
library(reshape2)

# Leer el archivo CSV
ruta_csv <- "~/ev_ad_strategy/ev_charging_patterns.csv"
data <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Convertir Charging.End.Time a formato de fecha y hora
data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Crear la columna Day.of.Week
data <- data %>% mutate(Day.of.Week = weekdays(Charging.End.Time))

# Verificar si las columnas están presentes
print(head(data))

# Análisis Bivariado 1: Stacked Bar Plot para Location vs Day of Week
stacked_bar_data <- data %>%
  group_by(Charging.Station.Location, Day.of.Week) %>%
  summarise(Count = n())

stacked_bar_plot <- ggplot(stacked_bar_data, aes(x = Charging.Station.Location, y = Count, fill = Day.of.Week)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Stacked Bar Plot of Location vs Day of Week", x = "Charging Station Location", y = "Count", fill = "Day of Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Análisis Bivariado 2: Heatmap para Location vs Day of Week
heatmap_data <- data %>%
  group_by(Charging.Station.Location, Day.of.Week) %>%
  summarise(Count = n()) %>%
  dcast(Charging.Station.Location ~ Day.of.Week, value.var = "Count", fill = 0)

heatmap_plot <- ggplot(melt(heatmap_data), aes(x = variable, y = Charging.Station.Location, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Location vs Day of Week", x = "Day of Week", y = "Charging Station Location", fill = "Frequency") +
  theme_minimal()

# Convertir gráficos a interactivos
stacked_bar_plot_interactive <- ggplotly(stacked_bar_plot)
heatmap_plot_interactive <- ggplotly(heatmap_plot)

# Crear el contenido HTML con los gráficos
html_content <- tags$div(
  tags$h1("Bivariate Analysis - Location vs Day of Week"),
  tags$h2("Stacked Bar Plot of Location vs Day of Week"),
  stacked_bar_plot_interactive,
  tags$h2("Heatmap of Location vs Day of Week"),
  heatmap_plot_interactive
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content, file = "bivariate_analysis_location_dayofweek.html")
cat("The report has been saved as 'bivariate_analysis_location_dayofweek.html'\n")