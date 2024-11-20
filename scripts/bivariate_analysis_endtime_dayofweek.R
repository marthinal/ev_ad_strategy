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
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
data <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Convertir Charging.End.Time a formato de fecha y hora
data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Crear la columna End.Hour y Day.of.Week
data <- data %>%
  mutate(End.Hour = as.numeric(format(Charging.End.Time, "%H")),
         Day.of.Week = weekdays(Charging.End.Time))

# Verificar si las columnas están presentes
print(head(data))

# Análisis Bivariado: Crear el dataframe para el heatmap
heatmap_data <- data %>%
  group_by(End.Hour, Day.of.Week) %>%
  summarise(Count = n(), .groups = "drop")

# Crear el heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = Day.of.Week, y = End.Hour, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of End Time vs Day of Week", x = "Day of Week", y = "End Hour", fill = "Frequency") +
  theme_minimal()

# Convertir a gráfico interactivo inmediatamente
heatmap_plot_interactive <- ggplotly(heatmap_plot)

# Análisis Bivariado: Boxplot para End Time vs Day of Week
boxplot_plot <- ggplot(data, aes(x = Day.of.Week, y = End.Hour, fill = Day.of.Week)) +
  geom_boxplot() +
  labs(title = "Boxplot of End Time by Day of Week", x = "Day of Week", y = "End Hour of Charging") +
  theme_minimal() +
  theme(legend.position = "none")

# Convertir a gráfico interactivo inmediatamente
boxplot_plot_interactive <- ggplotly(boxplot_plot)

# Crear el contenido HTML con los gráficos
html_content <- tags$div(
  tags$h1("Bivariate Analysis - End Time vs Day of Week"),
  tags$h2("Heatmap of End Time vs Day of Week"),
  heatmap_plot_interactive,
  tags$h2("Boxplot of End Time by Day of Week"),
  boxplot_plot_interactive
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content, file = "bivariate_analysis_endtime_dayofweek.html")
cat("The report has been saved as 'bivariate_analysis_endtime_dayofweek_fixed_v2.html'\n")