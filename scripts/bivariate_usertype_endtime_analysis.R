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

# Convertir Charging.End.Time a formato de fecha y hora
data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
data$End.Hour <- as.numeric(format(data$Charging.End.Time, "%H"))

# Nueva paleta de colores
colors <- c("Commuter" = "#4B6F91", "Long-Distance Traveler" = "#E69F00", "Casual Driver" = "#56B4E9")
line_plot_color <- "#5A9BD4"  # Azul recomendado para gráficos de línea
boxplot_color <- "#0072B2"    # Azul para el boxplot

# Análisis bivariado: Line Plot de UserType vs EndTime
line_data <- data %>%
  group_by(User.Type, End.Hour) %>%
  summarise(Count = n())

line_plot <- ggplot(line_data, aes(x = End.Hour, y = Count, color = User.Type, group = User.Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  labs(title = "Bivariate Analysis - User Type vs Charging End Time",
       x = "Hour of Day", y = "Number of Charges",
       color = "User Type") +
  theme_minimal()

# Análisis bivariado: Boxplot de UserType vs EndTime
boxplot <- ggplot(data, aes(x = User.Type, y = End.Hour, fill = User.Type)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.shape = 16) +
  scale_fill_manual(values = colors) +
  labs(title = "Boxplot of User Type vs Charging End Time",
       x = "User Type", y = "End Hour of Charging") +
  theme_minimal() +
  theme(legend.position = "none")

# Convertir los gráficos a objetos interactivos con Plotly
interactive_line_plot <- ggplotly(line_plot)
interactive_boxplot <- ggplotly(boxplot)

# Crear el contenido HTML con ambos gráficos
html_content <- tags$div(
  tags$h1("Bivariate Analysis - User Type vs Charging End Time"),
  tags$h2("Line Plot of User Type vs Charging End Time"),
  interactive_line_plot,
  tags$h2("Boxplot of User Type vs Charging End Time"),
  interactive_boxplot
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content, file = "bivariate_usertype_endtime_analysis.html")
cat("The report has been saved as 'bivariate_usertype_endtime_analysis.html'\n")