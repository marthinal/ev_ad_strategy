# Instalar y cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require("htmltools")) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if (!require("forcats")) install.packages("forcats", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(plotly)
library(htmltools)
library(forcats)

# Leer el archivo CSV
ruta_csv <- "~/ev_ad_strategy/datasources/ev_charging_patterns.csv"
data <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Convertir Charging.End.Time a formato de fecha y hora
data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Nueva paleta de colores más sobria
user_colors <- c("Commuter" = "#708090", "Long-Distance Traveler" = "#4682B4", "Casual Driver" = "#A9A9A9")
day_colors <- c("Sunday" = "#B0C4DE", "Monday" = "#778899", "Tuesday" = "#6A5ACD",
                "Wednesday" = "#8B4513", "Thursday" = "#CD853F", "Friday" = "#556B2F", "Saturday" = "#8FBC8F")
location_colors <- c("Los Angeles" = "#5F9EA0", "San Francisco" = "#4682B4",
                     "Houston" = "#A0522D", "New York" = "#6B8E23", "Chicago" = "#BDB76B")
end_hour_color <- "#2F4F4F"

# Configurar fondo blanco
theme_set(theme_minimal(base_size = 14) + theme(plot.background = element_rect(fill = "white", color = NA)))

# Análisis de User Type
user_type_count <- data %>%
  group_by(User.Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

user_type_count$User.Type <- factor(user_type_count$User.Type, levels = names(user_colors))

# Gráfico de barras para User Type
bar_chart_usertype <- ggplot(user_type_count, aes(x = User.Type, y = Count, fill = User.Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = user_colors) +
  scale_x_discrete(labels = levels(user_type_count$User.Type)) + # Corregir etiquetas
  theme(legend.position = "none")

# Análisis de Day of Week (ordenado de mayor a menor frecuencia)
dayofweek_count <- data %>%
  group_by(Day.of.Week) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

dayofweek_count$Day.of.Week <- factor(dayofweek_count$Day.of.Week, levels = dayofweek_count$Day.of.Week)

# Gráfico de barras para Day of Week (ordenado)
bar_chart_dayofweek <- ggplot(dayofweek_count, aes(x = Day.of.Week, y = Count, fill = Day.of.Week)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = day_colors) +
  scale_x_discrete(labels = levels(dayofweek_count$Day.of.Week)) + # Corregir etiquetas
  theme(legend.position = "none")

# Análisis de Charging Station Location
location_count <- data %>%
  group_by(Charging.Station.Location) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

location_count$Charging.Station.Location <- factor(location_count$Charging.Station.Location, levels = names(location_colors))

# Gráfico de barras para Charging Station Location
bar_chart_location <- ggplot(location_count, aes(x = Charging.Station.Location, y = Count, fill = Charging.Station.Location)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = location_colors) +
  scale_x_discrete(labels = levels(location_count$Charging.Station.Location)) + # Corregir etiquetas
  theme(legend.position = "none")

# Análisis por hora del día para Charging.End.Time
data$End.Hour <- as.numeric(format(data$Charging.End.Time, "%H"))
hourly_end_count <- data %>%
  group_by(End.Hour) %>%
  summarise(Count = n()) %>%
  arrange(End.Hour)

# Gráfico de histograma para hora del día (Charging.End.Time)
end_hour_histogram <- ggplot(hourly_end_count, aes(x = End.Hour, y = Count)) +
  geom_bar(stat = "identity", fill = end_hour_color) +
  scale_x_continuous(breaks = 0:23)

# Gráfico de boxplot para End.Hour
boxplot_end_hour <- ggplot(data, aes(x = "", y = End.Hour)) +
  geom_boxplot(fill = end_hour_color) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Crear la carpeta para guardar el HTML
dir.create("html_report", showWarnings = FALSE)

# Crear el contenido HTML con todos los gráficos
html_content <- tags$div(
  tags$h1("Univariate Analysis"),
  tags$h2("User Type"),
  ggplotly(bar_chart_usertype),
  tags$h2("Day of Week"),
  ggplotly(bar_chart_dayofweek),
  tags$h2("Charging Station Location"),
  ggplotly(bar_chart_location),
  tags$h2("End Hour"),
  ggplotly(end_hour_histogram),
  tags$h2("Boxplot of End Hour"),
  ggplotly(boxplot_end_hour)
)

# Análisis de Charger Type (ordenado de mayor a menor frecuencia)
charger_type_count <- data %>%
  group_by(Charger.Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

charger_type_count$Charger.Type <- factor(charger_type_count$Charger.Type, levels = charger_type_count$Charger.Type)

# Gráfico de barras para Charger Type
bar_chart_chargertype <- ggplot(charger_type_count, aes(x = Charger.Type, y = Count, fill = Charger.Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("DC Fast Charger" = "#FFD700", "Level 1" = "#8B0000", "Level 2" = "#32CD32")) +
  scale_x_discrete(labels = levels(charger_type_count$Charger.Type)) +
  theme(legend.position = "none")

# Guardar el gráfico de Charger Type como imagen en la carpeta Images
ggsave("Images/Charger_Type_Bar.png", bar_chart_chargertype, width = 8, height = 6, bg = "white")

# Añadir el análisis de Charger Type al HTML
html_content <- tags$div(
  html_content, # Mantener el contenido existente
  tags$h2("Charger Type"),
  ggplotly(bar_chart_chargertype)
)

# Guardar el reporte en un archivo HTML dentro de la carpeta html_report
htmltools::save_html(html_content, file = "html_report/univariate_analysis.html")
cat("El reporte ha sido guardado como 'html_report/univariate_analysis.html'\n")

# Guardar cada gráfico como imagen en la carpeta Images con fondo blanco
dir.create("Images", showWarnings = FALSE)

ggsave("Images/User_Type_Bar.png", bar_chart_usertype, width = 8, height = 6, bg = "white")
ggsave("Images/Day_of_Week_Bar.png", bar_chart_dayofweek, width = 8, height = 6, bg = "white")
ggsave("Images/Location_Bar.png", bar_chart_location, width = 8, height = 6, bg = "white")
ggsave("Images/End_Hour_Histogram.png", end_hour_histogram, width = 8, height = 6, bg = "white")
ggsave("Images/End_Hour_Boxplot.png", boxplot_end_hour, width = 8, height = 6, bg = "white")

cat("Las imágenes han sido guardadas en la carpeta 'Images' con fondo blanco.\n")