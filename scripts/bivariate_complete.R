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

# Ordenar los días de la semana
data$Day.of.Week <- factor(data$Day.of.Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Crear carpetas para imágenes y reportes HTML
dir.create("images", showWarnings = FALSE)
dir.create("html_report", showWarnings = FALSE)

# Función para guardar gráficos e informes
guardar_graficos <- function(nombre, grafico) {
  # Guardar imagen
  ggsave(paste0("images/", nombre, ".png"), grafico, width = 8, height = 6, bg = "white")
  # Convertir a objeto interactivo
  interactivo <- ggplotly(grafico)
  # Crear contenido HTML
  html_content <- tags$div(
    tags$h1(paste0("Bivariate Analysis - ", nombre)),
    interactivo
  )
  # Guardar reporte HTML
  htmltools::save_html(html_content, file = paste0("html_report/", nombre, ".html"))
  cat(paste0("Reporte guardado como html_report/", nombre, ".html\n"))
}

# 1. UserType vs EndTime
line_data <- data %>%
  mutate(End.Hour = as.numeric(format(as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S"), "%H"))) %>%
  group_by(User.Type, End.Hour) %>%
  summarise(Count = n())

line_plot <- ggplot(line_data, aes(x = End.Hour, y = Count, color = User.Type, group = User.Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "User Type vs End Time",
       x = "End Hour", y = "Number of Charges",
       color = "User Type") +
  theme_minimal()

guardar_graficos("UserType_vs_EndTime", line_plot)

# 2. UserType vs Location
location_data <- data %>%
  group_by(User.Type, Charging.Station.Location) %>%
  summarise(Count = n())

location_bar <- ggplot(location_data, aes(x = Charging.Station.Location, y = Count, fill = User.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "User Type vs Location",
       x = "Location", y = "Number of Charges",
       fill = "User Type") +
  theme_minimal()

guardar_graficos("UserType_vs_Location", location_bar)

# 3. UserType vs DayOfWeek
dayofweek_data <- data %>%
  group_by(User.Type, Day.of.Week) %>%
  summarise(Count = n())

dayofweek_bar <- ggplot(dayofweek_data, aes(x = Day.of.Week, y = Count, fill = User.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "User Type vs Day of Week",
       x = "Day of Week", y = "Number of Charges",
       fill = "User Type") +
  theme_minimal()

guardar_graficos("UserType_vs_DayOfWeek", dayofweek_bar)

# 4. ChargerType vs DayOfWeek
charger_dayofweek_data <- data %>%
  group_by(Charger.Type, Day.of.Week) %>%
  summarise(Count = n())

charger_dayofweek_heatmap <- ggplot(charger_dayofweek_data, aes(x = Day.of.Week, y = Charger.Type, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FFFFCC", high = "#006837") +
  labs(title = "Charger Type vs Day of Week",
       x = "Day of Week", y = "Charger Type",
       fill = "Number of Charges") +
  theme_minimal()

guardar_graficos("ChargerType_vs_DayOfWeek", charger_dayofweek_heatmap)

# 5. ChargerType vs UserType
charger_usertype_data <- data %>%
  group_by(Charger.Type, User.Type) %>%
  summarise(Count = n())

charger_usertype_bar <- ggplot(charger_usertype_data, aes(x = Charger.Type, y = Count, fill = User.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Charger Type vs User Type",
       x = "Charger Type", y = "Number of Charges",
       fill = "User Type") +
  theme_minimal()

guardar_graficos("ChargerType_vs_UserType", charger_usertype_bar)

# 6. EndTime vs DayOfWeek
endtime_dayofweek_data <- data %>%
  mutate(End.Hour = as.numeric(format(as.POSIXct(data$Charging.End.Time, format="%Y-%m-%d %H:%M:%S"), "%H"))) %>%
  group_by(End.Hour, Day.of.Week) %>%
  summarise(Count = n())

endtime_dayofweek_heatmap <- ggplot(endtime_dayofweek_data, aes(x = Day.of.Week, y = End.Hour, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FFFFCC", high = "#006837") +
  labs(title = "End Time vs Day of Week",
       x = "Day of Week", y = "End Hour",
       fill = "Number of Charges") +
  theme_minimal()

guardar_graficos("EndTime_vs_DayOfWeek", endtime_dayofweek_heatmap)

# 7. Location vs DayOfWeek
location_dayofweek_data <- data %>%
  group_by(Charging.Station.Location, Day.of.Week) %>%
  summarise(Count = n())

location_dayofweek_heatmap <- ggplot(location_dayofweek_data, aes(x = Day.of.Week, y = Charging.Station.Location, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FFFFCC", high = "#006837") +
  labs(title = "Location vs Day of Week",
       x = "Day of Week", y = "Location",
       fill = "Number of Charges") +
  theme_minimal()

guardar_graficos("Location_vs_DayOfWeek", location_dayofweek_heatmap)

cat("Todos los análisis bivariantes han sido completados y guardados.\n")