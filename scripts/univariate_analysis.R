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

# Nueva paleta de colores
colors <- c("Commuter" = "#4B6F91", "Long-Distance Traveler" = "#E69F00", "Casual Driver" = "#56B4E9")
day_colors <- c("Sunday" = "#D55E00", "Monday" = "#F0E442", "Tuesday" = "#009E73", 
                "Wednesday" = "#0072B2", "Thursday" = "#CC79A7", "Friday" = "#F0A202", "Saturday" = "#87CEEB")
location_colors <- c("Los Angeles" = "#66C2A5", "San Francisco" = "#FC8D62", 
                     "Houston" = "#8DA0CB", "New York" = "#E78AC3", "Chicago" = "#A6D854")
end_hour_color <- "#4682B4"  # SteelBlue, un color más adecuado para variables de tiempo

# Análisis de la variable User.Type
user_type_count <- data %>%
  group_by(User.Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Gráfico de pastel para User.Type
pie_chart_usertype <- plot_ly(
  data = user_type_count,
  labels = ~User.Type,
  values = ~Count,
  type = 'pie',
  marker = list(colors = colors)
) %>%
  layout(title = "Overall Distribution of User Type")

# Gráfico de barras para User.Type
bar_chart_usertype <- ggplot(user_type_count, aes(x = reorder(User.Type, -Count), y = Count, fill = User.Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "User Type Distribution (Sorted by Count)", x = "User Type", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# Análisis de la variable Day.of.Week
dayofweek_count <- data %>%
  group_by(Day.of.Week) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Gráfico de pastel para Day.of.Week
pie_chart_dayofweek <- plot_ly(
  data = dayofweek_count,
  labels = ~Day.of.Week,
  values = ~Count,
  type = 'pie',
  marker = list(colors = day_colors)
) %>%
  layout(title = "Overall Distribution of Day of Week")

# Gráfico de barras para Day.of.Week
bar_chart_dayofweek <- ggplot(dayofweek_count, aes(x = reorder(Day.of.Week, -Count), y = Count, fill = Day.of.Week)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = day_colors) +
  labs(title = "Day of Week Distribution (Sorted by Count)", x = "Day of Week", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# Análisis de la variable Charging.Station.Location
location_count <- data %>%
  group_by(Charging.Station.Location) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Gráfico de pastel para Charging.Station.Location
pie_chart_location <- plot_ly(
  data = location_count,
  labels = ~Charging.Station.Location,
  values = ~Count,
  type = 'pie',
  marker = list(colors = location_colors)
) %>%
  layout(title = "Overall Distribution of Charging Station Location")

# Gráfico de barras para Charging.Station.Location
bar_chart_location <- ggplot(location_count, aes(x = reorder(Charging.Station.Location, -Count), y = Count, fill = Charging.Station.Location)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = location_colors) +
  labs(title = "Charging Station Location Distribution (Sorted by Count)", x = "Charging Station Location", y = "Frequency") +
  theme_minimal() +
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
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Charging End Time Distribution by Hour", x = "Hour of Day", y = "Frequency") +
  theme_minimal()

# Gráfico de boxplot para End.Hour
boxplot_end_hour <- ggplot(data, aes(x = "", y = End.Hour)) +
  geom_boxplot(fill = end_hour_color) +
  labs(title = "Boxplot of Charging End Time", y = "End Hour of Charging") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Crear el contenido HTML con todos los gráficos
html_content <- tags$div(
  tags$h1("Exploratory Data Analysis - User Type, Day of Week, Charging Station Location, and Charging End Time"),
  tags$h2("User Type Analysis"),
  pie_chart_usertype,
  ggplotly(bar_chart_usertype),
  tags$h2("Day of Week Analysis"),
  pie_chart_dayofweek,
  ggplotly(bar_chart_dayofweek),
  tags$h2("Charging Station Location Analysis"),
  pie_chart_location,
  ggplotly(bar_chart_location),
  tags$h2("Charging End Time Analysis"),
  ggplotly(end_hour_histogram),
  tags$h2("Boxplot of Charging End Time"),
  ggplotly(boxplot_end_hour)
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content, file = "eda_final_analysis_with_boxplot.html")
cat("The report has been saved as 'eda_final_analysis_with_boxplot.html'\n")