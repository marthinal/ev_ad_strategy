# Instalar y cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("forcats")) install.packages("forcats", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(forcats)

# Función para cargar los datos
load_data <- function(ruta_csv) {
  data <- read.csv(ruta_csv, stringsAsFactors = FALSE)
  data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data$End.Hour <- as.numeric(format(data$Charging.End.Time, "%H"))
  data$Day.of.Week <- factor(data$Day.of.Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  return(data)
}

# Función para generar el gráfico de User Type
generate_user_type_plot <- function(data) {
  user_colors <- c("Commuter" = "#708090", "Long-Distance Traveler" = "#4682B4", "Casual Driver" = "#A9A9A9")
  
  user_type_count <- data %>%
    group_by(User.Type) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  ggplot(user_type_count, aes(x = User.Type, y = Count, fill = User.Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = user_colors) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Función para generar el gráfico de Day of Week
generate_day_of_week_plot <- function(data) {
  day_colors <- c("Monday" = "#778899", "Tuesday" = "#6A5ACD", "Wednesday" = "#8B4513",
                  "Thursday" = "#CD853F", "Friday" = "#556B2F", "Saturday" = "#8FBC8F", "Sunday" = "#B0C4DE")
  
  dayofweek_count <- data %>%
    group_by(Day.of.Week) %>%
    summarise(Count = n())
  
  ggplot(dayofweek_count, aes(x = Day.of.Week, y = Count, fill = Day.of.Week)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = day_colors) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Función para generar el gráfico de Charging Station Location
generate_charging_station_location_plot <- function(data) {
  location_colors <- scales::hue_pal()(length(unique(data$Charging.Station.Location)))
  
  location_count <- data %>%
    group_by(Charging.Station.Location) %>%
    summarise(Count = n())
  
  ggplot(location_count, aes(x = Charging.Station.Location, y = Count, fill = Charging.Station.Location)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = location_colors) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Función para generar el gráfico de End Hour
generate_end_hour_plot <- function(data) {
  ggplot(data, aes(x = End.Hour)) +
    geom_histogram(binwidth = 1, fill = "#2F4F4F", color = "black") +
    scale_x_continuous(breaks = 0:23) +
    labs(title = "Distribution of Charging End Hours", x = "Hour of the Day", y = "Number of Charges") +
    theme_minimal()
}

# Función para generar el gráfico de Charger Type
generate_charger_type_plot <- function(data) {
  charger_colors <- c("DC Fast Charger" = "#FFD700", "Level 1" = "#8B0000", "Level 2" = "#32CD32")
  
  charger_type_count <- data %>%
    group_by(Charger.Type) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  ggplot(charger_type_count, aes(x = Charger.Type, y = Count, fill = Charger.Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = charger_colors) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Exportar funciones
univariate_analysis <- list(
  load_data = load_data,
  generate_user_type_plot = generate_user_type_plot,
  generate_day_of_week_plot = generate_day_of_week_plot,
  generate_charging_station_location_plot = generate_charging_station_location_plot,
  generate_end_hour_plot = generate_end_hour_plot,
  generate_charger_type_plot = generate_charger_type_plot
)