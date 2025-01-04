# Cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)

# Función para cargar datos
load_data <- function(ruta_csv) {
  data <- read.csv(ruta_csv, stringsAsFactors = FALSE)
  data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data$End.Hour <- as.numeric(format(data$Charging.End.Time, "%H"))
  return(data)
}

# Función: User Type vs End Time
generate_user_type_end_time_plot <- function(data) {
  line_data <- data %>%
    group_by(User.Type, End.Hour) %>%
    summarise(Count = n())
  
  plot <- ggplot(line_data, aes(x = End.Hour, y = Count, color = User.Type, group = User.Type)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "User Type vs End Time",
         x = "End Hour", y = "Number of Charges") +
    theme_minimal()
  
  return(plot)
}

# Función: User Type vs Location
generate_user_type_location_plot <- function(data) {
  location_data <- data %>%
    group_by(User.Type, Charging.Station.Location) %>%
    summarise(Count = n())
  
  plot <- ggplot(location_data, aes(x = Charging.Station.Location, y = Count, fill = User.Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "User Type vs Location",
         x = "Location", y = "Number of Charges",
         fill = "User Type") +
    theme_minimal()
  
  return(plot)
}

# Función: Charger Type vs Day of Week
generate_charger_type_day_of_week_plot <- function(data) {
  charger_dayofweek_data <- data %>%
    group_by(Charger.Type, Day.of.Week) %>%
    summarise(Count = n())
  
  plot <- ggplot(charger_dayofweek_data, aes(x = Day.of.Week, y = Charger.Type, fill = Count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#FFFFCC", high = "#006837") +
    labs(title = "Charger Type vs Day of Week",
         x = "Day of Week", y = "Charger Type",
         fill = "Number of Charges") +
    theme_minimal()
  
  return(plot)
}

# Función: End Time vs Day of Week
generate_end_time_day_of_week_plot <- function(data) {
  endtime_dayofweek_data <- data %>%
    group_by(End.Hour, Day.of.Week) %>%
    summarise(Count = n())
  
  plot <- ggplot(endtime_dayofweek_data, aes(x = Day.of.Week, y = End.Hour, fill = Count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#FFFFCC", high = "#006837") +
    labs(title = "End Time vs Day of Week",
         x = "Day of Week", y = "End Hour",
         fill = "Number of Charges") +
    theme_minimal()
  
  return(plot)
}

# Exportar funciones en un objeto
bivariate_complete <- list(
  load_data = load_data,
  generate_user_type_end_time_plot = generate_user_type_end_time_plot,
  generate_user_type_location_plot = generate_user_type_location_plot,
  generate_charger_type_day_of_week_plot = generate_charger_type_day_of_week_plot,
  generate_end_time_day_of_week_plot = generate_end_time_day_of_week_plot
)