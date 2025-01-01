# Instalar y cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require("htmltools")) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if (!require("forcats")) install.packages("forcats", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(plotly)
library(forcats)

# Función para cargar los datos
load_data <- function(ruta_csv) {
  data <- read.csv(ruta_csv, stringsAsFactors = FALSE)
  data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data$End.Hour <- as.numeric(format(data$Charging.End.Time, "%H"))
  return(data)
}

# Función para generar el gráfico de User Type
generate_user_type_plot <- function(data) {
  user_colors <- c("Commuter" = "#708090", "Long-Distance Traveler" = "#4682B4", "Casual Driver" = "#A9A9A9")
  
  user_type_count <- data %>%
    group_by(User.Type) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  user_type_count$User.Type <- factor(user_type_count$User.Type, levels = names(user_colors))
  
  ggplot(user_type_count, aes(x = User.Type, y = Count, fill = User.Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = user_colors) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Función para generar el gráfico de Day of Week
generate_day_of_week_plot <- function(data) {
  day_colors <- c("Sunday" = "#B0C4DE", "Monday" = "#778899", "Tuesday" = "#6A5ACD",
                  "Wednesday" = "#8B4513", "Thursday" = "#CD853F", "Friday" = "#556B2F", "Saturday" = "#8FBC8F")
  
  dayofweek_count <- data %>%
    group_by(Day.of.Week) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  ggplot(dayofweek_count, aes(x = Day.of.Week, y = Count, fill = Day.of.Week)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = day_colors) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Función para generar otros gráficos
# (Crea funciones similares para los gráficos de Location, End Hour, etc.)

# Exportar funciones
univariate_analysis <- list(
  load_data = load_data,
  generate_user_type_plot = generate_user_type_plot,
  generate_day_of_week_plot = generate_day_of_week_plot
)