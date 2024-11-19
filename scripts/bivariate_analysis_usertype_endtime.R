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

# Crear la columna End.Hour
data <- data %>% mutate(End.Hour = as.numeric(format(Charging.End.Time, "%H")))

# Verificar si la columna End.Hour ha sido creada correctamente
print(head(data))

# Análisis Bivariado 1: User Type vs End Time (Boxplot)
boxplot_usertype_endtime <- ggplot(data, aes(x = User.Type, y = End.Hour, fill = User.Type)) +
  geom_boxplot() +
  labs(title = "Boxplot of User Type vs Charging End Time", x = "User Type", y = "End Hour of Charging") +
  theme_minimal()

# Análisis Bivariado 1: User Type vs End Time (Line Plot)
lineplot_usertype_endtime <- data %>%
  group_by(User.Type, End.Hour) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = End.Hour, y = Count, color = User.Type, group = User.Type)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Line Plot of User Type vs Charging End Time", x = "End Hour of Charging", y = "Number of Charges") +
  theme_minimal()

# Crear el contenido HTML con los gráficos actualizados
html_content <- tags$div(
  tags$h1("Bivariate Analysis"),
  tags$h2("User Type vs End Time (Boxplot)"),
  ggplotly(boxplot_usertype_endtime),
  tags$h2("User Type vs End Time (Line Plot)"),
  ggplotly(lineplot_usertype_endtime)
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content, file = "bivariate_analysis_usertype_endtime.html")
cat("The report has been saved as 'bivariate_analysis_usertype_endtime.html'\n")