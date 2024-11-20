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

# Paleta de colores
colors_charger_user <- c("Level 1" = "#D73027", "Level 2" = "#1A9850", "DC Fast Charger" = "#FFD700")
user_colors <- c("Commuter" = "#4B6F91", "Long-Distance Traveler" = "#E69F00", "Casual Driver" = "#56B4E9")

# Análisis bivariado: ChargerType vs UserType (Bar Plot)
bar_data <- data %>%
  group_by(Charger.Type, User.Type) %>%
  summarise(Count = n())

bar_plot <- ggplot(bar_data, aes(x = Charger.Type, y = Count, fill = User.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = user_colors) +
  labs(title = "Bivariate Analysis - Charger Type vs User Type",
       x = "Charger Type", y = "Number of Charges",
       fill = "User Type") +
  theme_minimal()

# Convertir el gráfico a objeto interactivo
interactive_bar_plot <- ggplotly(bar_plot)

# Crear el contenido HTML con el gráfico
html_content_user <- tags$div(
  tags$h1("Bivariate Analysis - Charger Type vs User Type"),
  tags$h2("Bar Plot of Charger Type vs User Type"),
  interactive_bar_plot
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content_user, file = "bivariate_charger_usertype.html")
cat("The report has been saved as 'bivariate_charger_usertype_analysis.html'\n")