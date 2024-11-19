# Instalar y cargar librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require("htmltools")) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if (!require("vcd")) install.packages("vcd", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(plotly)
library(htmltools)
library(vcd)

# Leer el archivo CSV
ruta_csv <- "~/ev_ad_strategy/ev_charging_patterns.csv"
data <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Verificar los primeros datos
print(head(data))

# Análisis Bivariado: User Type vs Location
# Gráfico de Barras
bar_chart_usertype_location <- data %>%
  group_by(User.Type, Charging.Station.Location) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Charging.Station.Location, y = Count, fill = User.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Chart of User Type vs Location",
       x = "Charging Station Location",
       y = "Number of Charges",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de Mosaico
mosaic_usertype_location <- structable(~ User.Type + Charging.Station.Location, data = data)
mosaicplot_usertype_location <- mosaic(mosaic_usertype_location, shade = TRUE, legend = TRUE,
                                       main = "Mosaic Plot of User Type vs Location",
                                       xlab = "Location", ylab = "User Type")

# Crear el contenido HTML con los gráficos
html_content <- tags$div(
  tags$h1("Bivariate Analysis - User Type vs Location"),
  tags$h2("Bar Chart of User Type vs Location"),
  ggplotly(bar_chart_usertype_location),
  tags$h2("Mosaic Plot of User Type vs Location"),
  "The mosaic plot cannot be displayed interactively, please refer to the saved plot.",
  tags$img(src = "mosaicplot_usertype_location.png", alt = "Mosaic Plot of User Type vs Location")
)

# Guardar el reporte en un archivo HTML
htmltools::save_html(html_content, file = "bivariate_analysis_usertype_location.html")
cat("The report has been saved as 'bivariate_analysis_usertype_location.html'\n")

# Guardar el Mosaic Plot como una imagen PNG
png(filename = "mosaicplot_usertype_location.png", width = 800, height = 600)
mosaic(mosaic_usertype_location, shade = TRUE, legend = TRUE,
       main = "Mosaic Plot of User Type vs Location",
       xlab = "Location", ylab = "User Type")
dev.off()