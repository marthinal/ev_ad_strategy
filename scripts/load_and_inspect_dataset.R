# Cargar los datos usando read.csv()
ruta_csv <- "~/ev_ad_strategy/ev_charging_patterns.csv"
datos <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Instalar los paquetes necesarios si no están instalados
install.packages("skimr")
install.packages("knitr")
install.packages("kableExtra")

# Cargar las librerías
library(skimr)
library(knitr)
library(kableExtra)
library(dplyr)

# Generar el resumen con skimr
resumen_skim <- skim(datos)

# Convertir el resumen a un data frame
df_resumen <- as.data.frame(resumen_skim)

# Crear la tabla completa en formato Markdown
tabla_md <- kable(df_resumen, format = "markdown", align = "l", caption = "Resumen Completo de Datos con skimr") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  scroll_box(width = "100%", height = "500px")  # Añadir scroll para manejar tablas grandes

# Exportar la tabla a un archivo HTML para mejor visualización
save_kable(tabla_md, file = "/Users/marthinal/ev_ad_strategy/reports/skim_summary.html")

