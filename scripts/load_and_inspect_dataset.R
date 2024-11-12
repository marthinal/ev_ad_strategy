# Cargar los datos usando read.csv()
ruta_csv <- "~/ev_ad_strategy/ev_charging_patterns.csv"
datos <- read.csv(ruta_csv, stringsAsFactors = FALSE)

# Inspeccionar datos.
str(datos)