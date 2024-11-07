# Instalar y cargar las librerías necesarias
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RMariaDB", quietly = TRUE)) install.packages("RMariaDB")

library(DBI)
library(RMariaDB)

# Conectar a la base de datos MySQL
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port = 3306,
                 user = "root",
                 password = "root",
                 dbname = "EV_DS")

# Leer el archivo CSV
ruta_csv <- "/Users/marthinal/ev_ad_strategy/ev_charging_patterns.csv"
data_original <- read.csv(ruta_csv)

# Escribir los datos en la base de datos MySQL
dbWriteTable(con, "charging_data", data_original, overwrite = TRUE, row.names = FALSE)

# Leer los datos de la tabla de MySQL
data_mysql <- dbReadTable(con, "charging_data")

# Comparar los data.frames
comparacion <- all.equal(data_original, data_mysql)

if (isTRUE(comparacion)) {
  cat("Los datos se han insertado correctamente en la tabla de MySQL.\n")
} else {
  cat("Se encontraron diferencias entre los datos originales y los datos en MySQL:\n")
  print(comparacion)
}

# Desconectar de la base de datos
dbDisconnect(con)



