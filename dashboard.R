# Instalar y cargar librerías necesarias
if (!require("shiny")) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Función para cargar datos
load_data <- function() {
  ruta_csv <- file.path(getwd(), "datasources", "ev_charging_patterns.csv")
  if (!file.exists(ruta_csv)) {
    stop("El archivo CSV no se encuentra en la ruta: ", ruta_csv)
  }
  data <- read.csv(ruta_csv, stringsAsFactors = FALSE)
  data$Charging.End.Time <- as.POSIXct(data$Charging.End.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data$End.Hour <- as.numeric(format(data$Charging.End.Time, "%H"))
  return(data)
}

# UI (Interfaz de usuario)
ui <- fluidPage(
  titlePanel("EV Charging Patterns - Univariate Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Seleccione la visualización"),
      selectInput("plot_type", "Gráfico:",
                  choices = c("User Type" = "user_type",
                              "Day of Week" = "day_of_week",
                              "Charging Station Location" = "location",
                              "End Hour" = "end_hour",
                              "Charger Type" = "charger_type")),
      hr(),
      h5("Información del script:"),
      p("El análisis univariado se basa en el script:"),
      a("Univariate Analysis", href = "https://github.com/marthinal/ev_ad_strategy/blob/main/scripts/univariate_analysis.R", target = "_blank"),
      p("Este script realiza los siguientes análisis:"),
      tags$ul(
        tags$li("Cuenta y agrupa usuarios por tipo (User Type), generando un gráfico de barras."),
        tags$li("Analiza la distribución de cargas según los días de la semana (Day of Week), creando una visualización ordenada de mayor a menor."),
        tags$li("Segmenta las ubicaciones de estaciones de carga (Charging Station Location) y produce un gráfico de barras."),
        tags$li("Examina los patrones de carga por hora del día (End Hour) mediante un histograma."),
        tags$li("Agrupa los tipos de cargadores (Charger Type) y visualiza su distribución con un gráfico de barras.")
      ),
      width = 3
    ),
    mainPanel(
      plotlyOutput("main_plot"),
      width = 9
    )
  )
)

# Servidor
server <- function(input, output, session) {
  data <- reactive({
    load_data()
  })
  
  output$main_plot <- renderPlotly({
    req(input$plot_type)
    
    if (input$plot_type == "user_type") {
      user_colors <- c("Commuter" = "#708090", "Long-Distance Traveler" = "#4682B4", "Casual Driver" = "#A9A9A9")
      user_type_count <- data() %>%
        group_by(User.Type) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      
      user_type_count$User.Type <- factor(user_type_count$User.Type, levels = names(user_colors))
      
      p <- ggplot(user_type_count, aes(x = User.Type, y = Count, fill = User.Type)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = user_colors) +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if (input$plot_type == "day_of_week") {
      day_colors <- c("Sunday" = "#B0C4DE", "Monday" = "#778899", "Tuesday" = "#6A5ACD",
                      "Wednesday" = "#8B4513", "Thursday" = "#CD853F", "Friday" = "#556B2F", "Saturday" = "#8FBC8F")
      
      dayofweek_count <- data() %>%
        group_by(Day.of.Week) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      
      p <- ggplot(dayofweek_count, aes(x = Day.of.Week, y = Count, fill = Day.of.Week)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = day_colors) +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if (input$plot_type == "location") {
      location_colors <- c("Los Angeles" = "#5F9EA0", "San Francisco" = "#4682B4",
                           "Houston" = "#A0522D", "New York" = "#6B8E23", "Chicago" = "#BDB76B")
      
      location_count <- data() %>%
        group_by(Charging.Station.Location) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      
      p <- ggplot(location_count, aes(x = Charging.Station.Location, y = Count, fill = Charging.Station.Location)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = location_colors) +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if (input$plot_type == "end_hour") {
      hourly_end_count <- data() %>%
        group_by(End.Hour) %>%
        summarise(Count = n()) %>%
        arrange(End.Hour)
      
      p <- ggplot(hourly_end_count, aes(x = End.Hour, y = Count)) +
        geom_bar(stat = "identity", fill = "#2F4F4F") +
        scale_x_continuous(breaks = 0:23) +
        theme_minimal()
      
    } else if (input$plot_type == "charger_type") {
      charger_colors <- c("DC Fast Charger" = "#FFD700", "Level 1" = "#8B0000", "Level 2" = "#32CD32")
      charger_type_count <- data() %>%
        group_by(Charger.Type) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      
      p <- ggplot(charger_type_count, aes(x = Charger.Type, y = Count, fill = Charger.Type)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = charger_colors) +
        theme_minimal() +
        theme(legend.position = "none")
    }
    
    ggplotly(p)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)