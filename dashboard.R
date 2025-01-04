# Instalar y cargar librerías necesarias
if (!require("shiny")) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages("plotly", repos = "http://cran.us.r-project.org")

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Ruta de los scripts
univariate_script_path <- file.path(getwd(), "scripts", "univariate_analysis.R")
bivariate_script_path <- file.path(getwd(), "scripts", "bivariate_complete.R")
multivariate_script_path <- file.path(getwd(), "scripts", "multivariate_analysis.R")

# Verificar y cargar scripts
if (!file.exists(univariate_script_path)) {
  stop("El script univariado no se encuentra en la ruta: ", univariate_script_path)
}
if (!file.exists(bivariate_script_path)) {
  stop("El script bivariado no se encuentra en la ruta: ", bivariate_script_path)
}
if (!file.exists(multivariate_script_path)) {
  stop("El script multivariado no se encuentra en la ruta: ", multivariate_script_path)
}

source(univariate_script_path, local = TRUE)
source(bivariate_script_path, local = TRUE)
source(multivariate_script_path, local = TRUE)

# Cargar datos
load_data <- univariate_analysis$load_data

# UI (Interfaz de usuario)
ui <- fluidPage(
  titlePanel("EV Charging Patterns - Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Seleccione el tipo de análisis"),
      selectInput("analysis_type", "Tipo de análisis:",
                  choices = c("Univariado" = "univariate",
                              "Bivariado" = "bivariate",
                              "Multivariado" = "multivariate")),
      hr(),
      conditionalPanel(
        condition = "input.analysis_type == 'univariate'",
        h5("Información del script univariado"),
        p("El análisis univariado utiliza el siguiente script:"),
        a("Univariate Analysis Script", 
          href = "https://github.com/marthinal/ev_ad_strategy/blob/main/scripts/univariate_analysis.R", 
          target = "_blank"),
        h5("Seleccione la visualización univariada"),
        selectInput("univariate_plot", "Gráfico:",
                    choices = c("User Type" = "user_type",
                                "Day of Week" = "day_of_week",
                                "Charging Station Location" = "location",
                                "End Hour" = "end_hour",
                                "Charger Type" = "charger_type"))
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'bivariate'",
        h5("Información del script bivariado"),
        p("El análisis bivariado utiliza el siguiente script:"),
        a("Bivariate Complete Script", 
          href = "https://github.com/marthinal/ev_ad_strategy/blob/main/scripts/bivariate_complete.R", 
          target = "_blank"),
        h5("Seleccione la visualización bivariada"),
        selectInput("bivariate_plot", "Gráfico:",
                    choices = c("User Type vs End Time" = "user_type_end_time",
                                "User Type vs Location" = "user_type_location",
                                "Charger Type vs Day of Week" = "charger_type_day_of_week",
                                "End Time vs Day of Week" = "end_time_day_of_week"))
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'multivariate'",
        h5("Información del script multivariado"),
        p("El análisis multivariado utiliza el siguiente script:"),
        a("Multivariate Analysis Script", 
          href = "https://github.com/marthinal/ev_ad_strategy/blob/main/scripts/multivariate_analysis.R", 
          target = "_blank")
      ),
      width = 3
    ),
    mainPanel(
      plotlyOutput("main_plot"),
      hr(),
      h4("Explicación del análisis seleccionado"),
      textOutput("analysis_explanation"),
      width = 9
    )
  )
)

# Servidor
server <- function(input, output, session) {
  data <- reactive({
    load_data(file.path(getwd(), "datasources", "ev_charging_patterns.csv"))
  })
  
  output$main_plot <- renderPlotly({
    req(input$analysis_type)
    plot <- NULL
    
    if (input$analysis_type == "univariate") {
      req(input$univariate_plot)
      plot <- switch(input$univariate_plot,
                     user_type = univariate_analysis$generate_user_type_plot(data()),
                     day_of_week = univariate_analysis$generate_day_of_week_plot(data()),
                     location = univariate_analysis$generate_charging_station_location_plot(data()),
                     end_hour = univariate_analysis$generate_end_hour_plot(data()),
                     charger_type = univariate_analysis$generate_charger_type_plot(data()))
    } else if (input$analysis_type == "bivariate") {
      req(input$bivariate_plot)
      plot <- switch(input$bivariate_plot,
                     user_type_end_time = bivariate_complete$generate_user_type_end_time_plot(data()),
                     user_type_location = bivariate_complete$generate_user_type_location_plot(data()),
                     charger_type_day_of_week = bivariate_complete$generate_charger_type_day_of_week_plot(data()),
                     end_time_day_of_week = bivariate_complete$generate_end_time_day_of_week_plot(data()))
    } else if (input$analysis_type == "multivariate") {
      plot <- multivariate_analysis$generate_multivariate_plot(data())
    }
    
    validate(need(!is.null(plot), "Error: No se pudo generar el gráfico."))
    ggplotly(plot)
  })
  
  output$analysis_explanation <- renderText({
    req(input$analysis_type)
    if (input$analysis_type == "univariate") {
      "El análisis univariado examina una sola variable a la vez, mostrando su distribución y características principales."
    } else if (input$analysis_type == "bivariate") {
      "El análisis bivariado muestra la relación entre dos variables seleccionadas."
    } else if (input$analysis_type == "multivariate") {
      "El análisis multivariado combina múltiples variables para examinar interacciones complejas entre ellas."
    }
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)