# Cargar las bibliotecas necesarias
library(shiny)
library(shinycssloaders)

# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel("Ejemplo Avanzado de shinycssloaders"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", 
                  "Número de observaciones:", 
                  min = 1, 
                  max = 1000, 
                  value = 500)
    ),
    mainPanel(
      # Agregar el spinner personalizado al output del gráfico <-> pantalla de carga
      withSpinner(plotOutput("distPlot"), type = 1, # del 1 al 10 (tipo de animación) 
                  color = "#00BFFF", size = 3)
    )
  )
)

# --- server ---
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Simular un retardo para mostrar el spinner
    Sys.sleep(2)
    
    # Generar el gráfico
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
