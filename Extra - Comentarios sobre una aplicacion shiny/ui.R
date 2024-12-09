fluidPage(
  # Application title
  # Título de la web
  titlePanel("Word Cloud"),
  
  # Barra lateral con controles: elección del libro y sliders
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      # Selección de libros
      selectInput("selection", "Choose a book:",
                  choices = books), # books : variable global
      
      # Botón que confirma el cambio de libro
      actionButton("update", "Change"),
      hr(),
      
      # Sliders para controlar la frecuencia de aparición de palabras y el 
      # número máximo de palabras en el plot
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),

    # Show Word Cloud
    # Imagen con las palabras
    mainPanel(
      plotOutput("plot")
    )
  )
)
