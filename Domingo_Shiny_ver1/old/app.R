library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)

library(shiny)
library(shinythemes)



var_texto = "
Esta aplicación shiny tiene el objetivo de demostrar 
los conocimientos adquiridos
en la construcción de un cuadro de mandos básico con R.
"
var_texto_mitfm = "
Mi TFM consiste en ...
"

GDatasets = c("advertising.csv","datos_salarios.csv","datos_ordenadores.csv")
datos <- read.csv("advertising.csv")
names(datos) = c("Id","TVPubl","RadioPubl","PeriodicosPubl","Ventas")
var_selec = c(2:5)

GLugares <- c("Sevilla","Córdoba","Londres","Edimburgo", "Vaticano")

GLatLong <- data.frame(
  Lat = c(37.35945,37.886129,51.500818,55.95710112,41.90234465),
  Long = c(-5.98814,-4.867665,-0.124510,-3.187314305,12.4568083)
)


# fluidPage  -----
ui <- fluidPage(
  # --- Selección de temas ---
  
  # Caja de selección de los temas disponibles
  # shinythemes::themeSelector(),
  
  # (??)
  # includeCSS("www/estilos.css"),
  
  # Tema por defecto
  # theme=shinythemes::shinytheme(theme = "slate"),
  theme=shinythemes::shinytheme(theme = "journal"),
  
  # --- Panel del título ---
  titlePanel(
    strong("Inteligencia de Negocio con R: DSBI", style="color:blue;"),
    windowTitle = "titulo_DSBI"
  ),
  
  
  # --- Panel lateral: navegación ---
  navlistPanel(
    # El total de la página es 12, así que repartimos el panel lateral como 3 y 
    # el central (el principal) como 9
    widths=c(2,9),
    
    # --- tabPanel: Información  ---
    tabPanel("Información",
             # Icono "i"
             icon = icon("info"),  
             
             # --- Estructura de la página de información ---
             # Imágenes
             div(class="center",
                 img(src="portadashiny.png",height="200px"),
                 img(src="hex_shiny.jpg",height="200px")),
             
             # Separación
             br(),br(),br(), # saltos de línea
             hr(), # línea horizontal
             
             
             
             # Texto
             h3("Objetivo:",class="estiloh3"),
             var_texto,
             
             h3("Autor:",class="estiloh3"),
             strong("Página original de Pedro Luis Luque"),
             br(),
             strong("Editado por Domingo Parrales de la Cruz"),
             
             h3("Resumen de mi Trabajo Fin de Máster:", class="estiloh3"),
             var_texto_mitfm
    ),
    
    
    # --- tabPanel: Datos  ---   
    tabPanel("Datos", icon = icon("database"),
             # NOTA: width de cada fila suma hasta 12
             
             # --- FluidRow: cosas sobre la tabla ---
             # Fila 1
             fluidRow(
               # Entrada de ficheros
               column(width=4,
                      shiny::fileInput(inputId="file1", 
                                       label="Elige un fihero csv para subir",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))
               ),
               
               # ¿La primera fila de los datos son nombres? CheckBox
               column(width=2, offset=2,
                      shiny::checkboxInput(inputId="tabla_headers", 
                                           label = "Tiene nombres Columnas", 
                                           value = TRUE)
               ),
               
               # ¿Qué tipo de separación hay entre los datos? radioButtons
               column(width=4,
                      shiny::radioButtons(inputId="tabla_separador", 
                                          label = "Separador",
                                          choices = list(Comma = ",", 
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                          selected = ",",
                                          inline=TRUE)
               )
             ),
             
             # Fila 2
             fluidRow(
               # Entrada de ficheros
               column(width=12,
                      shiny::selectInput(inputId="tabla_seleccion", 
                                         label = "Selecciona un dataset",
                                         choices = list(
                                           advertising.csv = "advertising.csv",
                                           datos_ordenadores.csv = "datos_ordenadores.csv", 
                                           datos_salarios.csv = "datos_salarios.csv"),
                                         selected = "advertising.csv")
               )
             ),
             
             # Fila 3
             fluidRow(
               # Tabla
               DT::dataTableOutput("tabla_contents"))
             
             
             
             
    ),
    
    
    # --- tabPanel: Estudio descriptivo ---
    tabPanel("Estudio Descriptivo",icon = icon("chart-bar"),
             
             # fluidRow: selecciona variable
             fluidRow(
               column(width=4,
                      shiny::selectInput("Selvariable01Uni","Selecciona variable",
                                         choices = names(datos)[c(var_selec)],
                                         selected = names(datos)[var_selec[1]]),
                      
               )
             ),
             
             # tabsetPanel: Unidimensional-----
             tabsetPanel(
               
               # --- tabPanel: resumen numérico ---
               tabPanel("Resumen Numérico",
                        # (??)
                        # Render a reactive output variable as text within an application page
                        shiny::verbatimTextOutput("OUT_resumen_numerico") # verbatimText: OUTResNum
               ),
               
               # --- tabPanel: gráficos unidimensionales ---
               tabPanel("Gráficos unidimensionales",
                        shiny::verbatimTextOutput("OUT_graficos_unidimensionales")
               ),
               
               # --- tabPanel: regresión lineal ---
               tabPanel("Regresión lineal",
                        shiny::verbatimTextOutput("OUT_regresion_lineal")
               )
               
             ) 
    ),
    
    # --- tabPanel: Mapas  ---   
    tabPanel("Mapas",icon = icon("globe")),
  )
)


server <- function(input, output, session) {
  
  # --- Datos ---
  # Cuando se sube un nuevo archivo, actualiza la lista de datasets
  datasets <- reactiveVal({
    list(
      advertising.csv = "advertising.csv",
      datos_ordenadores.csv = "datos_ordenadores.csv", 
      datos_salarios.csv = "datos_salarios.csv")
    })
  
  observeEvent(input$file1, {
    # Obtén el nombre y la ruta del archivo subido
    file_name <- input$file1$name
    file_path <- input$file1$datapath
    
    # Agrega el nuevo archivo a la lista de datasets
    datasets_list <- datasets()
    datasets_list[[file_name]] <- file_path
    datasets(datasets_list)
    
    # Actualiza el inputSelection y selecciona el nuevo dataset
    updateSelectInput(session, "tabla_seleccion", 
                      choices = datasets(),
                      selected = datasets_list[[length(datasets_list)]])
    
  })
  
  # Creación de la tabla
  output$tabla_contents <- renderDT({
    
    # --- Tabla a partir de los datos subidos ---
    df <- read.csv(input$tabla_seleccion,
                   header = input$tabla_headers,
                   sep = input$tabla_separador)
    
    
    datatable(df,
              filter = "bottom",
              extensions = 'Buttons',
              options = list(
                dom = 'lBfrtip',
                buttons = list('copy', 'print', 
                               list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel', 'pdf'),
                                 text = 'Download'
                               )),
                lengthMenu = c(15, 30, 50, 200),
                pageLength = 15
                # language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              ))
    
    
  })
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
