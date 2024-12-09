library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)

library(shiny)
library(shinythemes)
# install.packages("shinyjs")
library(shinyjs) # <- cambiar temas de forma dinámica con javascript

var_texto = "
Esta aplicación shiny tiene el objetivo de demostrar 
los conocimientos adquiridos
en la construcción de un cuadro de mandos básico con R.
"
var_texto_mitfm = "
Mi TFM consiste en estimar el precio de viviendas en Inglaterra. Se tienen 
varios factores en cuenta, como la antigüedad, si fue embargada por el banco,
si es de alquiler, etc.
"

datos <- read.csv("advertising.csv")

GLugares <- c("Sevilla","Córdoba","Londres","Edimburgo", "Vaticano")




# fluidPage  -----
ui <- fluidPage(
  # --- Selección de temas ---
  
  # Caja de selección de los temas disponibles
  # shinythemes::themeSelector(),
  
  # (??)
  # includeCSS("www/estilos.css"),
  
  # Tema por defecto + tag para el cambio dinámico con javascript
  # useShinyjs(),
  # tags$head(
  #   tags$script(src = "switch-theme.js")
  # ),
  theme=shinythemes::shinytheme(theme = "darkly"),
  
  # theme = shinythemes::shinytheme(theme = "united"),
  
  
  # --- Panel del título ---
  titlePanel(
    strong("Inteligencia de Negocio con R: DSBI", style=".strokeme
{
    color: skyblue;
    text-shadow:
    -1px -1px 0 #000,
    1px -1px 0 #000,
    -1px 1px 0 #000,
    1px 1px 0 #000;  
};"),
    windowTitle = "titulo_DSBI"
  ),

  # Botón para cambiar el tema (entre tema oscuro y claro)
  # shiny::checkboxInput(inputId='tema_oscuro',
  #                      label='Tema oscuro',
  #                      value=TRUE),

  shiny::checkboxInput(inputId='themeToggle',
                      lab = tagList(
                        tags$span(class = "visually-hidden", "Cambiar tema"),
                        tags$span(class = "fa fa-sun", `aria-hidden` = "true"))),
  
  # Texto en h3
  tags$head(
    tags$style(HTML("
                    h3 {
                    color: #268bd2;
                    }
                    ")
               )
    ),
  
  # NavPanel
  tags$head(tags$style(HTML(".nav.nav-pills.nav-stacked > .active > a, .nav.nav-pills.nav-stacked > .active > a:hover {
    background-color: #2f7057 ;
    }

    .well {
        font-family: 'Rock Salt', cursive;
    }

                              "))),
  
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
             div(style="text-align: center;",
                 img(src="portadashiny.png",height="200px"),
                 img(src="hex_shiny.jpg",height="200px")),
             
             # Separación
             br(),br(),br(), # saltos de línea
             hr(), # línea horizontal
             
             
             
             # Texto
             div(h3("Objetivo:",class="estiloh3")),
             var_texto,
             
             h3("Autor:",class="estiloh3"),
             strong("Domingo Parrales de la Cruz."),
             br(),
             "Página de referencia de Pedro Luis Luque.",
             
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
               # Selección del dataset
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
               tags$head(
                 tags$style(HTML(".cell-border-right{border-right: 3px solid #FFFFFF}"))),
               shiny::column(width=12,
                             DT::dataTableOutput("tabla_contents"))
             )
               
             
             
             
             
    ),
    
    
    # --- tabPanel: Estudio descriptivo ---
    tabPanel("Estudio Descriptivo",icon = icon("chart-bar"),
             
             # fluidRow: selecciona variable
             fluidRow(
               column(width=4,
                      shiny::selectInput("Selvariable01Uni","Selecciona variable",
                                         choices = names(datos),
                                         selected = names(datos)[2]),
                      
               )
             ),
             
             # --- tabsetPanel: las 3 opciones disponibles ---
             tabsetPanel(
               
               # --- tabPanel: resumen numérico ---
               tabPanel("Resumen Numérico",
                        
                        # Render a reactive output variable as text within an application page
                        shiny::verbatimTextOutput("OUT_resumen_numerico")
               ),
               
               # --- tabPanel: gráficos unidimensionales ---
               tabPanel("Gráficos unidimensionales",
                        
                        # Control bins
                        sliderInput("bins", "Number of bins:", 
                                    min = 1, max = 50, value = 30),
                        
                        # Plot (de plot_ly)
                        
                        div(style = "height: 600px;",  # Adjust the height as needed
                          plotlyOutput("unidimensional_plot")
                        )
               ),
               
               # --- tabPanel: regresión lineal ---
               tabPanel("Regresión lineal",
                        
                        # Variable Y
                        fluidRow(
                          shiny::column(width=6,
                                        selectInput(inputId='regresion_Y',
                                                    lab='Selecciona variable dependiente (Y)',
                                                    choices=names(datos),
                                                    selected=names(datos)[2])),
                          
                          # Variable X
                          shiny::column(width=6,
                                        selectInput(inputId='regresion_X',
                                                    lab='Selecciona variable independiente (X)',
                                                    choices=names(datos),
                                                    selected=names(datos)[2]))
                        ),
                        
                        # Hacer regresión lineal (botón)
                        fluidRow(
                          shiny::column(width=12,
                                        shiny::actionButton(inputId='regresion_boton',
                                                            lab='Calcular regresión',
                                                            icon=icon('calculator')))
                        ),
                        
                        # Resultado: regresión lineal (summary) y scatter plot
                        fluidRow(
                          shiny::column(width = 6,
                                        shiny::verbatimTextOutput('regresion_summary')),
                          shiny::column(width = 6,
                                        shiny::plotOutput('regresion_plot'))
                        )
                        
                        
               ),
               
               tabPanel('Correlaciones', 
                        
                        fluidRow(
                          shiny::column(width=12,
                                        shiny::plotOutput('correlacion_plot'))
                        )
               ),
               
               tabPanel('Regresión múltiple',
                        
                        # Variable Y
                        fluidRow(
                          shiny::column(width=6,
                                        selectInput(inputId='regresion_MULTIPLE_Y',
                                                    lab='Selecciona variable dependiente (Y)',
                                                    choices=names(datos),
                                                    selected=names(datos)[2])),
                          
                          # Variable X
                          shiny::column(width=6,
                                        selectInput(inputId='regresion_MULTIPLE_X',
                                                    lab='Selecciona variable independiente (X)',
                                                    choices=names(datos),
                                                    selected=names(datos)[3]),
                                        
                                        # Botones para añadir contenido a la regresión multiple
                                        shiny::actionButton(inputId='boton_anadir',
                                                            lab=' Añadir a regresión',
                                                            icon=icon('plus')),
                                        shiny::actionButton(inputId='boton_resetear',
                                                            lab=' Resetear',
                                                            icon=icon('toilet-paper')),
                                        shiny::actionButton(inputId='boton_todo',
                                                            lab=' Añadir todo',
                                                            icon=icon('list')))
                        ),
                        
                        # Hacer regresión lineal (botón)
                        fluidRow(
                          shiny::column(width=12,
                                        shiny::actionButton(inputId='boton_regresion_MULTIPLE',
                                                            lab='Calcular regresión',
                                                            icon=icon('calculator')))
                        ),
                        
                        # Mostrar ecuación
                        fluidRow(
                          shiny::column(width=12, align='center',
                                        shiny::textOutput('formula_regresion_MULTIPLE'))
                        ),
                        
                        # Resultado: regresión lineal (summary) y scatter plot
                        fluidRow(
                          shiny::column(width = 6,
                                        shiny::verbatimTextOutput('regresion_MULTIPLE_summary')),
                          shiny::column(width = 6,
                                        shiny::plotOutput('regresion_MULTIPLE_plot'))
                        )
                        
                        )
                        
               
             ) 
    ),
    
    # --- tabPanel: Mapas  ---   
    tabPanel("Mapas",icon = icon("globe"),
             fluidRow(
               
               # Selección del sitio
               shiny::column(offset=1, width=4,
                             shiny::selectInput(inputId = 'mapa_seleccion',
                                                lab = strong('Selecciona lugar de visita'),
                                                choices=GLugares)),
               
               # Enlaces
               shiny::column(offset=1, width=5,
                             HTML('
                                  <ul>
                                  <li><a href="https://www.bufa.es/google-maps-latitud-longitud/" target="_blank">Cómo obtener latitud-longitud</a></li>
                                  <li><a href="https://www.youtube.com/watch?v=AInC_XTANrQ&ab_channel=LuisCorona" target="_blank">¿Cómo obtener latitud-longitud? (YouTube)</a></li>
                                  </ul>
                                  ')
                             )
               ),
             
             # Mapa
             leafletOutput('map')
             
             )
  ),
tags$script(
  "
        //Referencia: https://stackoverflow.com/questions/61632272/r-shiny-light-dark-mode-switch
  
        // define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: white!important;} .paginate_button { background: white!important;} thead { color: white;}'
        const extraDarkThemeElement = document.createElement('style');
        extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
        head.appendChild(extraDarkThemeElement);


        // define event - checked === 'light'
        toggle.addEventListener('input', function(event) {
            // if checked, switch to light theme
            if (toggle.checked) {
                removeLink(themes.dark);
                head.removeChild(extraDarkThemeElement);
                head.appendChild(lightTheme);
            }  else {
                // else add darktheme
                removeLink(themes.light);
                head.appendChild(extraDarkThemeElement)
                head.appendChild(darkTheme);
            }
        })
        "
)
)
