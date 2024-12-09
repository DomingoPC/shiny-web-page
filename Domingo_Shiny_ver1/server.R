library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs) # <- cambiar temas de forma dinámica con javascript


server <- function(input, output, session) {
  
  # --- Cambiar de temas dinámicamente ---
  current_theme <- reactiveValues()
  current_theme$theme <- 'darkly'
  
  observeEvent(input$tema_oscuro, {
    print(current_theme)
    
    if(current_theme$theme == 'darkly') {
      current_theme$theme <- 'united'
    } else {
      current_theme$theme <- 'darkly'
    }
    
    session$sendCustomMessage(type = "switchTheme", message = list(theme = current_theme$theme,
                                                                   color = current_theme$color))
  })
  
  # --- Current data: lista de variables reactivas con info de los datos ---
  current_data <- reactiveValues()
  
  regresion_multiple <- reactiveValues()
  
  # Lista de posibles data sets disponibles
  current_data$datasets <- list(
    advertising.csv = "advertising.csv",
    datos_ordenadores.csv = "datos_ordenadores.csv", 
    datos_salarios.csv = "datos_salarios.csv")
  
  # Manera de leer los data sets:
  current_data$read_options <- list(
    advertising.csv = list(sep = ',', header = TRUE),
    datos_ordenadores.csv = list(sep = ',', header = TRUE),
    datos_salarios.csv = list(sep = ';', header = TRUE)
  )
  
  
  # --- Reacción a cargar un archivo ---
  observeEvent(input$file1, {
    req(input$file1)
    
    # ---Actualización de la lista de data sets ---
    
    # Obtén el nombre y la ruta del archivo subido
    file_name <- input$file1$name
    file_path <- input$file1$datapath
    
    # Agrega el nuevo archivo a la lista de datasets
    datasets_list <- current_data$datasets
    datasets_list[[file_name]] <- file_path
    current_data$datasets <- datasets_list
    
    # --- Añade las opciones de carga especificadas a la lista ---
    # read_options_list <- current_data$read_options
    current_data$read_options[[file_path]] <- list(sep = input$tabla_separador,
                                                   header = input$tabla_headers)
    
    # --- Actualiza el inputSelection y selecciona el nuevo data set ---
    updateSelectInput(session, "tabla_seleccion", 
                      choices = current_data$datasets,
                      selected = current_data$datasets[[length(current_data$datasets)]])
  })
  
  # --- Reacción a seleccionar dataset ---
  observeEvent(input$tabla_seleccion, {
    
    # Carga de datos
    
    current_data$df <- read.csv(input$tabla_seleccion,
                                sep = current_data$read_options[[input$tabla_seleccion]][['sep']]
                                )
    # ,
    #                             header = current_data$read_options[[input$tabla_seleccion]][['header']])
    
    # Mostrar la tabla 
    output$tabla_contents <- renderDT({
      
      datatable(current_data$df,
                style = 'bootstrap',
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
                  columnDefs=list(list(className="dt-right cell-border-right", 
                                       targets='_all')), # barritas verticales
                  initComplete = JS( # Cambiar cabecera
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#404040', 'color': '#ffffff'});",
                    "}"),
                  lengthMenu = c(15, 30, 50, 200), # número de filas (Show entries)
                  pageLength = 15 # número de filas inicial
                  )
                )
      })
    
    
    # --- Actualizar opciones de variables disponibles ---
    
    # Guarda variables disponibles
    current_data$variables <- names(current_data$df)
    regresion_multiple$x_disponibles <- current_data$variables
    
    # Seleccionar primera variable por defecto para estudio unidimensional
    current_data$uni_var <- current_data$variables[2]
    
    # Estudio descriptivo: actualizar variables
    updateSelectInput(session, "Selvariable01Uni", 
                      choices = current_data$variables[-1],
                      selected = current_data$uni_var)
    
    # Regresión lineal: selecciona variables X e Y
    updateSelectInput(session, "regresion_X", 
                      choices = current_data$variables[-1],
                      selected = current_data$variables[3]) # le ponemos otra var.
    
    updateSelectInput(session, "regresion_Y", 
                      choices = current_data$variables[-1],
                      selected = current_data$uni_var)
    
    # Regresión múltiple: eslecciona variables X e Y
    regresion_multiple$y <- regresion_multiple$x_disponibles[2]
    
    updateSelectInput(session, "regresion_MULTIPLE_Y", 
                      choices = current_data$variables,
                      selected = regresion_multiple$y)
    
    updateSelectInput(session, "regresion_MULTIPLE_X", 
                      choices = regresion_multiple$x_disponibles,
                      selected = regresion_multiple$x_disponibles[1]) 
    
    
    
    
    # --- EXTRA: Correlaciones ---
    library(corrplot)
    numericas <- sapply(current_data$df, is.numeric)
    output$correlacion_plot <- renderPlot({
      corrplot(round(cor(current_data$df[, numericas]), 2),
               method='shade', type='full')
    })
    
    
    })
  
  # --- Reacción a cambiar variable del estudio unidimensional ---
  
  observeEvent(input$Selvariable01Uni, {
    
    # --- Almacenamos nombre de la variable seleccionada ---
    current_data$uni_var <- input$Selvariable01Uni
    idx_X <- which(current_data$variables %in% current_data$uni_var)
    x <- current_data$df[, idx_X]
    
    # --- Preparamos el plot de salida ---
    output$unidimensional_plot <- renderPlotly({
      
      # Usamos plot_ly para hacer el histograma de salida
      plot_ly(x = ~x, type = "histogram", nbinsx = input$bins) %>%
        layout(
          title = list(text=paste('<b>Histograma de', 
                                  current_data$uni_var, 
                                  '</b>'),
                       xanchor='left',
                       x=0),
          
          xaxis = list(title = current_data$uni_var,
                       zeroline = FALSE,
                       showgrid = TRUE,
                       gridcolor = "white"),
          
          yaxis = list(title='count',
                       zeroline = FALSE,
                       showgrid = TRUE,
                       gridcolor = "white"),
          
          # plot_bgcolor = "#e5ecf6",
          plot_bgcolor = "#EBEBEB",
          margin = list(l = 50, r = 50, b = 50, t = 50),
          font = list(family = "Arial", size = 12, color = "#000")
        )
    })
  })
  
  
  # --- Regresión lineal: respuesta al botón de hacer la regresión ---
  observeEvent(input$regresion_boton, {
    
    # Seleccionamos las variables
    idx_X = which(current_data$variables == input$regresion_X)
    idx_Y = which(current_data$variables == input$regresion_Y)
    
    x = current_data$df[, idx_X]
    y = current_data$df[, idx_Y]
    
    xlab=current_data$variables[idx_X]
    ylab=current_data$variables[idx_Y]
    
    # --- Plot ---
    output$regresion_plot <- renderPlot({
      # plot(x=x, y=y,
      #      xlab=xlab, ylab=ylab,
      #      pch=19,
      #      main='Diagrama de dispersión')
      
      ggplot(current_data$df, aes(x=x, y=y)) + geom_point() + 
        ggtitle('Diagrama de dispersión') +
        xlab(xlab) + 
        ylab(ylab)
    })
    
    # --- Summary de la regresión lineal: texto ---
    model.regresion_lineal <- lm(y ~ x)
    output$regresion_summary <- renderPrint({
      cat('Regresión lineal. Variable Y =', xlab, 
          ', Variable X =', ylab, 
          '\n\n')
      summary(model.regresion_lineal)
    })
  })
  
  
  # --- Regresión múltiple ---
  # Añadir variable: actualización de valores disponibles
  regresion_multiple$x_usadas <- c()
  
  # Actualizar variable Y
  observeEvent(input$regresion_MULTIPLE_Y, {
    regresion_multiple$y <- input$regresion_MULTIPLE_Y
    
    idx_Y = which(regresion_multiple$x_disponibles == input$regresion_MULTIPLE_Y)
    regresion_multiple$x_disponibles <- regresion_multiple$x_disponibles[-idx_Y]
  })
  
  # Actualizar variable X
  observeEvent(input$boton_anadir, {
    if (input$regresion_MULTIPLE_X %in% regresion_multiple$x_disponibles) {
      idx_X = which(regresion_multiple$x_disponibles == input$regresion_MULTIPLE_X)
      regresion_multiple$x_disponibles <- regresion_multiple$x_disponibles[-idx_X]
      regresion_multiple$x_usadas <- c(regresion_multiple$x_usadas, input$regresion_MULTIPLE_X)
      print(regresion_multiple$x_disponibles)
    }
    
  })
  
  # Resetear
  observeEvent(input$boton_resetear, {
    regresion_multiple$x_disponibles <- current_data$variables
    regresion_multiple$x_usadas <- c()
    regresion_multiple$y <- current_data$variables[2]
    
    updateSelectInput(session, "regresion_MULTIPLE_Y", 
                      choices = current_data$variables,
                      selected = regresion_multiple$y)
    
    updateSelectInput(session, "regresion_MULTIPLE_X", 
                      choices = regresion_multiple$x_disponibles,
                      selected = regresion_multiple$x_disponibles[1]) 
    
  })
  
  # Incluir todo
  observeEvent(input$boton_todo, {
    regresion_multiple$x_usadas <- c(regresion_multiple$x_usadas, regresion_multiple$x_disponibles)
    regresion_multiple$x_disponibles <- c()
  })
  
  observeEvent(input$boton_regresion_MULTIPLE, {
    
    # Seleccionamos las variables
    idx_X = c()
    
    for (var in regresion_multiple$x_usadas) {
      idx_X = c(idx_X, which(current_data$variables == var))
    }
    # print(idx_X)
    
    idx_Y = which(current_data$variables == regresion_multiple$y)
    
    x = current_data$df[, idx_X]
    y = current_data$df[, idx_Y]
    
    xlab=current_data$variables[idx_X]
    ylab=current_data$variables[idx_Y]
    
    # --- Summary de la regresión múltiple: texto ---
    model.regresion_multiple <- lm(formula(regresion_multiple$formula), current_data$df)
    output$regresion_MULTIPLE_summary <- renderPrint({
      cat('Regresión múltiple \nVariable Y =', ylab, 
          ', Variable X =', paste(xlab, collapse=', '), 
          '\n\n')
      summary(model.regresion_multiple)
    })
    
    # --- Plot ---
    output$regresion_MULTIPLE_plot <- renderPlot({
      par(mfrow = c(2, 2))
      plot(model.regresion_multiple)
      par(mfrow = c(1, 1))
    })
  })
  
  output$formula_regresion_MULTIPLE <- renderText({
    regresion_multiple$formula <- paste(regresion_multiple$y, '~', 
                                        paste(regresion_multiple$x_usadas, collapse='+'))
  })
  
  
  # --- Estudio descriptivo: Resumen numérico ---
  output$OUT_resumen_numerico <- renderPrint({
    cat('Variable =', current_data$uni_var, '\n\n')
    summary(current_data$df[[current_data$uni_var]])
  })
  
  
  # --- Mapa ---
  GLugares <- c("Sevilla","Córdoba","Londres","Edimburgo", "Vaticano")
  GLatLong <- data.frame(
    Lat = c(37.35945,37.886129,51.500818,55.95710112,41.90234465),
    Long = c(-5.98814,-4.867665,-0.124510,-3.187314305,12.4568083)
  )
  
  output$map <- renderLeaflet({
    
    cual = which(input$mapa_seleccion==GLugares)
    LAT = GLatLong$Lat[cual]
    LONG = GLatLong$Long[cual]
    ZOOM=18
    # Dibuja el mapa!
    leaflet() %>%
      setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
      addProviderTiles("OpenStreetMap.Mapnik")
  })  
  
  
  
  
}
