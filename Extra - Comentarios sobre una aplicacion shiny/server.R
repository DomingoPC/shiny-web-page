# Text of the books downloaded from:
# A Mid Summer Night's Dream:
#  http://www.gutenberg.org/cache/epub/2242/pg2242.txt
# The Merchant of Venice:
#  http://www.gutenberg.org/cache/epub/2243/pg2243.txt
# Romeo and Juliet:
#  http://www.gutenberg.org/cache/epub/1112/pg1112.txt

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Hasta que no se actualice input$update no reacciona. Solo hace
    # falta que se actualice ese, porque input$selection esá aislado.
    input$update
    
    isolate({
      # Pantallita de carga que aparece abajo a la derecha con el mensaje
      # que pone (Processing corpus) y una barra de carga
      withProgress({
        setProgress(message = "Processing corpus...")
        
        # Función de global.R:
        # + Comprueba si el libro seleccionado está entre los disponibles
        # + Coge las palabras del libro
        # + Aplica transformaciones: minúsculas, quita el punto, ...
        # + Limita la longitud de las palabras...
        # + Ordena de mayor a menor
        getTermMatrix(input$selection)
      })
    })
  })

  # Make the wordcloud drawing predictable during a session
  # 'repeatable' hace que la función que tiene dentro se repita varias veces
  wordcloud_rep <- repeatable(wordcloud)
  
  # Plot
  output$plot <- renderPlot({
    v <- terms()
    
    # wordcloud_rep definido arriba 
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}
