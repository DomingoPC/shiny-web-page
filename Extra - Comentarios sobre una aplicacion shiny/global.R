library(tm)
library(wordcloud)
library(memoise)

# The list of valid books
books <<- list("A Midsummer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  # Si no sale un libro conocido, mejor parar
  if (!(book %in% books))
    stop("Unknown book")
  
  # Lee el libro
  text <- readLines(sprintf("./%s.txt.gz", book),
    encoding="UTF-8")
  
  # Corrige cosas del texto
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower)) # minúsculas
  myCorpus = tm_map(myCorpus, removePunctuation) # quita los puntos
  myCorpus = tm_map(myCorpus, removeNumbers) # quita los números
  myCorpus = tm_map(myCorpus, removeWords,
         c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but")) 
        # quita ciertas palabras

  # Limita el tamaño de las palabras
  myDTM = TermDocumentMatrix(myCorpus,
              control = list(minWordLength = 1))
  
  # Resultados en matriz
  m = as.matrix(myDTM)
  
  # Ordena de mayor a menor
  sort(rowSums(m), decreasing = TRUE)
})
