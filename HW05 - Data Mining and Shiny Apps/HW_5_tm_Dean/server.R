suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(datasets))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(stringr))
options(warn=-1)

reed <- read.csv("reedcollege.comments.csv", stringsAsFactors = FALSE, header=FALSE) %>%
  tbl_df() 

portland <- read.csv("portland.comments.csv", stringsAsFactors = FALSE, header=FALSE) %>%
  tbl_df()

sixty <-
  readLines("Sixty_Folk_Tales.txt", encoding="UTF-8") %>%
  as.character()

sixty <- sixty %>%
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  removeWords(removePunctuation(stopwords("english"))) %>%
  stemDocument() %>%
  stripWhitespace()

reed <- reed %>%
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  removeWords(removePunctuation(stopwords("english"))) %>%
  stemDocument() %>%
  stripWhitespace()

portland <- portland %>%
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  removeWords(removePunctuation(stopwords("english"))) %>%
  stemDocument() %>%
  stripWhitespace()

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "/r/reed" = reed,
           "/r/portland" = portland,
           "Sixty Folk Tales" = sixty)
  })
  
  output$table <- renderDataTable({
    top.n <- str_split(datasetInput(), pattern= " ") %>%
      unlist() %>%
      table() %>%
      sort(decreasing=TRUE) %>%
      .[1:input$n]
    
    top.n <- as.matrix(top.n)
    datatable(top.n ,colnames=c("Word","Count"),
              caption=paste("Top",input$n,"Non-stopword Words From",input$dataset),
              class = 'cell-border stripe',filter='top')
    
  })
  
  output$cloud <- renderPlot({
    top.n.vec <- VectorSource(datasetInput()) %>% Corpus()
    wordcloud(top.n.vec, scale=c(10,0.5), max.words=input$n, random.order=FALSE,
              rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "BuPu")) + 
      title("WordCloud")
  })
})