# Authentic Twitter in console first!!!!!!

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(twitteR))
suppressPackageStartupMessages(library(shiny))
setup_twitter_oauth("marsZEdgNRUJU4kjv9rDBc91Q", "2egtj3egISKhUsVRzJaPnwMSmK0af4O1lw5XE8XATdFvj6kUeF", "3140902760-oc8NAAm7C4wN1SF1h77NlSREETXTcTZmXujpG8Y", "JB7k6uDkdcuFFciv2rDE4KmtPRjXMAo3BDyDPvAKmEN7c")

shinyServer(
  function(input, output) {
    output$note <- renderText({"If you are receiving an argument error, it may be due to TwitteR not being able to
                               pull the requested number of tweets due to a lack of them. Either extend the date range
                               or lower the requested number of tweets."})
      
    output$dt <- renderDataTable({ 
      tweets <- searchTwitter(input$text, 
                              since=as.character(input$datestart), until=as.character(input$dateend),
                              n=input$n_tweets, lang="en")
                              
                               
      tweets <- sapply(tweets, function(x){x$text})
      tweets <- str_replace_all(tweets,"[^[:graph:]]", " ") 
      
      tweets1 <<- tweets %>%
        tolower() %>%
        removeNumbers() %>%
        removePunctuation() %>%
        removeWords(removePunctuation(stopwords("english"))) %>%
        stemDocument() %>%
        stripWhitespace()
      
      top.n.tweets <- str_split(tweets1, pattern= " ") %>%
        unlist() %>%
        table() %>%
        sort(decreasing=TRUE) %>%
        .[1:input$words]
      
      top.n.tweets <- as.matrix(top.n.tweets)
      datatable(top.n.tweets,colnames=c("Word","Count"),caption=paste("Top ",input$words,
               " Non-stopword Words From tweets including '",input$text,"'",sep=''),          
                class = 'cell-border stripe',filter='top')
    })
    
    output$wordcloud <- renderPlot({
      dummy <- input$text #forces Shiny to re-evaluate this output when text changes
      dummy2 <- input$n_tweets #similar to above
      
      tweets2 <- VectorSource(tweets1) %>% Corpus()
      wordcloud(tweets2, scale=c(5,0.5), max.words=input$words, random.order=FALSE,
                rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "BuPu")) + 
        title("WordCloud")
    })
    
  }
)