shinyUI(fluidPage(
  titlePanel(h3("TwitteR Word-Count Table/WordClouds")), ("Authenticate TwitteR in console first and not with app"),
  
  sidebarLayout(
                sidebarPanel( "Select Options",
                              dateInput('datestart',
                                        label = 'Start Date: yyyy-mm-dd',
                                        value = Sys.Date()-30),
                              dateInput('dateend',
                                        label = 'End Date: yyyy-mm-dd',
                                        value = Sys.Date()),
                              numericInput("words", "Number of Words:", 25),
                              numericInput("n_tweets", "Number of Tweets:", 100),
                              textInput("text", "Search Term:", value = "Federer"),
                              submitButton("Submit")
                              
                                          
                              
                              
                              
                              ),
                mainPanel(textOutput("note"),
                          dataTableOutput("dt"),
                          plotOutput("wordcloud")),
                         
  )
))