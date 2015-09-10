shinyUI(fluidPage(
  
  titlePanel("Most Popular Words of /r/reed, /r/portland, and 'Sixty Folk Tales'"),

  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("/r/reed", "/r/portland", "Sixty Folk Tales")),
      
      numericInput("n", "Select Number of Top Words:", 10),
      
      submitButton("Submit")
    ),
    
  
    mainPanel(
      dataTableOutput("table"),
      plotOutput("cloud")
      
    )
  )
))