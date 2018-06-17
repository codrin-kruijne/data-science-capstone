library(shiny)
library(shinythemes)

require(tidyr)
require(dplyr)
require(stringr)
require(ggplot2) 
require(gridExtra)
require(qdap)
require(data.table)

# Define UI

ui <- navbarPage(theme = shinytheme("united"),
                 "Modest word predictor",
                 tabPanel("Intro",
                          tags$h1("Basic word prediction"),
                          tags$p("Welcome to my simple word predictor. This word predictor app was created as Capstone project in the final course of the Coursera Data Science Specialization in collaboration with SwiftKey. The challenge was to create a simple word prediction app based on training data. In the following tabs you can:"),
                          tags$ul(
                            tags$li("Try the app; input some text and see what it predicts"), 
                            tags$li("Look at a short presentation on the language model and app creation"), 
                            tags$li("Explore some characteristics of the training data"),
                            tags$li("Find the code for the language model and app on GitHub"),
                            tags$li("Learn a bit more about me!")
                          ),
                          tags$p("I have learned a lot over these few weeks, especially that there is so much more to learn and that I miss colleagues for brainstorming and code reviewing. Now, what will I build next to learn more :-)?!")),
                 tabPanel("Try me!",
                          tags$h1("A simple word predictor"),
                          tags$p("Welcome to my simple word predictor!"),
                          tags$h2("Input some words"),
                          textInput(inputId = "text", label = "Please enter somt text here, then hit ENTER and wait for magic to happen"),
                          tags$br(),
                          tags$br(),
                          tags$h3("Predicted next word"),
                          tags$p("At the top of the list of possible following words the model offers is the most probable following word:"),
                          tags$h4(textOutput("prediction")),
                          tags$br(),
                          tags$br(),
                          tags$h3("Alternatives"),
                          tags$p("More possible following word alternatives the model has calculated based on the training data in decreasing order of likelihood"),
                          tableOutput("alternatives")),
                 tabPanel("Presenation",
                          tags$h1("Language modeling and app development"),
                          tags$p("Welcome to my simple word predictor!"),
                          tags$iframe(src = "presentation.html", width = "800", heigth = "600", seamless=NA)),
                 tabPanel("Training data",
                          tags$h1("Training text characteristics"),
                          tags$p("Welcome to my simple word predictor!")),
                 tabPanel("Code",
                          tags$h1("Learning data science in R"),
                          tags$p("You can find all the beginner's code on GitHub!"),
                          tags$a(href = "https://github.com/codrin-kruijne/data-science-capstone", "Check out the repository!"),
                          tags$a(href = "https://github.com/codrin-kruijne/data-science-capstone", tags$img(src = "GitHub-Mark-120px-plus.png", width = "120"))),
                 tabPanel("About me",
                          tags$h1("A budding data scientist"),
                          tags$p("Welcome to my simple word predictor!"),
                          tags$a(href = "https://www.linkedin.com/in/codrinkruijne/", tags$img(src = "LinkedInCodrinKruijne.PNG", width = "300")),
                          tags$br(),
                          tags$br(),
                          tags$a(href = "https://www.datacamp.com/profile/codrinkruijne", tags$img(src = "DataCampLogo.PNG", width = "252")))
)

### Define server logic required to draw a histogram

server <- function(input, output) {

  print("Executing server code!!")
  
  # Reading in the lookup data
  
  print(Sys.time())
  bigram_lookup <- readRDS("data/bigram_lookup.rds")
  trigram_lookup <- readRDS("data/trigram_lookup.rds")
  fourgram_lookup <- readRDS("data/fourgram_lookup.rds")
  fivegram_lookup <- readRDS("data/fivegram_lookup.rds")
  print("Lookout tables read!")
  print(Sys.time())
  
  # Source the wordPredictor function that is shared between model evaluator and Shiny App
  
  source("./wordPredictorFunction.R", local = TRUE)
  
  # Display what text was used for the prediction
  output$processed <- renderText({
  
    req(input$text)
    input$text
  
  })
  
  # Retrieve most likely next word
  output$prediction <- renderText({

    req(input$text)
    predicted <- predictWord(input$text)[[1]]
    print(predicted)

  })
  
  # Show alternatives
  output$alternatives <- renderTable({
    
    req(input$text)
    print(alt_table)
    
  }, hover = TRUE, width = 600, digits = 9)
}

# Run the application 
shinyApp(ui = ui, server = server)

