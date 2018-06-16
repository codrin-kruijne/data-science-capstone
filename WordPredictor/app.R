library(shiny)

require(tidyr)
require(dplyr)
require(stringr)
require(ggplot2) 
require(gridExtra)
require(qdap)
require(data.table)

# Define UI

ui <- navbarPage("Word predictor",
                 tabPanel("Intro",
                          tags$h1("A simple word predictor"),
                          tags$p("Welcome to my simple word predictor")),
                 tabPanel("Try me!",
                          tags$h1("A simple word predictor"),
                          tags$p("Welcome to my simple word predictor!"),
                          textInput(inputId = "text", label = "Please enter somt text here, then hit ENTER and wait for magic to happen"),
                          h3("Prediction"),
                          textOutput("prediction"),
                          h3("Alternatives"),
                          tableOutput("alternatives")),
                 tabPanel("Presenation",
                          tags$h1("Language modeling and app development"),
                          tags$p("Welcome to my simple word predictor!"),
                          tags$iframe(src = "https://shiny.rstudio.com/reference/shiny/latest/renderDataTable.html")),
                 tabPanel("Training data",
                          tags$h1("Training text characteristics"),
                          tags$p("Welcome to my simple word predictor!")),
                 tabPanel("Code",
                          tags$h1("Learning data science in R"),
                          tags$p("Welcome to my simple word predictor!")),
                 tabPanel("About me",
                          tags$head(tags$script(type = "text/javascript", src = "https://platform.linkedin.com/badges/js/profile.js", async = NA, defer = NA)),
                          tags$h1("A budding data scientist"),
                          tags$p("Welcome to my simple word predictor!"),
                          tags$div(class = "LI-profile-badge", `data-version` = "v1", `data-size` = "large", `data-locale` = "en_US", `data-type` = "horizontal", `data-theme` = "light", `data-vanity` = "codrinkruijne", tags$a(class="LI-simple-link", href='https://nl.linkedin.com/in/codrinkruijne?trk=profile-badge', "Codrin Kruijne")))
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
  
  # A lookup function: given a string, look up the most probable next word
  
  alt_table <<- NULL
  
  predictWord <- function(string) {
    
    # QDAP clean string
    string <- replace_ordinal(string)
    string <- replace_number(string, remove = TRUE)
    string <- replace_abbreviation(string)
    string <- replace_contraction(string)
    string <- replace_symbol(string, # except for # and @
                             pound = FALSE, # as we have twitter
                             at = FALSE) # texts to process later
    string <- qdap::qprep(string)
    
    # parse string: remove punctiation, lowercase, split into words
    words <- str_to_lower(string) %>%
      str_remove_all(pattern = "[:punct:]") %>%
      str_split(pattern = " ")
    
    # check length
    words_n <- length(words[[1]])
    
    if(words_n >= 5){ start <- words_n - 3 } else start <- 1
    
    end <- words_n
    selection <- words[[1]][start:end]
    str(selection)
    lookup_n <- length(selection)
    
    # What to look up in the lookup table
    print(paste("Lookup: ", selection))
    
    # Lookup ngram in data.table organised by ngram probability
    
    start_level <- lookup_n + 1
    print(paste("Start level:", start_level))
    
    ngram_lookup <- function(selection, level){
      
      backoff <- FALSE
      lookup <- str_c(selection, collapse = " ")
      
      if(level == 5){ # fivegram lookup
        print("Fivegram lookup!")
        
        # lookup fivegram with highest MLE
        prediction <- fivegram_lookup[base == lookup][1]$prediction
        
        if(!is.na(prediction)){ # if found
          print("FOUND FIVEGRAM!")
          print("Alternatives:")
          alt_table <<- head(fivegram_lookup[base == lookup], 50)
          print(alt_table)
          return(prediction)
          break
        }
        else {
          print("BACKING OFF!!")
          level <- 4
          backoff <- TRUE
        }
      }
      
      if(level == 4){ # fourgram lookup
        print("Fourgram lookup!")
        
        if(backoff){
          # shorten search string
          lookup <- str_c(str_split(lookup, pattern = " ")[[1]][2:4], collapse = " ") # remove last word from search string
          last <- str_c(str_split(lookup, pattern = " ")[[1]][4], collapse = " ")
          print(paste("New lookup:", lookup))
          
          # retrieve matching fourgrams
          fourgrams <- fourgram_lookup[base == lookup]
          str(fourgrams)
          print("Old values:")
          print(fourgrams[prediction == last, .(base, prediction, mle)])
          # discount MLE of failed fivegram
          fourgrams[prediction == last, mle := 0.4 * mle] 
          print("New values:")
          print(fourgrams[prediction == last, .(base, prediction, mle)])
        }
        else {
          fourgrams <- fourgram_lookup[base == lookup]
        }
        
        # lookup fourgram with highest MLE
        prediction <- fourgrams[1]$prediction
        print(paste("Prediction:", prediction))
        
        if(!is.na(prediction)){
          print("FOUND FOURGRAM!")
          print("Alternatives:")
          alternatives <<- head(fourgram_lookup[base == lookup], 50)
          print(alternatives)
          return(prediction)
          break
        }
        else {
          level <- 3
          backoff <- TRUE
        }
        
      }
      
      if(level == 3){ # trigram lookup
        print("Trigram lookup!")
        
        if(backoff){
          # shorten search string
          lookup <- str_c(str_split(lookup, pattern = " ")[[1]][2:3], collapse = " ") # remove last word from search string
          last <- str_c(str_split(lookup, pattern = " ")[[1]][3], collapse = " ")
          print(paste("New lookup:", lookup))
          
          # retrieve matching trigrams
          trigrams <- trigram_lookup[base == lookup]
          str(trigrams)
          print("Old values:")
          print(trigrams[prediction == last, .(base, prediction, mle)])
          # discount MLE of failed fourgram
          trigrams[prediction == last, mle := 0.4 * mle]
          print("New values:")
          print(trigrams[prediction == last, .(base, prediction, mle)])
        }
        else {
          trigrams <- trigram_lookup[base == lookup]
        }
        
        # lookup trigram with highest MLE
        prediction <- trigrams[1]$prediction
        print(paste("Prediction:", prediction))
        
        if(!is.na(prediction)){
          print("FOUND TRIGRAM!")
          print("Alternatives:")
          alt_table <<- head(trigram_lookup[base == lookup], 50)
          print(alt_table)
          return(prediction)
          break
        }
        else {
          level <- 2
          backoff <- TRUE
        }
      }
      
      if(level == 2){ # bigram lookup
        print("Bigram lookup!")
        
        if(backoff){
          # shorten search string
          lookup <- str_c(str_split(lookup, pattern = " ")[[1]][1:1], collapse = " ") # remove last word from search string
          last <- str_c(str_split(lookup, pattern = " ")[[1]][2], collapse = " ")
          print(paste("New lookup:", lookup))
          
          # retrieve matching bigrams
          bigrams <- bigram_lookup[base == lookup]
          print("Old values:")
          print(bigrams[prediction == last, .(base, prediction, mle)])
          
          # discount MLE of failed trigram
          bigrams[prediction == last, mle := 0.4 * mle]
          print("New values:")
          print(bigrams[prediction == last, .(base, prediction, mle)])
        }
        else {
          bigrams <- bigram_lookup[base == lookup]
        }
        
        # lookup bigram with highest MLE
        prediction <- bigrams[1]$prediction
        print(paste("Prediction:", prediction))
        
        if(!is.na(prediction)){
          print("FOUND BIGRAM!")
          print("Alternatives:")
          alt_table <<- head(bigram_lookup[base == lookup], 50)
          print(alt_table)
          return(prediction)
          break
        }
        else {
          print("PLEASE ENTER SOME MORE WORDS!!")
        }
      }
      
    } # end of ngram_lookup
    
    ngram_lookup(selection, start_level)
    
  }
  
  # Display what text was used for the prediction
  output$processed <- renderText({
  
    req(input$text)
    input$text
  
  })
  
  # Retrieve most likely next word
  output$prediction <- renderText({

    req(input$text)
    predicted <- predictWord(input$text)
    print(predicted)

  })
  
  # Show alternatives
  output$alternatives <- renderTable({
    
    req(input$text)
    print(alt_table)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

