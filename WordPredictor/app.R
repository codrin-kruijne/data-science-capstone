# 

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("WordPredictor"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput(inputId = "text", label = "Input your text here")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h3("Processed"),
         textOutput("processed"),
         h3("Prediction"),
         textOutput("prediction")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Reading in the lookup data
  
  bi_lookup <- readRDS("data/server_lookup.rds")


  # Prediction function for looking up predictions in saved tables
  
  predictWord <- function(string, bi_lookup) {
   
    str(bi_lookup)
    bi_df <- as.data.frame(bi_lookup)
    prediction <- bi_df[1, ]$variable
    print(paste("Prediction! ", prediction))
    prediction
    
  }
  
  # Display what text was used for the prediction
  output$processed <- renderText({
  
    req(input$text)
    input$text
  
  })
  
  # Retrieve most likely next word
  output$prediction <- renderText({
    
    req(input$text)
    predicted <- predictWord(input$text, bi_lookup)
    print(paste("Predicted! ", predicted))
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

