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
   
   output$processed <- renderText({
     input$text
   })
  
   output$prediction <- renderText({
      "PREDICTED!!"
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

