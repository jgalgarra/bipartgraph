# Install and load the necessary packages if not already installed
if (!require("shiny")) install.packages("shiny")

# Load the Shiny library
library(shiny)

# Define the user interface
ui <- fluidPage(
  titlePanel("Text File Reader"),
  fileInput("file", "Choose a text file:"),
  verbatimTextOutput("fileContent")
)

# Define the server logic
server <- function(input, output) {
  # Read the selected file and display its content
  file_data <- reactive({
    req(input$file)
    text <- readLines(input$file$datapath)
    paste(text, collapse = "\n")
  })
  
  output$fileContent <- renderText({
    file_data()
  })
}

# Create and run the Shiny app
shinyApp(ui = ui, server = server)
