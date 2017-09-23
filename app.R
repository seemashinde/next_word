setwd("F:\\R\\capston_proj\\Data\\en_US")

source('getPredWord.R') 

if (interactive()) {
  
  shinyApp(
    ui = basicPage(
      
      
      # Application title
      titlePanel("Next word prediction"),
      
      textInput("sentence", label = "Enter your text :"),
      submitButton("Submit", icon("refresh")),
      helpText("When you click the button above, you will see",
               "the next predicted words below :"),
      verbatimTextOutput("value", placeholder = FALSE)
    ),
    server = function(input, output) {
      
      # submit buttons do not have a value of their own,
      # they control when the app accesses values of other widgets.
      # input$num is the value of the number widget.
      #output$value <-""
      output$value <- renderPrint({ getPredWord(input$sentence) })
      
    }
  )
}
