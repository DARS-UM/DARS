

#
# Setup
library(shiny)
library(dplyr)
load("AR.RDATA")
load("SR.RDATA")


#
# Server
function(input, output) {
  
  output$rules <- renderDataTable(
    
    expr = {
      
      # Use chosen data set
      get(input$rules)[[input$item]] %>%
        
        # filter rows based on the ranges of the three buttons
        filter(
          between(count     , input$count[1]     , input$count[2]     ),
          between(confidence, input$confidence[1], input$confidence[2]),
          between(lift      , input$lift[1]      , input$lift[2]      )
        )
      
    },
    
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50, 100)
      )
    
    )
  
}
