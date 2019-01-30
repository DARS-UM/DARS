
# Setup
library(shiny)
library(dplyr)
load("AR.RDATA")

# Server
function(input, output) {
  
  output$rules <- renderDataTable({
    
    rules <- get(
      paste(input$rules, input$item, sep = "_")
      )
      
    rules %>%
      filter(
        between(count     , input$count[1]     , input$count[2]),
        between(confidence, input$confidence[1], input$confidence[2]),
        between(lift      , input$lift[1]      , input$lift[2]      )
      )
    
  })
  
}