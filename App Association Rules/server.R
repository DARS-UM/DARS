
# Setup
library(shiny)
library(dplyr)
load("sequence_rules.RDATA")

# Server
function(input, output) {
  
  output$rules <- renderDataTable({
    
    dataset <- sequence_rules
      
    dataset %>%
      filter(
        between(support  , input$support[1]   , input$support[2]),
        between(Conf_corr, input$confidence[1], input$confidence[2]),
        between(lift_corr, input$lift[1]      , input$lift[2]      )
      )
    
  })
  
}