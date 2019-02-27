

#
# Setup
library(shiny)
library(dplyr)
load("rules.RDATA")


#
# Server
function(input, output) {
  
  output$rules <- renderDataTable(
    
    expr = {
      
      # Selected type of rules
      get(input$rules)[[input$item]] %>%
        
        # filter rows based on the ranges of the three buttons
        filter(
          between(count     , input$count[1]     , input$count[2]     ),
          between(confidence, input$confidence[1], input$confidence[2]),
          between(lift      , input$lift[1]      , input$lift[2]      )
        ) %>%
        select(-lhs_course,-lhs_outcome, -rhs_course, -rhs_outcome, -rhs.support)
      
    },
    
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50, 100)
      )
    
    )
  
}
