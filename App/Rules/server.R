

#
# Setup
library(shiny)
library(dplyr)
#load("rules.RDATA")
load("rules_clean.RDATA")


#
# Server
function(input, output) {
  
  output$rules <- renderDataTable(
    
    expr = {
      
      # Selected type of rules
      my_rules <- as_tibble(rules_clean) %>%
        select(-rules_RS) %>%
        filter(type_rule == input$rules) %>%
       #get(input$rules) %>%
         #filter for selected type of rules
         filter(ID == input$item) %>%
        pull(rules_rules) 
      
      my_rules[[1]] %>%
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
