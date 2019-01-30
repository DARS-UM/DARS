
# Setup
library(shiny)

fluidPage(
  
  # App title
  titlePanel("Sequence Rules"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      
      # Input: button for the type of rules
      radioButtons(
        inputId = "rules",
        label = "Type of rules",
        choices = c(
          "Association rules" = "AR",
          "Sequence rules"    = "SR"
        ),
        selected = "AR"
      ),
      
      # Input: button for the type of rules
      radioButtons(
        inputId = "item",
        label   = "Type of item in rules",
        choices = c(
          "Course taken"                = "taken",
          "Failed grade"                = "PF",
          "Low grade (< 6.5)"           = "HL",
          "Course taken & not taken"    = "not_taken",
          "Expanded"                    = "expanded_taken",
          "Expanded (Failed grade)"     = "expanded_PF",
          "Expanded (low grade (< 6.5)" = "expanded_PF"
          
        ),
        selected = "taken"
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Slider for the support
      sliderInput(
        inputId = "count",
        label = "Count",
        min   = 5,
        max   = 1000,
        step  = 1,
        value = c(5, 1000)
        ),
      
      # Input: Slider for the Cconfidence
      sliderInput(
        inputId = "confidence",
        label = "Confidence (corrected)",
        min   = 0,
        max   = 1,
        step  = 0.01,
        value = c(0, 1)
      ),
      
      # Input: Slider for the Confidence
      sliderInput(
        inputId = "lift",
        label = "Lift (corrected)",
        min   = 0,
        max   = 100,
        step  = 0.01,
        value = c(0, 100)
      )
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: dataset
      dataTableOutput(outputId = "rules")
      
    )
    
  )
  
)