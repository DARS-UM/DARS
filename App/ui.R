
# Setup
library(shiny)

fluidPage(
  
  # App title
  titlePanel("Sequence Rules"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Slider for the support
      sliderInput(
        inputId = "support",
        label = "Support",
        min   = 0,
        max   = 1,
        step  = 0.01,
        value = c(0, 1)
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
      
      # Input: Slider for the Cconfidence
      sliderInput(
        inputId = "lift",
        label = "Lift (corrected)",
        min   = -15,
        max   = 15,
        step  = 0.01,
        value = c(-10, 10)
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Text
      helpText("TODO: use logarithmic scale for sliders")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: dataset
      dataTableOutput(outputId = "rules")
      
    )
    
  )
  
)