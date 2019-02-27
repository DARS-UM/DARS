
fluidPage(
  
  # App title
  titlePanel("Rules"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: button for the type of rules
      radioButtons(
        inputId = "rules",
        label   = "Type of rules",
        choices = c(
          "Association rules" = "AR_rulesAPP",
          "Sequence rules"    = "SR_rulesAPP"
        ),
        selected = "AR_rulesAPP"
      ),
      
      # Input: button for the type of rules
      radioButtons(
        inputId = "item",
        label   = "Type of item in rules",
        choices = c(
          "take => take"                   = "taken",
          "fail => fail"                   = "PF",
          "Low grade (< 6.5) => low grade" = "HL",
          "not taken => fail"              = "TPF",
          "not taken => low"               = "THL",
          "less than => less than"         = "G"
        ),
        selected = "taken"
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Slider for the support
      sliderInput(
        inputId = "count",
        label   = "Count",
        min     = 10,
        max     = 3000,
        step    = 1,
        value   = c(10, 3000)
        ),
      
      # Input: Slider for the Cconfidence
      sliderInput(
        inputId = "confidence",
        label   = "Confidence",
        min     = 0,
        max     = 1,
        step    = 0.01,
        value   = c(0.4, 1)
      ),
      
      # Input: Slider for the Confidence
      sliderInput(
        inputId = "lift",
        label   = "Lift",
        min     = 0,
        max     = 100,
        step    = 0.01,
        value   = c(1, 100)
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Text
      helpText("For rules concerning failed courses or course with low grade, we compute the confidence and lift in the following way:"),
      helpText("Confidence(A_fail -> B_fail) = P( (A_fail -> B_fail) | (A_fail -> B) ). In other words, given a student has failed course A and is taking course B, what is the probability that (s)he will fail course B"),
      helpText("Lift(A_fail - B_fail) = Confidence(A_fail -> B_fail) / Probability(B_fail | B). In other words, we compare the confidence of the rule and the probability of failing course_B *given we take course B*.")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: dataset
      dataTableOutput(outputId = "rules")
      
    )
    
  )
  
)