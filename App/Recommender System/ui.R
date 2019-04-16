
#
# set up

# libraries
library(tidyverse)
library(tidytext)

library(shiny)
library(shinythemes)

#load("rules.RDATA")
load("data_pillar_1.RDATA")
load("data_topic_models.RDATA") #contains distribution, kw, course_all, course_following_semester.
load("rules_clean.RDATA")

load("app_model.RDATA")

#
# ui
navbarPage(
  theme = shinytheme("united"),
  
  # App title
  title = "Recommender System",
  
  # -- RED FLAGS --
  tabPanel(
    
    # Panel Title
    title = "Red Flag",
    
    # Sidebar layout
    sidebarLayout(
      
      # Sidebar panel for inputs
      sidebarPanel(
        
        # Input: student id number
        textInput(
          inputId = "student",
          label   = "Student ID"#,
          #value   = "6087587"
          ),
        
        uiOutput('resetable_input'),
        tags$hr(),
        actionButton("reset_input", "Reset inputs")
        
        ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Output: dataset
        tableOutput(
          outputId = "red_flags"
          )
        
        )
      
      )
    
    ),
  
  # -- TRAFFIC LIGHTS --
  tabPanel(
    
    # Panel Title
    title = "Traffic Lights",
    
    # Sidebar layout
    sidebarLayout(
      
      # Sidebar panel for inputs
      sidebarPanel(
        
        # Input: student id number
        textInput(
          inputId = "student_traffic",
          label   = "Student ID"#,
          #value   = "6087587"
        ),
        
        uiOutput('resetable_input_traffic'),
        tags$hr(),
        actionButton("reset_input_traffic", "Reset inputs")
        
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Output: dataset
        tableOutput(
          outputId = "traffic_lights"
        )
        
      )
      
    )
    
  ),
  
  # -- COURSE RECOMMENDATIONS --
  tabPanel(
    
    # Panel Title
    title = "Course Recommender",
    
    
    # Sidebar layout
    sidebarLayout(
      
      # Sidebar panel for inputs
      sidebarPanel(
        
        # Input: button for the courses students wants to take following semester
        checkboxGroupInput(
          inputId  = "key_words",
          label    = "Academic Interest",
          choices  = sort(app_model$kw[[1]]),      #***********************************************SELECT: overview/manual
          #selected = c("international", "economic", "conflict", "develop", "policy"),
          inline   = TRUE
          ),
        
        # Input: additional key word 1-5
        textInput(
          inputId = "key_word_1",
          label   = "Additional Key Word 1" #,
          #value   = "war"
          ),
        textInput(
          inputId = "key_word_2",
          label   = "Additional Key Word 2"
          ),
        textInput(
          inputId = "key_word_3",
          label   = "Additional Key Word 3"
          ),
        textInput(
          inputId = "key_word_4",
          label   = "Additional Key Word 4"
          ),
        textInput(
          inputId = "key_word_5",
          label   = "Additional Key Word 5"
          )
        
        ),
      
      # Main panel for displaying outputs
      mainPanel(
        h3("Course Recommendations"),
        mainPanel( width = 12,
          tableOutput(outputId = "course_recommendation"), 
          tags$head(tags$style("#course_recommendation{color: black; font-size: 20px;}")
                    )
                  )
      
        )
      
      )
    
    )
  
  )
