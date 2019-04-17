library(tidyverse)
library(tidytext)
library(shiny)
library(shinythemes)

#
#General set up---------------------------------------------------------------------------------

load("data_pillar_1.RDATA")
load("data_topic_models.RDATA") #contains distribution, kw, course_all, course_following_semester.
load("rules_clean.RDATA")

kw_used <- app_model$kw[[1]]

#
# ui ---------------------------------------------------------------------------------
navbarPage(
  theme = shinytheme("united"),
  
  # App title
  title = "Recommender System",
  
  # ---------------------------------------------------------------------------------
  # -- RED FLAGS --
  # ---------------------------------------------------------------------------------
  
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
          label   = "Student ID",
          value   = "6113335"
          ),
        
        uiOutput('resetable_input'),
        tags$head(tags$style("#resetable_input{color: black; font-size: 19px;}")),
        tags$hr(),
        actionButton("reset_input", "Reset inputs")
        
        ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Output: dataset
        tableOutput(
          outputId = "red_flags"
          ),
        tags$head(tags$style("#red_flags{color: black; font-size: 20px;}"))
        
        )
      
      )
    
    ),
  
  # ---------------------------------------------------------------------------------
  # -- TRAFFIC LIGHTS --
  # ---------------------------------------------------------------------------------
  
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
          label   = "Student ID",
          value   = "6113335"
        ),
        
        uiOutput('resetable_input_traffic'),
        tags$head(tags$style("#resetable_input_traffic{color: black; font-size: 19px;}")),
        tags$hr(),
        actionButton("reset_input_traffic", "Reset inputs")
        
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Output: dataset
        tableOutput(
          outputId = "traffic_lights"
        ),
        tags$head(tags$style("#traffic_lights{color: black; font-size: 20px;}"))
        
      )
      
    )
    
  ),
  
  # ---------------------------------------------------------------------------------
  # -- COURSE RECOMMENDATIONS --
  # ---------------------------------------------------------------------------------
  
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
          choices  = sort(kw_used),      
          #selected = c("international", "economic", "conflict", "develop", "policy"),
          inline   = TRUE
          ),
        tags$head(tags$style("#key_words{color: black; font-size: 19px;}")),
        
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
