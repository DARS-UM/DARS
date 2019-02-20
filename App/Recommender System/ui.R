
#
# set up

# libraries
library(tidyverse)
library(tidytext)
library(shiny)

load("rules.RDATA")
load("data_pillar_1.RDATA")
load("LDA_overview.RDATA")
load("LDA_keyword_DELETE_.RDATA") #TO DELETE, THIS SHOULD BE INCLUDED IN 

# set seed
set.seed(1)

# for convenience
course_all <- d_course %>%
  inner_join(
    d_transcript,
    by = "Course ID"
  ) %>%
  distinct(
    `Course ID`
  ) %>%
  # remove semester abroad, skills and projects
  filter(
    ! str_detect(`Course ID`, patter = "SA|SKI|PRO")
  ) %>%
  pull

course_following_semester <- sample(
  x       = course_all,
  size    = 60,
  replace = FALSE
  ) %>% sort



#
# ui
navbarPage(
  
  # App title
  title = "Recommender System",
  
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
          value   = "6087587"
          ),
        
        # Input: button for the courses students wants to take following semester
        checkboxGroupInput(
          inputId  = "course_chosen",
          label    = "Tentative Courses for following period",
          choices  = course_all,
          selected = course_all,
          inline   = TRUE
          )
        
        ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Output: dataset
        htmlOutput(
          outputId = "red_flags"
          )
        
        )
      
      )
    
    ),
  
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
          choices  = sort(kw),
          selected = c("international", "economic", "conflict", "develop", "policy"),
          inline   = TRUE
          ),
        
        # Input: additional key word 1-5
        textInput(
          inputId = "key_word_1",
          label   = "Additional Key Word 1",
          value   = "war"
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
        
        # Output: dataset
        htmlOutput(
          outputId = "course_recommendation"
          )
        
        )
      
      )
    
    )
  
  )
