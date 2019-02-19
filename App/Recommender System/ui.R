
#
# set up

# libraries
library(dplyr)
library(shiny)
load("rules.RDATA")
load("data_pillar_1.RDATA")

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

pass_grade <- 5.5
high_grade <- 6.5

# 
augment_student_transcript <- function(transcript){
  
  transcript %>%
    
    mutate(
      fail       = grade < pass_grade,
      low        = grade < high_grade,
      grade_ceil = ceiling(grade)
    )
  
}



#
# ui
fluidPage(
  
  # App title
  titlePanel("Recommender System"),
  
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
        choices  = course_following_semester,
        selected = course_following_semester,
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
  
  )
