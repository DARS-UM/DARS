function(input, output) {
  
  output$red_flags <- renderUI({
    
    n_course <- 25
    
    student <- list(
      
      # transcript: tibble with course and grade
      transcript = tibble(
        
        course = sample(
          x       = course_all,
          size    = n_course,
          replace = FALSE
        ),
        
        grade = rnorm(
          n    = n_course,
          mean = 5,
          sd   = 2
        )
        
      ),
      
      # interest of student (to be used with topic model)
      interest = c("key", "words", "related", "to", "topics")
      
    )
    
    
    course_low <-  student$transcript %>%
      augment_student_transcript %>%
      filter(low) %>%
      pull(course)
    
    
    red_flags <- SR$HL %>%
      
      # select appropriate rules
      filter(
        rhs_course %in% input$course_chosen,
        lhs_course %in% course_low
      ) %>%
      
      # if several rule with same rhs, keep rule with highest confidence
      group_by(
        rhs_course
      ) %>%
      top_n(
        n  = 1,
        wt = confidence
      ) %>%
      ungroup %>%
      
      # provide explanation
      transmute(
        
        explanation = paste(
          "Red flag for ",
          rhs_course,
          ":<br/>",
          "You obtained less than 6.5/10 in ",
          lhs_course,
          "and ",
          round(confidence * 100, digits = 1),
          "% of the ",
          lhs.rhsTake.count,
          " students who have taken ", 
          rhs_course,
          "after obtaining a similar grade in ",
          lhs_course,
          "did not do well in ",
          rhs_course,
          ".",
          sep = ""
        )
        
      ) %>%
      
      pull %>%
      
      sort %>% 
      
      paste0(
        collapse = "<br/><br/>"
        ) %>% 
      
      HTML
    
    
  })
  
}