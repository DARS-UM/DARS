library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell)

load("rules.RDATA")
load("data_pillar_1.RDATA")
load("data_topic_models.RDATA") #contains distribution, kw, course_all, course_following_semester.



function(input, output) {
  #
  ##Set up
  
  #Return student function
  student_trans <- function(d_student){
 
    # Student profile
    student <- list()
    
    # Student transcript
    student$transcript <- d_transcript %>%
      
      filter(
        `Student ID` == d_student
      ) %>%
      
      select(
        course = `Course ID`,
        grade  = Grade
      ) %>%
      
      mutate(
        fail       = grade < 5.5,
        low        = grade < 6.5,
        grade_ceil = ceiling(grade)
      )
    
    # Courses with low score
    student$course_low <- student$transcript %>%
      filter(
        low
      ) %>%
      pull(
        course
      )
    
    # Courses with fail score
    student$course_fail <- student$transcript %>%
      filter(
        fail
      ) %>%
      pull(
        course
      )
    
    # Course not taken
    student$course_not_taken <- setdiff(
      x = d_course$`Course ID`, # list of all courses offered
      y = student$transcript$course
    )
    return(student)
  }
  
  #Reactive student function 
  current_student <- reactive({
   student_trans(input$student)
  })
  
  #
  ##Red Flags
  output$red_flags <- renderUI({
    #student
    student <- current_student()
    
    # rules
    rules <- list()
    
    # rules not
    rules$not <- SR_RSAPP$THL %>%
      
      # select appropriate rules
      filter(
        lhs_course %in% student$course_not_taken,
        rhs_course %in% input$course_chosen
        ) %>%
      
      # provide explanation
      mutate(
        explanation = paste(
          "Red flag for ",
          rhs_course,
          ":<br/>",
          "You have not taken ",
          lhs_course,
          " and ",
          round(confidence * 100, digits = 1),
          "% of the ",
          lhs.rhsTake.count,
          " students who have taken ", 
          rhs_course,
          " without first taking ",
          lhs_course,
          " obtained less than 6.5/10 in  ",
          rhs_course,
          ".",
          sep = ""
        )
      )
    
    
    # rules low
    rules$low <- SR_RSAPP$HL %>%
      
      filter(
        lhs_course %in% student$course_low,
        rhs_course %in% input$course_chosen
        ) %>%
      
      mutate(
        explanation = paste(
          "Red flag for ",
          rhs_course,
          ":<br/>",
          "You obtained less than 6.5/10 in ",
          lhs_course,
          " and ",
          round(confidence * 100, digits = 1),
          "% of the ",
          lhs.rhsTake.count,
          " students who have taken ", 
          rhs_course,
          " after obtaining less than 6.5/10 in ",
          lhs_course,
          " obtained less than 6.5/10 in  ",
          rhs_course,
          ".",
          sep = ""
          )
        )
    
    
    # rules fail
    rules$fail <- SR_RSAPP$PF %>%
      
      filter(
        lhs_course %in% student$course_fail,
        rhs_course %in% input$course_chosen
        ) %>%
      
      mutate(
        explanation = paste(
          "Red flag for ",
          rhs_course,
          ":<br/>",
          "You have failed ",
          lhs_course,
          " and ",
          round(confidence * 100, digits = 1),
          "% of the ",
          lhs.rhsTake.count,
          " students who have taken ", 
          rhs_course,
          " after failing ",
          lhs_course,
          " also failed ",
          rhs_course,
          ".",
          sep = ""
        )
      )
    
    
    # rules grade
    rules$grade <- SR_RSAPP$G %>%
      
      left_join(
        student$transcript,
        by = c("lhs_course" = "course")
        ) %>%
      
      # select appropriate rules
      filter(
        grade <= lhs_outcome, # lhs
        rhs_course %in% input$course_chosen
      ) %>%
      
      mutate(
        explanation = paste(
          "Red flag for ",
          rhs_course,
          ":<br/>",
          "You have obtained less than ",
          lhs_outcome,
          "/10 in ",
          lhs_course,
          " and ",
          round(confidence * 100, digits = 1),
          "% of the ",
          lhs.rhsTake.count,
          " students who have taken ", 
          rhs_course,
          " after obtaining less than ",
          lhs_outcome,
          "/10 in ",
          lhs_course,
          " obtained less than ",
          rhs_outcome,
          "/10 in ",
          rhs_course,
          ".",
          sep = ""
        )
      )
    
    
    rules <- rules %>%
      
      # bind data sets from lists `rules` into one data set
      lapply(
        FUN = function(rules) {
          rules %>% select(
            rhs_course,
            confidence,
            explanation
            )      
          }
        ) %>%
      
      bind_rows %>%
      
      # if several rule with same rhs, keep rule with highest confidence
      group_by(
        rhs_course
        ) %>%
      top_n(
        n  = 1,
        wt = confidence
        )
    
    
    if(nrow(rules) == 0){
      
      "No red flag"
      
    }else{
      
      rules  %>%
        
        pull(explanation) %>%
        
        sort %>% 
        
        paste0(
          collapse = "<br/><br/>" # skip a line between individual red flags
        ) %>% 
        
        HTML
      
    }
    
    
  })
  
  output$course_recommendation <- renderUI({
    
    
    #
    # Set up
    
    beta_distribution <- distribution$beta$manual 
    gamma_distribution <- distribution$gamma$manual
    
    # Key words
    key_words_additional <- c(
      input$key_word_1,
      input$key_word_2,
      input$key_word_3,
      input$key_word_4,
      input$key_word_5
      )
    
    
    stem_hunspell <- function(term) {
      stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
      n_stems <- length(stems)            # number of stems
      if (n_stems == 0) term              # if no stems, return original term
      else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem
    }
    
    key_words_additional <- sapply(key_words_additional, stem_hunspell)
    
    key_words <- c(input$key_words, key_words_additional)
    
    #
    # Course recommendation
    student <- list()
    
    student$topic_score <- beta_distribution %>%
      
      # Topic score
      filter(
        term %in% key_words
        ) %>%
      group_by(
        topic
        ) %>%
      summarize(
        topic_score = sum(beta)
        ) %>%
      ungroup %>%
      
      # Course score
      full_join(
        gamma_distribution,
        by = "topic"
        ) %>%
      group_by(
        document
        ) %>%
      summarise(
        course_score = sum(gamma * topic_score)
        ) %>%
      ungroup %>%
      
      # Recommendations
      top_n(
        n  = 20,
        wt = course_score
        ) %>%
      
      # Key words per recommendation
      left_join(
        gamma_distribution,
        by = "document"
      ) %>%
      
      left_join(
        beta_distribution,
        by = "topic"
        ) %>%
     
      filter(
        term %in% key_words
        ) %>%
      
      group_by(
        term,
        document
        ) %>%
      summarize(
        word_contribution_to_document = sum(gamma * beta)
        ) %>%
      
      group_by(
        document
        ) %>%
      top_n(
        n  = 3,
        wt = word_contribution_to_document
        ) %>%
      arrange(word_contribution_to_document) %>%
      summarize(
        key_words = paste(term, collapse = ", ")
        ) %>%
      ungroup %>%
      
      # Editing
      left_join(
        select(d_course, `Course ID`, `Course Title`),
        by = c("document" = "Course ID")
        ) %>%
      
      transmute(
        recommendation = paste(
          "Course Recommendation", "<em>","<b>",
          document, `Course Title`,"</b>","</em>",
          "because you selected the key words:" ,"<b>", key_words,"</b>"
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