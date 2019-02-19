library(tidyverse)
library(tidytext)
library(stringr)

load("rules.RDATA")
load("data_pillar_1.RDATA")
load("LDA_overview.RDATA")



function(input, output) {
  
  output$red_flags <- renderUI({
    
    pass_grade <- 5.5
    high_grade <- 6.5
    
    augment_student_transcript <- function(transcript){
      
      transcript %>%
        
        mutate(
          fail       = grade < pass_grade,
          low        = grade < high_grade,
          grade_ceil = ceiling(grade)
        )
      
    }
    
    student <- list(
      
      # transcript of student: tibble with course and grade
      transcript = d_transcript %>%
        filter(
          `Student ID` == input$student
          ) %>%
        select(
          course = `Course ID`,
          grade  = Grade
          ),

      # interest of student (to be used with topic model)
      interest = c("key", "words", "related", "to", "topics")
      
    )

    
    
    # # fake student
    # n_course <- 25
    # 
    # student <- list(
    #   
    #   # transcript: tibble with course and grade
    #   transcript = tibble(
    #     
    #     course = sample(
    #       x       = course_all,
    #       size    = n_course,
    #       replace = FALSE
    #     ),
    #     
    #     grade = rnorm(
    #       n    = n_course,
    #       mean = 5,
    #       sd   = 2
    #     )
    #     
    #   ),
    #   
    #   # interest of student (to be used with topic model)
    #   interest = c("key", "words", "related", "to", "topics")
    #   
    # )
    
    
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
          " and ",
          round(confidence * 100, digits = 1),
          "% of the ",
          lhs.rhsTake.count,
          " students who have taken ", 
          rhs_course,
          " after obtaining a similar grade in ",
          lhs_course,
          " did not do well in ",
          rhs_course,
          ".",
          sep = ""
        )
        
      )
    
    if(nrow(red_flags) == 0){
      
      "No red flag"
      
    }else{
      
      red_flags  %>%
        
        pull %>%
        
        sort %>% 
        
        paste0(
          collapse = "<br/><br/>"
        ) %>% 
        
        HTML
    }
    
    
  })
  
  output$course_recommendation <- renderUI({
    
    
    #
    # Set up
    get_beta <- function(results){
      
      tidytext::tidy(results, matrix = "beta") %>%
        mutate(topic = paste("Topic", topic)) %>%
        arrange(topic, desc(beta))
      
    }
    
    get_gamma <- function(results){
      
      tidytext::tidy(results, matrix = "gamma") %>%
        mutate(topic = paste("Topic", topic)) %>%
        arrange(topic, desc(gamma))
      
    }
    
    beta_description <- lapply(
      LDA_model,
      get_beta
      )
    
    gamma_descriptions <- lapply(
      LDA_model,
      get_gamma
      )
    
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
    # Topic Score
    student$topic_score <- beta_description$k35 %>%
      filter(
        term %in% key_words
        ) %>%
      group_by(
        topic
        )%>% #maybe we can do this with transcripts afterwards
      summarize(
        topic_score = sum(beta)
        )%>%
      ungroup() %>%
      mutate(topic_score_proportion = topic_score / sum(topic_score))
    
    
    #
    # Course Score
    student$course_score <- gamma_descriptions$k35 %>%
      full_join(
        student$topic_score,
        by = "topic"
        ) %>%
      group_by(
        document
        ) %>%
      summarise(
        course_score = sum(gamma * topic_score_proportion)
        ) %>%
      ungroup
      
    
    #
    # Course recommendation
    student$course_score  %>%
      
      top_n(
        n  = 5,
        wt = course_score
        ) %>%
      
      left_join(
        select(d_course, `Course ID`, `Course Title`),
        by = c("document" = "Course ID")
        ) %>%
      
      transmute(
        recommendation = paste(
          "Course Recommendation", 
          document, `Course Title`,
          "because ..."
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