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
          ) %>%
        
        augment_student_transcript,

      # interest of student (to be used with topic model)
      interest = c("key", "words", "related", "to", "topics")
      
    )
    
    course_all <- unique(d_course$`Course ID`)
    
    student$course_not_taken <- setdiff(course_all, student$transcript$course)

    student$course_low <- student$transcript %>%
      filter(
        low
        ) %>%
      pull(
        course
        )
    
    student$course_fail <- student$transcript %>%
      filter(
        fail
      ) %>%
      pull(
        course
      )
    
    
    rule_not <- SR$THL %>%
      
      select(
        rhs_course,
        lhs_course,
        confidence,
        lhs.rhsTake.count
        ) %>%
      
      filter(
        rhs_course %in% input$course_chosen,
        lhs_course %in% student$course_not_taken
        ) %>%
      
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
          " did not do well in ",
          rhs_course,
          ".",
          sep = ""
        )
      )
    
    rule_low <- SR$HL %>%
      
      select(
        rhs_course,
        lhs_course,
        confidence,
        lhs.rhsTake.count
      ) %>%
      
      # select appropriate rules
      filter(
        rhs_course %in% input$course_chosen,
        lhs_course %in% student$course_low
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
          " after obtaining a similar grade in ",
          lhs_course,
          " did not do well in ",
          rhs_course,
          ".",
          sep = ""
          )
        )
    
    rule_fail <- SR$PF %>%
      
      select(
        rhs_course,
        lhs_course,
        confidence,
        lhs.rhsTake.count
      ) %>%
      
      # select appropriate rules
      filter(
        rhs_course %in% input$course_chosen,
        lhs_course %in% student$course_fail
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
          " did not do well in ",
          rhs_course,
          ".",
          sep = ""
        )
      )
    
    
    rules <- rbind(
      rule_not,
      rule_low,
      rule_fail
      ) %>%
      
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