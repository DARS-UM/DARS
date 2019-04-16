library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell)

load("rules.RDATA")
load("data_pillar_1.RDATA")
load("data_topic_models.RDATA") #contains distribution, kw, course_all, course_following_semester.
load("app_model.RDATA")



function(input, output, session) {
  #
  ##Set up
  
  #Raw Data for RS:
  raw_rules <- rules_clean %>%
    select(-rules_rules)   %>%
    filter(type_rule == "SR")
  
  #Helper function: return tibble of type_rules
  get_type_rules <-  function(type){
    tmp <- raw_rules %>%
      filter(ID == type) %>%
      pull(rules_RS)
    
    return(tmp[[1]])
  }
  
  #Helper: resettabe input
  output$resetable_input <- renderUI({
    times <- input$reset_input
    div(id=letters[(times %% length(letters)) + 1],
        checkboxGroupInput(
          inputId  = "course_chosen",
          label    = "Tentative Courses for following period",
          choices  = course_all,
          #selected = course_all,
          inline   = TRUE
        ))
  })
  
  
  # Helper function: Return student
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
  output$red_flags <- renderTable({
    #student
    student <- current_student()
    
    # rules
    rules <- list()
    
    # rules not
    rules$not <- get_type_rules("THL") %>%
      
      # select appropriate rules
      filter(
        lhs_course %in% student$course_not_taken,
        rhs_course %in% input$course_chosen
        ) %>%
      
      # provide explanation
      # mutate(
      #   explanation = paste(
      #     "Red flag for ",
      #     rhs_course,
      #     ":<br/>",
      #     "You have not taken ",
      #     lhs_course,
      #     " and ",
      #     round(confidence * 100, digits = 1),
      #     "% of the ",
      #     lhs.rhsTake.count,
      #     " students who have taken ", 
      #     rhs_course,
      #     " without first taking ",
      #     lhs_course,
      #     " obtained less than 6.5/10 in  ",
      #     rhs_course,
      #     ".",
      #     sep = ""
      #   )
      # )
    mutate(
      `Red Flag` = rhs_course,
      Reason = paste(
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
    rules$low <- get_type_rules("HL") %>%
      
      filter(
        lhs_course %in% student$course_low,
        rhs_course %in% input$course_chosen
        ) %>%
      
      # mutate(
      #   explanation = paste(
      #     "Red flag for ",
      #     rhs_course,
      #     ":<br/>",
      #     "You obtained less than 6.5/10 in ",
      #     lhs_course,
      #     " and ",
      #     round(confidence * 100, digits = 1),
      #     "% of the ",
      #     lhs.rhsTake.count,
      #     " students who have taken ", 
      #     rhs_course,
      #     " after obtaining less than 6.5/10 in ",
      #     lhs_course,
      #     " obtained less than 6.5/10 in  ",
      #     rhs_course,
      #     ".",
      #     sep = ""
      #     )
      #   )
    mutate(
      `Red Flag` = rhs_course,
      Reason = paste(
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
    rules$fail <- get_type_rules("PF") %>%
      
      filter(
        lhs_course %in% student$course_fail,
        rhs_course %in% input$course_chosen
        ) %>%
      
      # mutate(
      #   explanation = paste(
      #     "Red flag for ",
      #     rhs_course,
      #     ":<br/>",
      #     "You have failed ",
      #     lhs_course,
      #     " and ",
      #     round(confidence * 100, digits = 1),
      #     "% of the ",
      #     lhs.rhsTake.count,
      #     " students who have taken ", 
      #     rhs_course,
      #     " after failing ",
      #     lhs_course,
      #     " also failed ",
      #     rhs_course,
      #     ".",
      #     sep = ""
      #   )
      # )
    mutate(
      `Red Flag` = rhs_course,
      Reason = paste(
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
    rules$grade <- get_type_rules("G") %>%
      
      left_join(
        student$transcript,
        by = c("lhs_course" = "course")
        ) %>%
      
      # select appropriate rules
      filter(
        grade <= lhs_outcome, # lhs
        rhs_course %in% input$course_chosen
      ) %>%
      
      # mutate(
      #   explanation = paste(
      #     "Red flag for ",
      #     rhs_course,
      #     ":<br/>",
      #     "You have obtained less than ",
      #     lhs_outcome,
      #     "/10 in ",
      #     lhs_course,
      #     " and ",
      #     round(confidence * 100, digits = 1),
      #     "% of the ",
      #     lhs.rhsTake.count,
      #     " students who have taken ", 
      #     rhs_course,
      #     " after obtaining less than ",
      #     lhs_outcome,
      #     "/10 in ",
      #     lhs_course,
      #     " obtained less than ",
      #     rhs_outcome,
      #     "/10 in ",
      #     rhs_course,
      #     ".",
      #     sep = ""
      #   )
      # )
    mutate(
      `Red Flag` = rhs_course,
      Reason = paste(
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
            `Red Flag`,
            Reason
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
    
    if(nrow(student$transcript)==0){
      #"ERROR: id not found- we need new students ID's before running this"
      tibble(ERROR = "Student ID not found")
    }else if(nrow(rules) == 0){
      
      tibble(`Red Flag` = "No red flag")
      
    }else{
      
      rules  %>%
        
        select(`Red Flag`, Reason) #%>%
        
        #sort %>% 
        # 
        # paste0(
        #   collapse = "<br/><br/>" # skip a line between individual red flags
        # ) %>% 
        # 
        # HTML
      
    }
    
    
  })
  
  #
  ##COURSE RECOMMENDATION
  ###Set up
  recommendations_data <- reactive({
    
    beta_distribution <- app_model$Beta[[1]]   # 55  #***********************************************SELECT: overview/manual
    gamma_distribution <- app_model$Gamma[[1]] # 55  #***********************************************SELECT: overview/manual
    course_titles <- app_model$`Course Titles`[[1]]
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
    
    #Student profiles
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
      
      arrange(desc(course_score)) %>%
      
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
      arrange(desc(word_contribution_to_document)) %>% 
      group_by(document) %>% 
      summarize(
        key_words = paste(term, collapse = ", "),
        doc_score = sum(word_contribution_to_document)
      ) %>%
      ungroup %>%
      
      # Editing
      left_join(
        course_titles,
        by = c("document" = "Course ID")
      ) %>%
      arrange(desc(doc_score))})
  
  ##OUTPUTS
  ###Table
  output$course_recommendation <- renderTable({
    
    recommendations_data() %>%

      transmute(
        Code = document,
        Course = `Course Title`,
        `because you selected` = key_words
        )
    
    
  })
  

}