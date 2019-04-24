library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell) #for stemming
library(glmnet)   #for grade prediction


#
#General set up ---------------------------------------------------------------------------------
load("./App/Recommender System/data_pillar_1.RDATA")
load("./App/Recommender System/data_topic_models.RDATA") #contains app_model and full dataframe of topic models
load("./App/Recommender System/rules_clean.RDATA")
load("./App/Recommender System/grade_prediction.RDATA") 

###Set up
use_past <- T

# Helper function: Return student
student_courses <- function(d_student) {
  student_hist <- d_transcript %>%
    
    filter(
      `Student ID` == d_student
    ) %>%
    
    select(
      course = `Course ID`
    ) 
}

#Reactive student function 
student_past <- student_courses("6113335")


#recommendations
  beta_distribution  <- app_model$Beta[[1]] 
  gamma_distribution <- app_model$Gamma[[1]] 
  course_titles      <- app_model$`Course Titles`[[1]]
  
  # Key words
  key_words_additional <- c("student", "economic")
  
  
  stem_hunspell <- function(term) {
    
    stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
    n_stems <- length(stems)            # number of stems
    
    if (n_stems == 0) term              # if no stems, return original term
    else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem
    
  }
  
  
  key_words_additional <- sapply(key_words_additional, stem_hunspell)
  
  with_guess <- student_past%>% 
    left_join(gamma_distribution, by = c("course" = "document"))%>%
    group_by(topic) %>%
    summarise(topic_score = sum(gamma)) %>%
    ungroup()%>%
    arrange(desc(topic_score)) %>%
    top_n(5, topic_score) %>%
    left_join(beta_distribution, by = "topic") %>%
    group_by(term) %>%
    summarize(topic_score = sum(beta)) %>%
    top_n(100, topic_score) %>% pull(term)
    

  key_words            <- c(input$key_words, key_words_additional, key_words_guess)
  
  kw_select <- intersect(with_guess, app_model$kw[[1]])
  
  
  
  test <- T
  kw_select <- ifelse(test,intersect(with_guess, kw_used), NA)
  
  kw_select <- if(test){
    intersect(with_guess, kw_used)
  } else {
    NA
  }
  