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
load("./App/Recommender System/student_TP.RDATA")
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

student_topics <- function(d_student){
  
   student_TP %>% filter(`Student ID` == d_student) %>% top_n(1, time) %>%
    select(-time, -`Student ID`) %>%
    gather(key = topic, value = weight) %>%
    mutate(topic = str_replace(topic, "_", " "),
    weight = weight/sum(weight))
    
}

#Reactive student function 
student_past <- student_courses("6113335")

student_tp <- student_topics("6113335")

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

  kw_used <- app_model$kw[[1]] #is the same as in ui
  
  with_guess <- student_tp %>% 
    top_n(10, weight) %>%
    left_join(beta_distribution, by = "topic") %>%
    group_by(topic) %>%
    top_n(3, beta) %>% pull(term)
  
  # with_guess <- student_past%>% 
  #   left_join(gamma_distribution, by = c("course" = "document"))%>%
  #   group_by(topic) %>%
  #   summarise(topic_score = sum(gamma)) %>%
  #   ungroup()%>%
  #   arrange(desc(topic_score)) %>%
  #   top_n(5, topic_score) %>%
  #   left_join(beta_distribution, by = "topic") %>%
  #   group_by(term) %>%
  #   summarize(topic_score = sum(beta)) %>%
  #   top_n(100, topic_score) %>% pull(term)
    
  kw_select <- if(use_past){
    intersect(with_guess, kw_used)
  } else {
    NA
  }
  
  #test key words
  key_words <- c("economic", "science", "analysis", "equation", "data")
  
  #__________________________
  
  #Kl function
  
  
  
  my_kl <- function(distribution, reference) {
    
    kl.dist(reference, tibble(distribution), base = 2)[[1]]
    
  }
  
  #Student profile
  student <- list()
  student$topic_score <- beta_distribution %>%
    
    # Interest profile
    filter(
      term %in% key_words
    ) %>%
    group_by(
      topic
    ) %>%
    summarize(
      topic_score = sum(beta)
    ) %>%
    ungroup %>% ############### NORMALIZE
    mutate(student_interest = topic_score/sum(topic_score)) %>%
    select(-topic_score) %>% 
    full_join(gamma_distribution %>% 
                spread(key = document, value = gamma),
              by = "topic")
  
  #KL Distance
  ref <- student$topic_score %>% select(student_interest)
  
  kl_dist <- student$topic_score %>% 
    select(-topic) %>% 
    map_dbl(my_kl, reference = ref) %>%
    tibble(course = names(.), distance = .) %>%
    arrange((distance))
  

  #Recommendations
  recommendations <- kl_dist %>% 
   filter(course != "student_interest") %>%   
    # Recommendations
    top_n(
      n  = -20,
      wt = distance
    ) %>%
    
    # Key words per recommendation
    left_join(
      gamma_distribution,
      by = c("course" = "document")
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
      course
    ) %>%
    summarize(
      word_contribution_to_document = sum(gamma * beta)
    ) %>%
    
    group_by(
      course
    ) %>%
    top_n(
      n  = 3,
      wt = word_contribution_to_document
    ) %>%
    arrange(desc(word_contribution_to_document)) %>% 
    group_by(course) %>% 
    summarize(
      key_words = paste(term, collapse = ", "),
      doc_score = sum(word_contribution_to_document)
    ) %>%
    ungroup %>%
    
    # Editing
    left_join(
      course_titles,
      by = c("course" = "Course ID")
    ) %>%
    arrange(desc(doc_score))
  