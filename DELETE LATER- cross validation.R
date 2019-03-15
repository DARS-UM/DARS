
dtm <- AssociatedPress

#
##crossv_sets
crossv_sets <- function(dtm){
  #Setup
  n_documents <- nrow(dtm)
  folding <- sample(rep(1:10, ceiling(n_documents))[seq_len(n_documents)]) #not very elegant
  
  #functions
  create_trainset <- function(Fold){
    train_set = dtm[folding != Fold,]
  }
  
  create_testset <- function(Fold){
    test_set  = dtm[folding == Fold,]
  }
  
  #create folds
  folds <-  data.frame(Fold = 1:10) %>%
    mutate(train_set = map(Fold, .f = create_trainset),
           test_set  = map(Fold, .f = create_testset))
  #
  ##Output
  return(folds)
}

#
##Testing Application:
a_test <- d_cast$overview[1:10,]

a <- crossv_sets(a_test)
  
b <- a %>% 
  
  mutate(n_topic = 2) %>%
  
  mutate(model = list(n_topic, train_set) %>% pmap(my_LDA, control = my_control_fast))

#
## Developing next function
crossv_model <-  function(sets_dtm) {
  b <- sets_dtm %>%
    mutate(n_topic = 2) %>% #we want 
}






topic_model <- tibble(
  
  n_topic = seq(from = 2, to = 80, by = 2)
  
) %>%
  
  mutate(
    TM_overview = n_topic %>% map( .f = my_LDA, corpus = d_cast$overview),
    TM_manual   = n_topic %>% map( .f = my_LDA, corpus = d_cast$manuals)
  )




save(d_course, d_transcript, file = "tmp_import_APP.RDATA")