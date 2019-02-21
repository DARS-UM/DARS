load("output/data_pillar_2.RDATA")


#
# Set up
 
library(topicmodels)
library(tidyverse)
library(tidytext)

d_overview <- d_overview %>%
  filter(
    `Academic Year` == "2018-2019"
  )

my_cast_tdm <- function(data, level) data %>%
  count(`Course ID`, word) %>%
  cast_dtm(`Course ID`, word, n)

d_overview_cast <- my_cast_tdm(d_overview)

my_control_individual <- list(
  
  nstart  = 10,
  seed    = c(1:10),
  best    = TRUE,
  
  burnin  = 500,
  iter    = 2000,
  thin    = 100,
  
  verbose = 1e2,
  save    = 1e3,
  keep    = 1e3
  )



#
# Fitting topic model

LDA_model <-list()

# LDA_model$k5 <- LDA(
#   x       = d_overview_cast,
#   k       = 5,
#   method  = "Gibbs",
#   control = my_control_individual
# )

LDA_model$k10 <- LDA(
  x       = d_overview_cast,
  k       = 10,
  method  = "Gibbs",
  control = my_control_individual
)

LDA_model$k20 <- LDA(
  x       = d_overview_cast,
  k       = 20,
  method  = "Gibbs",
  control = my_control_individual
)

LDA_model$k35 <- LDA(
  x       = d_overview_cast,
  k       = 35,
  method  = "Gibbs",
  control = my_control_individual
)
#Preparing for export

#Functions
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
#Applying functions
clean_models <- list()

clean_models$beta_distribution <- lapply(
  LDA_model,
  get_beta
)

clean_models$gamma_distribution <- lapply(
  LDA_model,
  get_gamma
)
  
#saving
save(clean_models,
  #LDA_model,
     file="App/Recommender System/LDA_overview.RDATA")