load("output/data_pillar_2.RDATA")

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


LDA_model <- LDA(
  x       = d_overview_cast,
  k       = 5,
  method  = "Gibbs",
  control = list(
    
    nstart  = 10,
    seed    = c(1:10),
    best    = TRUE,
    
    burnin  = 500,
    iter    = 2000,
    thin    = 100,
    
    verbose = 1e2,
    save    = 1e3,
    keep    = 1e3)
)

save(LDA_model,file="Output/LDA_overview_5.RDATA")