library(tidyverse)
library(tidytext)
library(stm)
library(janeaustenr)

tidy_austen <- austen_books() %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
#> Joining, by = "word"

austen_sparse <- tidy_austen %>%
  count(line, word) %>%
  cast_sparse(line, word, n)

## notice that I am scrambling the order of the rows
## the covariate data is in random order now
austen_covariates <- tidy_austen %>%
  sample_frac() %>%
  distinct(line, book)

austen_topic_model <- stm(austen_sparse, 
                   K = 12, 
                   prevalence = ~ book,
                   data = austen_covariates,
                   verbose = FALSE, 
                   init.type = "Spectral")
