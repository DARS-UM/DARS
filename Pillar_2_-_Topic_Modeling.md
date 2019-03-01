Pillar 2 - Topic Modeling
================
DARS
2019-02-27

-   [Setup](#setup)
-   [TF-IDF](#tf-idf)
    -   [Functions for Generating Barplots and Word Clouds](#functions-for-generating-barplots-and-word-clouds)
    -   [Course Level](#course-level)
    -   [Cluster Level](#cluster-level)
    -   [Concentration Level](#concentration-level)
-   [LDA](#lda)
    -   [Ideal Number of Topics (RUN ON UM COMPUTER)](#ideal-number-of-topics-run-on-um-computer)
    -   [Fitting Model](#fitting-model)
    -   [Visualization](#visualization)
        -   [Functions](#functions)
        -   [Plots](#plots)

``` r
knitr::opts_chunk$set(cache.path = "Cache/Pillar 2/")
```

``` r
library(tidyverse)
library(tidytext)

library(ggwordcloud) # Word Clouds
library(topicmodels)
library(lemon)
library(ggthemes)
library(rlang)
library(ldatuning) # ideal number of topics in LDA
```

Setup
=====

``` r
load("output/data_pillar_2.RDATA")

# Only keep course descriptions and overviews from most recent year
d_description <- d_description %>%
  filter(
    `Academic Year` == "2018-2019"
  )

d_overview <- d_overview %>%
  filter(
    `Academic Year` == "2018-2019"
  )
```

TF-IDF
======

Functions for Generating Barplots and Word Clouds
-------------------------------------------------

``` r
compute_tf_idf <- function(data){
  
  data %>%
    count(
      `Course ID`,
      word
      ) %>%
    bind_tf_idf(
      term = word, 
      document = `Course ID`,
      n = n
      )
  
}

tf_idf_description <- compute_tf_idf(d_description)
tf_idf_overview    <- compute_tf_idf(d_overview)
tf_idf_manual      <- compute_tf_idf(d_manual)

rm(compute_tf_idf)
```

``` r
plot_tf_idf <- function(data, n_col = 5, id_plot = NULL){
  
  
  #
  # Barplot
  g <- data %>%
    ggplot(
      aes(
        x = reorder_within(word, tf_idf, facet), 
        y = tf_idf
        )
      ) +
    geom_col(
      show.legend = FALSE
      ) +
    labs(
      x = NULL,
      y = "tf-idf"
      ) +
    scale_x_reordered() +
    facet_wrap(
      facets = ~ facet, 
      scales = "free",
      ncol = n_col
      ) +
    coord_flip()
  
  ggsave(paste(id_plot, "_BP.jpeg", sep = ""), path = "Output/Plots/tf_idf", width = 16, height = 8)
  
  
  #
  # Word Cloud
  g <- data %>%
    group_by(
      facet
      ) %>%
    mutate(
      freq = tf_idf / sum(tf_idf)
      ) %>% # normalization within facet
    ggplot(
      aes(
        size = freq ^ 0.7, 
        label = word, 
        color = freq ^ 0.7
        )
      ) +
    geom_text_wordcloud_area(
      area_corr_power = 1,
      eccentricity    = 1,
      rm_outside      = T
      ) +
    scale_radius(
      range = c(2, 10),
      limits = c(0, NA)
      ) +
    scale_color_gradient(
      low = "red", 
      high = "blue"
      ) +
    facet_wrap(
      facet = ~ facet,
      ncol = n_col
      ) + 
    theme(
      strip.text.x     = element_text(),
      panel.background = element_rect(fill = "white"),
      plot.title       = element_text(hjust = 0.5)
    )
  
  ggsave(paste(id_plot, "_WC.jpeg", sep = ""), path = "Output/Plots/tf_idf", width = 16, height = 8)

}
```

Course Level
------------

``` r
prepare_tf_idf_course <- function(data){
  
  data %>%
    
    # Selection 25 courses randomly
    filter(
      `Course ID` %in% sample(
        x = unique(.$`Course ID`),
        size = 25, 
        replace = FALSE
        )
      ) %>%
    
    # Select top 10 words per course 
    group_by(
      `Course ID`
      ) %>%
    filter(
      n >= 2
      ) %>%
    top_n(
      n = 10,
      wt = tf_idf
      ) %>%
    ungroup %>%
    
    # Prepare data for function `plot_tf_idf()`
    rename(
      facet = `Course ID`
      )

  }

set.seed(123)
tf_idf_description %>%
  prepare_tf_idf_course %>%
  plot_tf_idf(id_plot = "Course_description")

set.seed(123)
tf_idf_overview %>%
  prepare_tf_idf_course %>%
  plot_tf_idf(id_plot = "Course_overview")

set.seed(123)
tf_idf_manual %>%
  prepare_tf_idf_course %>%
  plot_tf_idf(id_plot = "Course_manual")
```

    ## One word could not fit on page. It has been removed.

``` r
rm(prepare_tf_idf_course)
```

Cluster Level
-------------

``` r
prepare_tf_idf_cluster <- function(data){
  
  data %>%
    
    # Include the variable Cluster
    left_join(
      select(d_course, `Course ID`, Cluster),
      by = "Course ID"
      ) %>%
    filter(
      !is.na(Cluster)
      ) %>%
    
    # Select top 10 words per cluster
    group_by(
      Cluster, 
      word
      ) %>%
    summarise(
      tf_idf = sum(tf_idf),
      n = sum(n)
      ) %>%
    filter(
      n >= 10
      ) %>%
    top_n(
      n = 10, 
      wt = tf_idf
      ) %>%
    ungroup %>%
    
    # Prepare data for function `plot_tf_idf()`
    rename(
      facet = Cluster
      )
  
}

tf_idf_description %>%
  prepare_tf_idf_cluster %>%
  plot_tf_idf(id_plot = "Cluster_description")

tf_idf_overview %>%
  prepare_tf_idf_cluster %>%
  plot_tf_idf(id_plot = "Cluster_overview")

tf_idf_manual %>%
  prepare_tf_idf_cluster %>%
  plot_tf_idf(id_plot = "Cluster_manual")

rm(prepare_tf_idf_cluster)
```

Concentration Level
-------------------

``` r
prepare_tf_idf_concentration <- function(data){
  
  data %>%
    
    # Include the variable Concentration
    left_join(
      select(d_course, `Course ID`, Concentration, `Concentration (additional)`),
      by = "Course ID"
      ) %>%
    filter(
      !is.na(Concentration)
      ) %>%
    gather(
      key = X,
      value = Concentration,
      Concentration, `Concentration (additional)`,
      na.rm = TRUE
      ) %>%
    
    # Select top 10 words per cluster
    group_by(
      Concentration, 
      word
      ) %>%
    summarise(
      tf_idf = sum(tf_idf),
      n = sum(n)
      ) %>%
    filter(
      n >= 10
      ) %>%
    top_n(
      n = 10,
      wt = tf_idf
      ) %>%
    ungroup %>%
    
    # Prepare data for function `plot_tf_idf()`
    rename(
      facet = Concentration
      )
  
} 

tf_idf_description %>%
  prepare_tf_idf_concentration %>%
  plot_tf_idf(id_plot = "Concentration_description")

tf_idf_overview %>%
  prepare_tf_idf_concentration %>%
  plot_tf_idf(id_plot = "Concentration_overview")

tf_idf_manual %>%
  prepare_tf_idf_concentration %>%
  plot_tf_idf(id_plot = "Concentration_manual")

rm(prepare_tf_idf_concentration, plot_tf_idf,
   tf_idf_description, tf_idf_overview, tf_idf_manual)
```

LDA
===

``` r
my_cast_tdm <- function(data, level) data %>%
  count(`Course ID`, word) %>%
  cast_dtm(`Course ID`, word, n)

d_description_cast <- my_cast_tdm(d_description)
d_overview_cast <- my_cast_tdm(d_overview)
d_manual_cast <- my_cast_tdm(d_manual)

rm(my_cast_tdm)
```

Ideal Number of Topics (RUN ON UM COMPUTER)
-------------------------------------------

``` r
my_control <- list(
  
  nstart = 10,
  seed   = 1 : 10,
  best   = TRUE,
  
  burnin = 100,
  iter   = 5000,
  thin   = 100
  
)
```

``` r
my_FindTopicsNumber <- function(data_cast, n_topics){
  
  FindTopicsNumber(
  
  dtm = data_cast,
  topics = n_topics,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = my_control,
  mc.cores = 4L
  
  )
  
  
}
```

``` r
start.time <- Sys.time()

result_description <- d_description_cast %>% my_FindTopicsNumber(n_topics = seq(from = 5, to = 100, by = 2))

end.time <- Sys.time()
time.taken <- end.time - start.time

save(result_description,
     file = "Output/LDA_ntopics.RDATA")

print(time.taken)
```

    ## Time difference of 3.050568 hours

``` r
start.time <- Sys.time()

result_overview <- d_overview_cast %>% my_FindTopicsNumber(n_topics = seq(from = 5, to = 100, by = 2))

end.time <- Sys.time()
time.taken <- end.time - start.time

save(result_overview , result_description,
     file = "Output/LDA_ntopics.RDATA")

print(time.taken)
```

    ## Time difference of 6.106553 hours

``` r
start.time <- Sys.time()

result_manual <- d_manual_cast %>% my_FindTopicsNumber(n_topics = seq(from = 5, to = 75, by = 5))

end.time <- Sys.time()
time.taken <- end.time - start.time

save(result_manual, result_overview , result_description,
     file = "Output/LDA_ntopics.RDATA")

print(time.taken)
```

    ## Time difference of 22.44461 hours

from 20, to 40, by 5 topics takes 22min for overview 11min for description 5.5hrs for manuals

``` r
load("Output/LDA_ntopics.RDATA")

result_description %>%
  select(-Griffiths2004) %>%
  FindTopicsNumber_plot
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/LDA%20number%20topics%20plot-1.png)

``` r
result_overview %>%
  select(-Griffiths2004) %>%
  FindTopicsNumber_plot
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/LDA%20number%20topics%20plot-2.png)

``` r
result_manual %>%
  select(-Griffiths2004) %>%
  FindTopicsNumber_plot
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/LDA%20number%20topics%20plot-3.png)

Fitting Model
-------------

``` r
n_topics <- c(35, 35, 40)
levels   <- c("description", "overview", "manual")
list_nam <- character(0)

for(i in seq_along(n_topics)){
  
  dataset   <- get(paste("d", levels[i], "cast", sep = "_"))
  
  LDA_model <- LDA(
    x       = dataset,
    k       = n_topics[i],
    method  = "Gibbs",
    control = my_control
    )
  
  nam <- paste("LDA", levels[i], n_topics[i], sep = "_")
  
  assign(
    x     = nam,
    value = LDA_model
  )
  
  list_nam <- c(list_nam, nam)
  
}

save(list = list_nam,
     file = "Output/topic_model.RDATA")

save(list = list_nam,
     file = "App/Recommender System/topic_model.RDATA")
```

Visualization
-------------

### Functions

``` r
prepare_data_LDA_beta <- function(results){
  
  tidy(results, matrix = "beta") %>%
    mutate(topic = paste("Topic", topic)) %>%
      group_by(topic) %>%
        top_n(10, beta) %>%
      ungroup %>%
      arrange(topic, desc(beta))
  
}

prepare_data_LDA_gamma <- function(results, level = Course){
  
  data_gamma <- tidy(results, matrix = "gamma") %>%
    mutate(topic = paste("Topic", topic)) %>%
    rename(`Course ID` = document)
  
  if(ensym(level) == sym("Course")) data_gamma <- data_gamma %>%
                                      rename(facet = `Course ID`) 
  
  if(ensym(level) == sym("Cluster")) data_gamma <- data_gamma %>% 
                                       left_join(select(d_course, `Course ID`, Cluster), by = "Course ID") %>%
                                       filter(!is.na(Cluster)) %>%
                                       rename(facet = Cluster)
                                       
  if(ensym(level) == sym("Concentration")) data_gamma <- data_gamma %>%
                                             left_join(select(d_course, `Course ID`, Concentration, `Concentration (additional)`), by = "Course ID") %>%
                                             gather(X, Concentration, Concentration, `Concentration (additional)`, na.rm = TRUE) %>%
                                             rename(facet = Concentration)
  
  data_gamma <- data_gamma %>%
    group_by(facet, topic) %>%
      summarise(gamma = sum(gamma)) %>%
    filter(gamma > 0.05) %>%
    ungroup %>%
    arrange(facet, desc(gamma))
  
  return(data_gamma)
  
}

# Bet Distribution
LDA_description_120 %>% prepare_data_LDA_beta

# Gamma Distribution
LDA_description_120 %>% prepare_data_LDA_gamma(level = Cluster)
```

``` r
visualize_LDA_beta <- function(data_prepared, id_plot = "test"){
  
  data_prepared %>%
  ggplot(aes(x = reorder_within(term,
                                by = beta,
                                within = topic),
             y = beta,
             fill = topic)) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free") +
    scale_x_reordered() +
    labs(x = "Terms", y = "Beta", title = "Main Terms of each Topic") +
    coord_flip() +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste(id_plot, "beta.jpeg"),
         width = 16, height = 10, path = "Plots/Topic Model")
  
}

visualize_LDA_gamma1 <- function(data_prepared, id_plot = "test"){
  
  data_prepared %>%
    ggplot(aes(reorder_within(facet, 
                              by = gamma, 
                              within = topic), 
               y = gamma, 
               fill = topic)) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, 
               scales = "free") +
    scale_x_reordered() +
    coord_flip() +
   # labs(x = ensym(facet), y = "Gamma", title = paste("Main", ensym(facet), "of each Topic") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text(hjust = 0.5))
  
  ggsave(paste(id_plot, "gamma1.jpeg"),
         width = 16, height = 10, path = "Plots/Topic Model")
  
}

visualize_LDA_gamma2 <- function(data_prepared, id_plot = "test"){
  
  data_prepared %>%
    ggplot(aes(reorder_within(topic, 
                              by = gamma,
                              within = facet), 
               y = gamma, 
               fill = topic)) +
    geom_col(show.legend = F) +
    facet_wrap(~ facet, 
               scales = "free") +
    scale_x_reordered() +
    coord_flip() +
  #  labs(x = "Topics", y = "Gamma", title = "Main Topics of each Courses/Cluster") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text(hjust = 0.5))
  
  ggsave(paste(id_plot, "gamma2.jpeg"),
         width = 16, height = 10, path = "Plots/Topic Model")
    
}
```

``` r
visualize_LDA_all_distrib <- function(data, level = Course, id_plot = "test"){

  # Beta distribution
  data %>% 
    prepare_data_LDA_beta %>%
    visualize_LDA_beta(id_plot = id_plot)
  
  # Gamma 1 distribution
  data %>% 
    prepare_data_LDA_gamma(level = !!ensym(level)) %>%
    visualize_LDA_gamma1(id_plot = id_plot)
  
  # Gamma 2 distribution
  data %>% 
    prepare_data_LDA_gamma(level = !!ensym(level)) %>%
    visualize_LDA_gamma2(id_plot = id_plot)
  
}
```

``` r
visualize_LDA_all_level <- function(data, id_plot = "test"){
  
  data %>% 
    visualize_LDA_all_distrib(level = Course,
                              id_plot = paste(id_plot, "_course"))
  
  data %>% 
    visualize_LDA_all_distrib(level = Cluster,
                              id_plot = paste(id_plot, "_cluster"))
    
  data %>% 
    visualize_LDA_all_distrib(level = Concentration,
                              id_plot = paste(id_plot, "_concentration"))
  
}
```

### Plots

``` r
load("LDA.RDATA")

LDA_description_5 %>% visualize_LDA_all_level(id_plot = "description_k5")

LDA_overview_5 %>% visualize_LDA_all_level(id_plot = "overview_k5")

LDA_manual_5 %>% visualize_LDA_all_level(id_plot = "manual_k5")
```

``` r
tidy(LDA_manual_10, "beta") %>%
  filter(beta > 1e-3) %>%
  bind_tf_idf(term = term, document = topic, n = beta) %>%
  group_by(topic) %>%
  top_n(10, tf_idf) %>%
  arrange(topic, desc(tf_idf))


  count(term, topic) %>%
  arrange(desc(n)) %>%
    
  filter(beta > 1e-3) %>%
  group_by(term) %>%
    mutate(n_topic = n(),
           beta = beta / n_topic) %>%
  group_by(topic) %>%
    top_n(10, beta)


  count(topic, term, wt = beta) %>%
  (term = term, document = topic, n = n) %>%
  group_by(topic) %>%
  top_n(10, tf_idf)


  group_by(term) %>%
  summarise(beta = sum(beta)) %>%
  arrange(desc(beta))



  


labels <- list(
  description_10 = "X"
)
```

``` r
visualize_LDA(LDA_cluster_10, id_plot = "Cluster, k = 10 with labelled topics", facet = T,
              topic_names = c("Psychology & Information", "Economics and Development", "Biology", "Chemistry and Math",
                              "Sociology", "Law", "Research", "International Politics",
                              "Philosophy", "Humanities"))

visualize_LDA(LDA_cluster_17, id_plot = "Cluster, k = 17 with labelled topics", facet = T,
              topic_names = c("Science/Knowledge", "Psycho", "Biology", "Chemistry", "Culture",
                              "Conflict", "Research", "Foreign Policy", "Arts", "Literature",
                              "Communication Skills", "Law", "Humanities", "Computer", "Economics", 
                              "Math", "Management and ?"))
visualize_LDA(LDA_cluster_25, id_plot = "Cluster, k = 25 with labelled topics", facet = T,
              topic_names = c(
                "Information", "Psycho", "Bio", "Chem", "Sociology",
                "History", "skills", "Europe", "Philosophy", "IR",
                "Conference", "Law", "Media", "Philosophy", "Economics",
                "Math", "Management and Literature", "Entrepreneurship", "Conflict", "Research",
                "Arts", "Computer Science", "Policy?", "Physiology", "Qual. Res."))
```
