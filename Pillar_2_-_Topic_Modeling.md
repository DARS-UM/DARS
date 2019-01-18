Pillar 2 - Topic Modeling
================
DARS
2019-01-18

-   [Setup](#setup)
-   [TF-IDF](#tf-idf)
    -   [Functions for Generating Barplots and Word Clouds](#functions-for-generating-barplots-and-word-clouds)
    -   [Course Level](#course-level)
    -   [Cluster Level](#cluster-level)
    -   [Concentration Level](#concentration-level)
-   [LDA](#lda)
    -   [Fitting Model](#fitting-model)
    -   [Visualization](#visualization)
        -   [Functions](#functions)
        -   [Plots](#plots)

``` r
library(tidyverse)
library(tidytext)

library(ggwordcloud) # Word Clouds
library(topicmodels)
library(lemon)
library(ggthemes)
library(rlang)
```

Setup
=====

``` r
load("data_pillar_2.RDATA")
```

TF-IDF
======

Functions for Generating Barplots and Word Clouds
-------------------------------------------------

``` r
compute_tf_idf <- function(data) data %>%
                                   count(`Course ID`, word) %>%
                                   bind_tf_idf(term = word, document = `Course ID`, n = n)

tf_idf_description <- compute_tf_idf(filter(d_description, `Academic Year` == "2018-2019"))
```

    ## Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    ## This warning is displayed once per session.

``` r
tf_idf_overview    <- compute_tf_idf(filter(d_overview, `Academic Year` == "2018-2019"))
tf_idf_manual      <- compute_tf_idf(d_manual)

rm(compute_tf_idf)
```

``` r
plot_tf_idf <- function(data, n_col = 5, id_plot = NULL){

  # Barplot
  g <- data %>%
    ggplot(aes(reorder_within(word, tf_idf, facet), tf_idf)) +
    geom_col(show.legend = F) +
    labs(x = NULL, y = "tf-idf") +
    scale_x_reordered() +
    facet_wrap(~ facet, scales = "free", ncol = n_col) +
    coord_flip()
  
  ggsave(paste(id_plot, "_BP.jpeg", sep = ""), path = "Plots", width = 16, height = 8)
    
  # Word Cloud
  g <- data %>%
    group_by(facet) %>%
      mutate(freq = tf_idf / sum(tf_idf)) %>% # normalization within clusters
    ggplot(aes(size = freq^0.7, label = word, color = freq^0.7)) +
    geom_text_wordcloud_area(area_corr_power = 1,
                             eccentricity    = 1,
                             rm_outside      = T) +
    scale_radius(range = c(2, 10), limits = c(0, NA)) +
    scale_color_gradient(low = "red", high = "blue") +
    facet_wrap(~ facet, ncol = n_col) + 
    theme(
      strip.text.x     = element_text(),
      panel.background = element_rect(fill = "white"),
      plot.title       = element_text(hjust=0.5)
    )
  
  ggsave(paste(id_plot, "_WC.jpeg", sep = ""), path = "Plots", width = 16, height = 8)

}
```

Course Level
------------

``` r
prepare_tf_idf_course <- function(data) data %>%
                                          filter(`Course ID` %in% sample(unique(.$`Course ID`), size = 25, replace = F)) %>% # random selection of courses
                                          rename(facet = `Course ID`) %>%
                                          group_by(facet) %>%
                                            filter(n >= 2) %>%
                                            top_n(10, tf_idf) %>%
                                          ungroup

set.seed(123)
tf_idf_description %>%
  prepare_tf_idf_course %>%
  plot_tf_idf(id_plot = "Course_description")
```

    ## Warning: `lang()` is soft-deprecated as of rlang 0.2.0.
    ## Please use `call2()` instead
    ## This warning is displayed once per session.

``` r
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
prepare_tf_idf_cluster <- function(data) data %>%
                                           left_join(select(d_course, `Course ID`, Cluster), by = "Course ID") %>%
                                           filter(!is.na(Cluster)) %>%
                                           rename(facet = Cluster) %>%
                                           group_by(facet, word) %>%
                                             summarise(tf_idf = sum(tf_idf),
                                                       n = sum(n)) %>%
                                             filter(n >= 10) %>%
                                             top_n(10, tf_idf) %>%
                                           ungroup

tf_idf_description %>%
  prepare_tf_idf_cluster %>%
  plot_tf_idf(id_plot = "Cluster_description")
```

    ## Warning: `new_overscope()` is soft-deprecated as of rlang 0.2.0.
    ## Please use `new_data_mask()` instead
    ## This warning is displayed once per session.

    ## Warning: `overscope_eval_next()` is soft-deprecated as of rlang 0.2.0.
    ## Please use `eval_tidy()` with a data mask instead
    ## This warning is displayed once per session.

    ## Warning: `chr_along()` is soft-deprecated as of rlang 0.2.0.
    ## This warning is displayed once per session.

``` r
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
prepare_tf_idf_concentration <- function(data) data %>%
                                                 left_join(select(d_course, `Course ID`, Concentration, `Concentration (additional)`), by = "Course ID") %>%
                                                 filter(!is.na(Concentration)) %>%
                                                 gather(X, Concentration, Concentration, `Concentration (additional)`, na.rm = TRUE) %>%
                                                 rename(facet = Concentration) %>%
                                                 group_by(facet, word) %>%
                                                   summarise(tf_idf = sum(tf_idf),
                                                             n = sum(n)) %>%
                                                   filter(n >= 10) %>%
                                                   top_n(10, tf_idf) %>%
                                                 ungroup

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

Fitting Model
-------------

``` r
my_cast_tdm <- function(data, level) data %>%
  count(`Course ID`, word) %>%
  cast_dtm(`Course ID`, word, n)

d_description_cast <- my_cast_tdm(d_description)
```

    ## Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    ## This warning is displayed once per session.

``` r
d_overview_cast <- my_cast_tdm(d_overview)
d_manual_cast <- my_cast_tdm(d_manual)

rm(my_cast_tdm)
```

``` r
LDA_description_10 <- LDA(d_description_cast, k = 10, control = list(seed = 123))
LDA_description_17 <- LDA(d_description_cast, k = 17, control = list(seed = 123))
LDA_description_25 <- LDA(d_description_cast, k = 25, control = list(seed = 123))

LDA_overview_10 <- LDA(d_overview_cast, k = 10, control = list(seed = 123))
LDA_overview_17 <- LDA(d_overview_cast, k = 17, control = list(seed = 123))
LDA_overview_25 <- LDA(d_overview_cast, k = 25, control = list(seed = 123))

LDA_manual_10 <- LDA(d_manual_cast, k = 10, control = list(seed = 123))
LDA_manual_17 <- LDA(d_manual_cast, k = 17, control = list(seed = 123))
LDA_manual_25 <- LDA(d_manual_cast, k = 25, control = list(seed = 123))
```

``` r
save(LDA_description_10, LDA_description_17, LDA_description_25,
     LDA_overview_10, LDA_overview_17, LDA_overview_25,
     LDA_manual_10, LDA_manual_17, LDA_manual_25,
     file = "LDA.RDATA")
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
LDA_description_10 %>% prepare_data_LDA_beta
```

    ## Warning: `lang()` is soft-deprecated as of rlang 0.2.0.
    ## Please use `call2()` instead
    ## This warning is displayed once per session.

    ## Warning: `new_overscope()` is soft-deprecated as of rlang 0.2.0.
    ## Please use `new_data_mask()` instead
    ## This warning is displayed once per session.

    ## # A tibble: 100 x 3
    ##    topic   term          beta
    ##    <chr>   <chr>        <dbl>
    ##  1 Topic 1 human      0.0171 
    ##  2 Topic 1 study      0.0132 
    ##  3 Topic 1 theory     0.0131 
    ##  4 Topic 1 psychology 0.0118 
    ##  5 Topic 1 social     0.0110 
    ##  6 Topic 1 cuss       0.0101 
    ##  7 Topic 1 gender     0.00878
    ##  8 Topic 1 process    0.00751
    ##  9 Topic 1 cell       0.00706
    ## 10 Topic 1 role       0.00609
    ## # ... with 90 more rows

``` r
# Gamma Distribution
LDA_description_10 %>% prepare_data_LDA_gamma(level = Cluster)
```

    ## Warning: `overscope_eval_next()` is soft-deprecated as of rlang 0.2.0.
    ## Please use `eval_tidy()` with a data mask instead
    ## This warning is displayed once per session.

    ## Warning: `chr_along()` is soft-deprecated as of rlang 0.2.0.
    ## This warning is displayed once per session.

    ## # A tibble: 101 x 3
    ##    facet               topic   gamma
    ##    <chr>               <chr>   <dbl>
    ##  1 Biomedical Sciences Topic 9 7.23 
    ##  2 Biomedical Sciences Topic 1 6.09 
    ##  3 Biomedical Sciences Topic 3 1.42 
    ##  4 Biomedical Sciences Topic 2 0.926
    ##  5 Biomedical Sciences Topic 6 0.224
    ##  6 Biomedical Sciences Topic 4 0.115
    ##  7 Business            Topic 6 2.00 
    ##  8 Business            Topic 8 2.00 
    ##  9 Business            Topic 3 2.00 
    ## 10 Business            Topic 7 1.85 
    ## # ... with 91 more rows

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
         width = 15, height = 10, path = "Plots")
  
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
         width = 15, height = 10, path = "Plots")
  
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
         width = 15, height = 10, path = "Plots")
    
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
LDA_description_10 %>% visualize_LDA_all_level(id_plot = "description_k10")
LDA_description_17 %>% visualize_LDA_all_level(id_plot = "description_k17")
LDA_description_25 %>% visualize_LDA_all_level(id_plot = "description_k25")

LDA_overview_10 %>% visualize_LDA_all_level(id_plot = "overview_k10")
LDA_overview_17 %>% visualize_LDA_all_level(id_plot = "overview_k17")
LDA_overview_25 %>% visualize_LDA_all_level(id_plot = "overview_k25")

LDA_manual_10 %>% visualize_LDA_all_level(id_plot = "manual_k10")
LDA_manual_17 %>% visualize_LDA_all_level(id_plot = "manual_k17")
LDA_manual_25 %>% visualize_LDA_all_level(id_plot = "manual_k25")
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
