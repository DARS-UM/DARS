Pillar 2 - Topic Modeling
================
DARS
2019-01-16

-   [Setup](#setup)
-   [TF-IDF](#tf-idf)
    -   [Functions for Generating Barplots and Word Clouds](#functions-for-generating-barplots-and-word-clouds)
    -   [Course Level](#course-level)
    -   [Cluster Level](#cluster-level)
    -   [Concentration Level](#concentration-level)
-   [LDA](#lda)
    -   [Fitting Model](#fitting-model)
    -   [Results](#results)

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
                                   count(Code, word) %>%
                                   bind_tf_idf(term = word, document = Code, n = n)

tf_idf_description <- compute_tf_idf(filter(d_description, `Calendar Year` == "2018-2019"))
```

    ## Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    ## This warning is displayed once per session.

``` r
tf_idf_overview    <- compute_tf_idf(filter(d_overview, `Calendar Year` == "2018-2019"))
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
  print(g)
    
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
  print(g)

}
```

Course Level
------------

``` r
prepare_tf_idf_course <- function(data) data %>%
                                          filter(Code %in% sample(unique(.$Code), size = 25, replace = F)) %>% # random selection of courses
                                          rename(facet = Code) %>%
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

    ## Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    ## This warning is displayed once per session.

<img src="Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf course-1.png" width="16" height="8" />

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

    ## One word could not fit on page. It has been removed.

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

    ## One word could not fit on page. It has been removed.

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

<img src="Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf course-2.png" width="16" height="8" />

``` r
set.seed(123)
tf_idf_overview %>%
  prepare_tf_idf_course %>%
  plot_tf_idf(id_plot = "Course_overview")
```

<img src="Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf course-3.png" width="16" height="8" />

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

<img src="Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf course-4.png" width="16" height="8" />

``` r
set.seed(123)
tf_idf_manual %>%
  prepare_tf_idf_course %>%
  plot_tf_idf(id_plot = "Course_manual")
```

    ## One word could not fit on page. It has been removed.

<img src="Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf course-5.png" width="16" height="8" />

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

<img src="Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf course-6.png" width="16" height="8" />

``` r
rm(prepare_tf_idf_course)
```

Cluster Level
-------------

``` r
prepare_tf_idf_cluster <- function(data) data %>%
                                           left_join(select(d_course, Code, Cluster), by = "Code") %>%
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

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20cluster-1.png)

    ## Some words could not fit on page. They have been removed.

    ## One word could not fit on page. It has been removed.

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

    ## One word could not fit on page. It has been removed.

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

    ## One word could not fit on page. It has been removed.
    ## One word could not fit on page. It has been removed.

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

    ## One word could not fit on page. It has been removed.

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20cluster-2.png)

``` r
tf_idf_overview %>%
  prepare_tf_idf_cluster %>%
  plot_tf_idf(id_plot = "Cluster_overview")
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20cluster-3.png)

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20cluster-4.png)

``` r
tf_idf_manual %>%
  prepare_tf_idf_cluster %>%
  plot_tf_idf(id_plot = "Cluster_manual")
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20cluster-5.png)

    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.
    ## Some words could not fit on page. They have been removed.

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20cluster-6.png)

``` r
rm(prepare_tf_idf_cluster)
```

Concentration Level
-------------------

``` r
prepare_tf_idf_concentration <- function(data) data %>%
                                                 left_join(select(d_course, Code, Concentration, `Concentration (additional)`), by = "Code") %>%
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
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20concentration-1.png)![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20concentration-2.png)

``` r
tf_idf_overview %>%
  prepare_tf_idf_concentration %>%
  plot_tf_idf(id_plot = "Concentration_overview")
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20concentration-3.png)![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20concentration-4.png)

``` r
tf_idf_manual %>%
  prepare_tf_idf_concentration %>%
  plot_tf_idf(id_plot = "Concentration_manual")
```

![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20concentration-5.png)![](Pillar_2_-_Topic_Modeling_files/figure-markdown_github/tf-idf%20concentration-6.png)

``` r
rm(prepare_tf_idf_concentration, plot_tf_idf,
   tf_idf_description, tf_idf_overview, tf_idf_manual)
```

LDA
===

Fitting Model
-------------

``` r
my_cast_tdm <- function(data, level) data %>%
  count(Code, word) %>%
  cast_dtm(Code, word, n)

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

Results
-------

``` r
prepare_data_LDA_beta <- function(results) tidy(results, matrix = "beta") %>%
                                        mutate(topic = paste("Topic", topic)) %>%
                                          group_by(topic) %>%
                                            top_n(10, beta) %>%
                                          ungroup %>%
                                          arrange(topic, desc(beta))

prepare_data_LDA_gamma <- function(results, level = Course){
  
  data_gamma <- tidy(results, matrix = "gamma") %>%
    mutate(topic = paste("Topic", topic)) %>%
    rename(Code = document)
  
  if(ensym(level) == sym("Course")) data_gamma <- data_gamma %>%
                                      rename(facet = Code) 
  
  if(ensym(level) == sym("Cluster")) data_gamma <- data_gamma %>% 
                                       left_join(select(d_course, Code, Cluster), by = "Code") %>%
                                       filter(!is.na(Cluster)) %>%
                                       rename(facet = Cluster)
                                       
  if(ensym(level) == sym("Concentration")) data_gamma <- data_gamma %>%
                                             left_join(select(d_course, Code, Concentration, `Concentration (additional)`), by = "Code") %>%
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
    ##    topic   term             beta
    ##    <chr>   <chr>           <dbl>
    ##  1 Topic 1 student       0.0212 
    ##  2 Topic 1 develop       0.0154 
    ##  3 Topic 1 human         0.0144 
    ##  4 Topic 1 cuss          0.0141 
    ##  5 Topic 1 public        0.0140 
    ##  6 Topic 1 international 0.0137 
    ##  7 Topic 1 model         0.0130 
    ##  8 Topic 1 heal          0.0105 
    ##  9 Topic 1 meet          0.00889
    ## 10 Topic 1 issue         0.00872
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

    ## # A tibble: 104 x 3
    ##    facet               topic     gamma
    ##    <chr>               <chr>     <dbl>
    ##  1 Biomedical Sciences Topic 8  8.02  
    ##  2 Biomedical Sciences Topic 1  3.62  
    ##  3 Biomedical Sciences Topic 5  2.09  
    ##  4 Biomedical Sciences Topic 7  1.96  
    ##  5 Biomedical Sciences Topic 2  0.136 
    ##  6 Biomedical Sciences Topic 3  0.0969
    ##  7 Biomedical Sciences Topic 4  0.0783
    ##  8 Business            Topic 4  3.08  
    ##  9 Business            Topic 10 1.91  
    ## 10 Business            Topic 5  1.84  
    ## # ... with 94 more rows

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
               fill = facet)) +
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
# Bet Distribution
LDA_description_10 %>%
  prepare_data_LDA_beta %>%
  visualize_LDA_beta

# Gamma Distribution 1
LDA_description_10 %>%
  prepare_data_LDA_gamma(level = Cluster) %>%
  visualize_LDA_gamma1

# Gamma Distribution 2
LDA_description_10 %>%
  prepare_data_LDA_gamma(level = Cluster) %>%
  visualize_LDA_gamma2
```

``` r
visualize_LDA <- function(data, level = Course, id_plot = "test"){

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
    visualize_LDA(level = Course,
                  id_plot = paste(id_plot, "_course"))
  
  data %>% 
    visualize_LDA(level = Cluster,
                  id_plot = paste(id_plot, "_cluster"))
    
  data %>% 
    visualize_LDA(level = Concentration,
                  id_plot = paste(id_plot, "_concentration"))
  
}
```

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
