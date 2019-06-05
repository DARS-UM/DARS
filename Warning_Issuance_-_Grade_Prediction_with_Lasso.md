Pillar 1 - Student Topic Profile
================
DARS
2019-06-03

-   [Set up](#set-up)
-   [Data](#data)
-   [Lasso](#lasso)
    -   [Learn lambda with cross-validation](#learn-lambda-with-cross-validation)
    -   [Extract results](#extract-results)
    -   [Save results](#save-results)
    -   [Explore results](#explore-results)
        -   [CV MAE](#cv-mae)
        -   [Predictors](#predictors)
    -   [Prediction](#prediction)
    -   [Extra: find best alpha](#extra-find-best-alpha)

**Considerations**:

-   extract course descriptions from courses not offer in 2018-2019 e.g. SCI2012.

-   give more weight to 3000-level courses

-   restrict prep courses to UCM courses

Set up
======

Data
====

Remove course enrollments from 2007 from the student model because we have no information on the previous course enrolment of these students, hence no data on their previous academic performance and skills acquired in previous courses.

``` r
d_student_model <- d_student_model %>% filter(Year_numerical != 2007)
```

``` r
find_df <- function(course)  d_student_model %>% filter(`Course ID` == course)
```

Consider courses currently on offer and with at least 20 enrollments since 2008

``` r
d_lasso_course <- tibble(target = course_current) %>%
  
  mutate(student_model = target        %>% map(find_df),
         n             = student_model %>% map_dbl(nrow)) %>%
  
  filter(n > 20)
```

Beta of topic model to inspect results

``` r
beta  <- app_model$Beta[[1]] %>% group_by(topic) %>% top_n(10, beta)
gamma <- app_model$Gamma[[1]] %>%  mutate(topic = topic %>% str_replace(" ", "_"))
remove(app_model)
```

Lasso
=====

Learn lambda with cross-validation
----------------------------------

``` r
d_lasso <- d_lasso_course %>%
  mutate(lasso_cv = student_model %>% map(my_cv.glmnet, predictors = "GPA|Topic")) %>%
  select(-student_model)
```

Extract results
---------------

``` r
d_lasso <- d_lasso %>%
  
  # Results from CV
  mutate(m_lasso     = lasso_cv %>% map    (~ .[["glmnet.fit"]]),
         lambda_min  = lasso_cv %>% map_dbl(~ .[["lambda.min"]]),
         lambda_1se  = lasso_cv %>% map_dbl(~ .[["lambda.1se"]]),
         index_best  = lasso_cv %>% map_dbl(~ which.min(.[["cvm"]])),
         cv_error    = list(lasso_cv, index_best) %>% pmap_dbl(~ ..1[["cvm"]][..2]),
         cv_error_sd = list(lasso_cv, index_best) %>% pmap_dbl(~ ..1[["cvsd"]][..2])) %>%
  
  # Best model
  mutate(intercept = list(m_lasso, index_best) %>% pmap_dbl(~ ..1[["a0"]][..2]),
         coefi     = list(m_lasso, lambda_min) %>% pmap(coef), # clearer output than pmap_dbl(~ ..1[["beta"]][,..2])
         df        = list(m_lasso, index_best) %>% pmap_dbl(~ ..1[["df"]][..2]),
         
         dev_null  = m_lasso %>% map_dbl(~ .[["nulldev"]]),
         dev_left  = list(m_lasso, index_best) %>% pmap_dbl(~deviance.glmnet(..1)[..2]),
         dev_expl  = dev_null - dev_left,
         dev_ratio = list(m_lasso, index_best) %>% pmap_dbl(~ ..1[["dev.ratio"]][..2])) %>%
  
  arrange(cv_error)
```

Save results
------------

``` r
save(d_lasso, file = "Output/lasso.RDATA")

d_lasso_app <- d_lasso %>% select(target, lasso_cv)
save(d_lasso_app, file = "APP/Recommender System/d_lasso_app.RDATA")
```

Explore results
---------------

### CV MAE

``` r
d_lasso %>%
  ggplot() +
  geom_histogram(aes(cv_error), bins = 14) +
  labs(x = "CV mae", caption = "red: mean\ngreen: median") +
  geom_vline(xintercept = c(mean(d_lasso$cv_error)), col = "red", alpha = 0.5) +
  geom_vline(xintercept = c(median(d_lasso$cv_error)), col = "green", alpha = 0.5)
```

![](Cache/Figures/unnamed-chunk-5-1.png)

meand, median and weighted mean

``` r
mean(d_lasso$cv_error)
```

    ## [1] 0.7986161

``` r
median(d_lasso$cv_error)
```

    ## [1] 0.7676683

``` r
weighted.mean(d_lasso$cv_error, d_lasso$n)
```

    ## [1] 0.7579954

### Predictors

Topic chosen by model are related to the course.

Show results for good (largest deviance ratio): SCI2040 Microbiology, SCI1004 Introduction to Chemistry, SSC2066 Protection of Civilians in Armed Conflicts

``` r
show_predictors <- function(course){
  
  coefi <- d_lasso %>% filter(target == course) %>%
    pull(coefi) %>% .[[1]] %>%
    as.matrix %>% as_tibble(rownames = "id") %>% rename(coefi = `1`)
  
  coefi %>% filter(id %>% str_detect("GPA")) %>% print # GPA
  
  coefi_topic <- coefi %>% filter(id %>% str_detect("Topic")) %>%
    filter(coefi != 0) %>%
    filter(coefi > 0 ) %>%
    top_n(3, coefi)
  print(coefi_topic) # Topic
  
  main_topics <- coefi_topic %>% pull(id) %>% str_replace("_", " ")
  
  beta %>% filter(topic %in% main_topics) %>% head(30) 
  
}

show_predictors("SCI2040")
```

    ## # A tibble: 7 x 2
    ##   id          coefi
    ##   <chr>       <dbl>
    ## 1 GPA      0.360   
    ## 2 GPA_HUM  0       
    ## 3 GPA_SCI  0.472   
    ## 4 GPA_SSC  0       
    ## 5 GPA_COR -0.000931
    ## 6 GPA_SKI  0.364   
    ## 7 GPA_PRO  0       
    ## # A tibble: 3 x 2
    ##   id       coefi
    ##   <chr>    <dbl>
    ## 1 Topic_46  2.64
    ## 2 Topic_57  1.25
    ## 3 Topic_9   6.90

    ## # A tibble: 30 x 3
    ## # Groups:   topic [3]
    ##    topic    term           beta
    ##    <chr>    <chr>         <dbl>
    ##  1 Topic 46 material     0.0734
    ##  2 Topic 46 apply        0.0422
    ##  3 Topic 46 technique    0.0355
    ##  4 Topic 46 understand   0.0178
    ##  5 Topic 46 obtain       0.0169
    ##  6 Topic 46 meet         0.0153
    ##  7 Topic 46 conservation 0.0136
    ##  8 Topic 46 polymer      0.0136
    ##  9 Topic 46 advance      0.0127
    ## 10 Topic 46 description  0.0119
    ## # ... with 20 more rows

``` r
show_predictors("SCI1004")
```

    ## # A tibble: 7 x 2
    ##   id      coefi
    ##   <chr>   <dbl>
    ## 1 GPA     0.193
    ## 2 GPA_HUM 0    
    ## 3 GPA_SCI 0.411
    ## 4 GPA_SSC 0    
    ## 5 GPA_COR 0    
    ## 6 GPA_SKI 0.485
    ## 7 GPA_PRO 0.177
    ## # A tibble: 3 x 2
    ##   id       coefi
    ##   <chr>    <dbl>
    ## 1 Topic_25  3.35
    ## 2 Topic_30  2.80
    ## 3 Topic_4   3.05

    ## # A tibble: 30 x 3
    ## # Groups:   topic [3]
    ##    topic    term            beta
    ##    <chr>    <chr>          <dbl>
    ##  1 Topic 25 behaviour     0.0513
    ##  2 Topic 25 brain         0.0418
    ##  3 Topic 25 develop       0.0412
    ##  4 Topic 25 function      0.0374
    ##  5 Topic 25 cognitive     0.0254
    ##  6 Topic 25 behavioural   0.0197
    ##  7 Topic 25 developmental 0.0171
    ##  8 Topic 25 hormone       0.0159
    ##  9 Topic 25 child         0.0152
    ## 10 Topic 25 attention     0.0121
    ## # ... with 20 more rows

``` r
show_predictors("SSC2066")
```

    ## # A tibble: 7 x 2
    ##   id      coefi
    ##   <chr>   <dbl>
    ## 1 GPA     0    
    ## 2 GPA_HUM 0    
    ## 3 GPA_SCI 0.319
    ## 4 GPA_SSC 0    
    ## 5 GPA_COR 0    
    ## 6 GPA_SKI 0    
    ## 7 GPA_PRO 0.144
    ## # A tibble: 3 x 2
    ##   id       coefi
    ##   <chr>    <dbl>
    ## 1 Topic_16  1.40
    ## 2 Topic_49  1.52
    ## 3 Topic_62  1.68

    ## # A tibble: 30 x 3
    ## # Groups:   topic [3]
    ##    topic    term             beta
    ##    <chr>    <chr>           <dbl>
    ##  1 Topic 16 heal           0.106 
    ##  2 Topic 16 public         0.0882
    ##  3 Topic 16 evaluate       0.0290
    ##  4 Topic 16 develop        0.0215
    ##  5 Topic 16 book           0.0189
    ##  6 Topic 16 avail          0.0189
    ##  7 Topic 16 gram           0.0145
    ##  8 Topic 16 approach       0.0133
    ##  9 Topic 16 intervention   0.0133
    ## 10 Topic 16 implementation 0.0127
    ## # ... with 20 more rows

Prediction
----------

``` r
my_predict <- function(model, profile){
  
  predict.cv.glmnet(object = model, newx = profile, s = "lambda.min")
  
}
```

``` r
course_ID  <- c("COR1005", "HUM2005") # input$course
student_ID <- "6087587" # input$student

load("APP/Recommender System/student_model_nest_app.RDATA")
load("APP/Recommender System/d_lasso_app.RDATA")

student_model <- student_model_nest_app %>% 
  
  filter(`Student ID` == student_ID) %>%
  
  pull(model) %>% .[[1]]


d_lasso_app %>%
  
  filter(target %in% course_ID) %>%
  
  mutate(prediction = lasso_cv %>% map_dbl(my_predict, student_model))
```

    ## # A tibble: 2 x 3
    ##   target  lasso_cv        prediction
    ##   <chr>   <list>               <dbl>
    ## 1 HUM2005 <S3: cv.glmnet>       7.73
    ## 2 COR1005 <S3: cv.glmnet>       8.12

Extra: find best alpha
----------------------

Best alpha turns out to be 1 (lasso). Good, because predictor regularization also serves as predictor selection.

``` r
n_alpha <- 101
mae     <- numeric(n_alpha)
alphas  <- seq(0, 1, length.out = n_alpha)

for(i in 1 : n_alpha){
  
  fit_lasso <- tibble(target = course_target) %>%
  
    mutate(d = target %>% map(find_df),
           n = d      %>% map_dbl(nrow)) %>%
    
    filter(n > 20) %>%
    
    mutate(cv = d %>% map(my_cv.glmnet, alpha = alphas[i], predictors = "GPA|Topic")) %>%
    
    mutate(index_best = cv                   %>% map_dbl (~ which.min(.[["cvm"]])),
           cv_error   = list(cv, index_best) %>% pmap_dbl(~ ..1[["cvm"]][..2]    ))
    
  mae[i] <- weighted.mean(fit_lasso$cv_error, fit_lasso$n)
  
}

# slight decrease in mae as alpha increase. Set alpha = 1 in my_cv.glmnet().
m <- lm(mae ~ alphas)
plot(alphas, mae)
abline(m, col = "red")
m %>% summary
```
