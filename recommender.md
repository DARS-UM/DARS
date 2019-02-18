Recommender
================
DARS
2019-02-18

-   [Set up](#set-up)
    -   [Libraries, import data](#libraries-import-data)
    -   [Background data](#background-data)
-   [Red Flags](#red-flags)

Set up
======

Libraries, import data
----------------------

``` r
library(tidyverse)
```

``` r
load("Output/data_pillar_1.RDATA")
load("App/rules.RDATA")
rm(AR)
```

For convenience:

``` r
course_all <- d_course %>%
  inner_join(
    d_transcript,
    by = "Course ID"
    ) %>%
  distinct(
    `Course ID`
    ) %>%
  # remove semester abroad, skills and projects
  filter(
    ! str_detect(`Course ID`, patter = "SA|SKI|PRO")
    ) %>%
  pull

pass_grade <- 5.5
high_grade <- 6.5
```

Background data
---------------

``` r
set.seed(1)


#
# student profile

# number of course taken by student so far
n_course <- 25


student <- list(
  
  # transcript: tibble with course and grade
  transcript = tibble(
    
    course = sample(
      x       = course_all,
      size    = n_course,
      replace = FALSE
      ),
    
    grade = rnorm(
      n    = n_course,
      mean = 7,
      sd   = 2
      )
    
    ),
  
  # interest of student (to be used with topic model)
  interest = c("key", "words", "related", "to", "topics")
    
)


#
# list of courses offered following semester
pool_courses <- sample(
  x       = course_all,
  size    = 60,
  replace = FALSE
  )
```

Red Flags
=========

``` r
augment_student_transcript <- function(transcript){
  
  transcript %>%
    
    mutate(
      fail       = grade < pass_grade,
      low        = grade < high_grade,
      grade_ceil = ceiling(grade)
      )
  
}


#
# past courses with low grade
course_low <- student$transcript %>%
  augment_student_transcript %>%
  filter(low) %>%
  pull(course)



#
# Red flags
red_flags <- SR$HL %>%
  
  # select appropriate rules
  filter(
    rhs_course %in% pool_courses,
    lhs_course %in% course_low
    ) %>%
  
  # if several rule with same rhs, keep rule with highest confidence
  group_by(
    rhs_course
    ) %>%
  top_n(
    n  = 1,
    wt = confidence
    ) %>%
  ungroup %>%
  
  # provide explanation
  transmute(
    
    explanation = paste(
      "Red flag for ",
      rhs_course,
      ": you obtained less than 6.5/10 in ",
      lhs_course,
      "\n",
      "and ",
      round(confidence * 100, digits = 1),
      "% of the ",
      lhs.rhsTake.count,
      " students who have taken ", 
      rhs_course,
      "\n",
      "after obtaining a similar grade in ",
      lhs_course,
      "\n",
      "did not do well in ",
      rhs_course,
      ".",
      sep = ""
      ),
    
    course = rhs_course
    
    )

print(red_flags)
```

    ## # A tibble: 8 x 2
    ##   explanation                                                        course
    ##   <chr>                                                              <chr> 
    ## 1 "Red flag for SSC3036: you obtained less than 6.5/10 in SSC1009\n~ SSC30~
    ## 2 "Red flag for SSC2043: you obtained less than 6.5/10 in SSC1009\n~ SSC20~
    ## 3 "Red flag for SSC2038: you obtained less than 6.5/10 in SSC1009\n~ SSC20~
    ## 4 "Red flag for SSC2037: you obtained less than 6.5/10 in SSC1009\n~ SSC20~
    ## 5 "Red flag for SSC1006: you obtained less than 6.5/10 in SSC1009\n~ SSC10~
    ## 6 "Red flag for SCI2012: you obtained less than 6.5/10 in SCI2011\n~ SCI20~
    ## 7 "Red flag for SCI1016: you obtained less than 6.5/10 in SSC1009\n~ SCI10~
    ## 8 "Red flag for HUM1013: you obtained less than 6.5/10 in SSC1009\n~ HUM10~

``` r
cat(red_flags[[1,1]])
```

    ## Red flag for SSC3036: you obtained less than 6.5/10 in SSC1009
    ## and 51.9% of the 27 students who have taken SSC3036
    ## after obtaining a similar grade in SSC1009
    ## did not do well in SSC3036.
