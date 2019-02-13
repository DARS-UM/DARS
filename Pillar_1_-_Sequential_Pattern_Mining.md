Pillar 1 - Sequential Pattern Mining
================
DARS
2019-02-13

-   [Setup](#setup)
-   [Data Exploration](#data-exploration)
-   [Creating Transactions and Sequences](#creating-transactions-and-sequences)
    -   [Take / not take, pass / fail / not take](#take-not-take-pass-fail-not-take)
    -   [Grade](#grade)
    -   [Transcript with preceding courses (for SR by hand)](#transcript-with-preceding-courses-for-sr-by-hand)
    -   [Transactions](#transactions)
    -   [Sequences](#sequences)
    -   [Support](#support)
-   [Mining Rules](#mining-rules)
    -   [Association Rule](#association-rule)
        -   [Functions](#functions)
        -   [take -&gt; take](#take---take)
        -   [fail -&gt; fail](#fail---fail)
        -   [low grade -&gt; low grade](#low-grade---low-grade)
        -   [not take -&gt; fail](#not-take---fail)
        -   [grade less than or equal to x -&gt; grade less than or equal to 6](#grade-less-than-or-equal-to-x---grade-less-than-or-equal-to-6)
        -   [Editing AR](#editing-ar)
        -   [Save Association Rules](#save-association-rules)
    -   [Sequence Rules](#sequence-rules)
        -   [Functions](#functions-1)
        -   [take -&gt; take](#take---take-1)
        -   [fail -&gt; fail](#fail---fail-1)
        -   [low grade -&gt; low grade](#low-grade---low-grade-1)
        -   [not take -&gt; fail](#not-take---fail-1)
        -   [grade less than or equal to x -&gt; grade less than or equal to 6](#grade-less-than-or-equal-to-x---grade-less-than-or-equal-to-6-1)
        -   [Editing SR](#editing-sr)
        -   [Save Sequences Rules](#save-sequences-rules)

``` r
library(tidyverse)
library(arulesSequences)
```

Setup
=====

We load the environment `data_pillar1` which we saved at the end of the data preparation. It contains the data sets `d_course` and `d_transcript`.

``` r
load("Output/data_pillar_1.RDATA")
```

``` r
pass_grade <- 5.5
high_grade <- 6.5
```

We create a function, which, when given the code of a course, returns its title.

``` r
find_course <- function(code){ 
  
  dataset <- d_transcript %>%
    filter(`Course ID`== code)
  
  title <- dataset$`Course Title`[1]
  
  return(title)
  
}

# Example
find_course("HUM1005")
```

    ## [1] "Songs and Poetry: Theory and Analysis"

Data Exploration
================

We compute summary statistics (minimum, maximum, mean, median, standard deviation, failure rate, number of failure and count) at different levels (student, course, cluster, concentration, year and course level). We save the results in the environment `Transcript Statistics`.

``` r
#
# function providing statistics
provide_statistics <- function(data, var){
  
  data %>%
    filter(
      !is.na(!!enquo(var))
      ) %>%
    group_by(
      !!enquo(var)
      ) %>%
    summarise(
      Min             = min(Grade),
      Max             = max(Grade), 
      Mean            = mean(Grade), 
      Median          = median(Grade), 
      SD              = sd(Grade),
      `Failure Rate`  = mean(Fail),
      `Failure Count` = sum(Fail),
      Count           = n()
      ) %>%
    mutate_at(
      vars(Mean, SD, `Failure Rate`),
      round,
      digits = 2
    )
  
}

#
# Generating Statistics
statistics <- list()

# Student level
statistics$student <- d_transcript %>%
  provide_statistics(`Student ID`)

# Course level
statistics$course <- d_transcript %>%
  inner_join(
    d_course,
    by = c("Course ID")
    ) %>%
  provide_statistics(`Course ID`)

# Cluster level
statistics$cluster <- d_transcript %>%
  inner_join(
    d_course, 
    by = "Course ID"
    ) %>%
  provide_statistics(Cluster)

# Type level
statistics$type <- d_transcript %>%
  inner_join(
    d_course, by = "Course ID"
    ) %>%
  provide_statistics(Type)

# Concentration evel
statistics$concentration <- d_transcript %>%
  inner_join(
    d_course,
    by = "Course ID"
    ) %>%
  gather(
    key = X, 
    value = Concentration, 
    Concentration, `Concentration (additional)`,
    na.rm = TRUE
    ) %>%
  provide_statistics(Concentration)

# Year level
statistics$year <- d_transcript %>%
  provide_statistics(Year_numerical)

# Level level
statistics$level <- d_transcript %>%
  # TODO: filter for student who completed their studies
  inner_join(d_course, by = c("Course ID")) %>%
  provide_statistics(Level)

#
# output
save(
  statistics,
  file = "Output/Transcript Statistics.RDATA"
  )

rm(statistics)
```

Creating Transactions and Sequences
===================================

For a first exploration of arules, we conceptualise our framework like this: transaction = student item = course

First we transform our data into transaction data. For this, we first create a vector of mandatory courses that we exclude from transcripts. \#\# Data Preparation \#\#\# Take, fail, low grade

``` r
d_transactions <- list()

d_transactions$taken_PF_HL <- d_course %>%
  
  # Exclude 
  filter(
    Type != "Mandatory",                  # (i) mandatory courses e.g. COR, CAP, etc
    ! Letters %in% c("SKI", "PRO",        # (ii) skills & projects (taken by majority of students)
                     "SAS", "SAH", "SAC") # (iii) courses of semester abroad (uninformative)
    ) %>%
  
  # Join with transcripts.
  select(- Period) %>%
  inner_join(
    d_transcript,
    by = "Course ID"
    ) %>%
  
  # Identifying sequenceID
  rename(sequenceID = `Student ID`) %>%
  
  # Identifying evenID
  mutate(
    Period  = substr(Period, 1, 1),
    eventID = as.numeric(paste(Year_numerical, Period, sep = ""))
    ) %>%

  # Identifying itemID
  mutate(
    PF = case_when( Grade <  pass_grade ~ "fail",
                    Grade >= pass_grade ~ "pass"),
    HL = case_when( Grade <  high_grade ~ "low",
                    Grade >= high_grade ~ "high"),
    
    item    = `Course ID`,
    item_PF = paste(`Course ID`, PF, sep = "_"),
    item_HL = paste(`Course ID`, HL, sep = "_")
    )
```

### Take / not take, pass / fail / not take

``` r
d_transactions$TPF <- expand.grid(
  
  # Expand along students (sequenceID) and courses (itemID)
  sequenceID = unique(d_transactions$taken_PF_HL$sequenceID),
  item       = unique(d_transactions$taken_PF_HL$item),
  stringsAsFactors = FALSE
  ) %>%
  
  # Join with d_transactions
  left_join(
    d_transactions$taken_PF_HL, 
    by = c("sequenceID", "item")
    ) %>%
  
  # Create 
  mutate(
    
    TPF = case_when(
      is.na(Grade)       ~ "not taken",
      Grade < pass_grade ~ "fail",
      TRUE               ~ "pass"
      ),

    item_TPF = paste(item, TPF, sep = "_")
    
    )

rm(high_grade, pass_grade)
```

### Grade

``` r
d_transactions$G <- d_transactions$taken_PF_HL %>%
  
  # Spread along rounded grades
  mutate(
    grade_ceil = ceiling(Grade),
    Values     = TRUE
    ) %>%
  spread(
    key   = grade_ceil, 
    value = Values
    ) %>%
  
  # fill grades inferior to obtained grade with TRUE
  mutate(
    `1`  = `0`|`1`,
    `2`  = `1`|`2`,
    `3`  = `2`|`3`,
    `4`  = `3`|`4`,
    `5`  = `4`|`5`,
    `6`  = `5`|`6`,
    `7`  = `6`|`7`,
    `8`  = `7`|`8`,
    `9`  = `8`|`9`,
    `10` = `9`|`10`
    ) %>%
  
  # Gather along rounded grades
  gather(
    key   = grade_ceil,
    value = X,
    `0` :`10`, 
    na.rm   = TRUE, 
    convert = TRUE
    ) %>%
  select(
    -X
    ) %>%

  # Identifying item
  mutate(
    item_G = paste(item, grade_ceil, sep = "_")
    )
```

### Transcript with preceding courses (for SR by hand)

``` r
#
# List of all courses
course_all <- unique(d_transactions$taken_PF_HL$item)


#
# courses taken so far and not taken so far
d_transcript_cum <- d_transactions$taken_PF_HL %>%

  group_by(
    sequenceID,
    eventID
    ) %>%
  summarize(
    course_current = list(
      unique(item_PF)
      )
    ) %>%
  
  group_by(
    sequenceID
    ) %>%
  mutate(
    course_past = Reduce(
      f = c, 
      x = course_current, 
      accumulate = TRUE
      ) %>%
      lag(
        default = as.character(NA)
        )
    ) %>%
  
  rowwise %>% # equivalent to "group by row"
  mutate(
    course_so_far     = list(union  (course_past, course_current)),
    course_not_so_far = list(setdiff(course_all , course_so_far ))
    ) %>%
  ungroup


rm(course_all)
```

Transactions
------------

``` r
make_transaction <- function(data = d_transactions$taken_PF_HL, item = item){
  
  data %>%
    group_by(
      sequenceID
    ) %>%
    summarise(
      list_item = list(unique(!!enquo(item)))
    ) %>%
    ungroup %>%
    pull(
      list_item
      ) %>%
    as("transactions")
  
}


#
# Making transactions
transactions <- list()

transactions$taken <- make_transaction(item = item   )
transactions$PF    <- make_transaction(item = item_PF)
transactions$HL    <- make_transaction(item = item_HL)
transactions$TPF   <- make_transaction(data = d_transactions$TPF, item = item_TPF)
transactions$G     <- make_transaction(data = d_transactions$G  , item = item_G  )


#
# Checking transactions
# inspect(head(transactions, 10))

rm(make_transaction)
```

Sequences
---------

``` r
make_sequence <- function(data = d_transactions$taken_PF_HL, item = item){
  
    # make transactions
  data_temp <- data %>%
    arrange(
      sequenceID,
      eventID
    ) %>%
    filter(
      !is.na(eventID) #this is here casue we have courses that were never taken.
      ) %>% 
    group_by(
      sequenceID,
      eventID
    ) %>%
    summarise(
      list_item = list(unique(!!enquo(item)))
    ) %>%
    ungroup
  
  sequences <- data_temp %>%
    pull(
      list_item
      ) %>%
    as("transactions")
  
  # indicating sequence ID and event ID for each transaction
  sequences@itemsetInfo <- data_temp %>%
    select(
      sequenceID,
      eventID
      ) %>%
    as.data.frame
  
  return(sequences)
  
}


#
# Making sequences
sequences <- list()

sequences$taken <- make_sequence(item = item   )
sequences$PF    <- make_sequence(item = item_PF)
sequences$HL    <- make_sequence(item = item_HL)
#sequences_TPF  <- make_sequence(data = d_transactions$TPF, item = item_TPF) # compute ad hoc
sequences$G     <- make_sequence(data = d_transactions$G    , item = item_G  )


#
# Checking sequences
 inspect(head(sequences$G, 10))
```

    ##      items        sequenceID eventID
    ## [1]  {SSC3012_10,                   
    ##       SSC3012_7,                    
    ##       SSC3012_8,                    
    ##       SSC3012_9,                    
    ##       SSC3032_10}    0112836   20071
    ## [2]  {SSC1007_10,                   
    ##       SSC1007_7,                    
    ##       SSC1007_8,                    
    ##       SSC1007_9}     0133523   20105
    ## [3]  {HUM2005_10,                   
    ##       HUM2005_7,                    
    ##       HUM2005_8,                    
    ##       HUM2005_9}     0133523   20111
    ## [4]  {HUM2050_10,                   
    ##       HUM2050_9,                    
    ##       HUM3019_10,                   
    ##       HUM3019_8,                    
    ##       HUM3019_9}     0133523   20114
    ## [5]  {SSC2020_10,                   
    ##       SSC2020_7,                    
    ##       SSC2020_8,                    
    ##       SSC2020_9,                    
    ##       SSC3023_10,                   
    ##       SSC3023_6,                    
    ##       SSC3023_7,                    
    ##       SSC3023_8,                    
    ##       SSC3023_9}     0144452   20071
    ## [6]  {HUM2003_10,                   
    ##       HUM2003_6,                    
    ##       HUM2003_7,                    
    ##       HUM2003_8,                    
    ##       HUM2003_9,                    
    ##       HUM2018_10,                   
    ##       HUM2018_7,                    
    ##       HUM2018_8,                    
    ##       HUM2018_9,                    
    ##       SSC2019_10,                   
    ##       SSC2019_8,                    
    ##       SSC2019_9}     0171247   20071
    ## [7]  {SCI1009_10,                   
    ##       SCI1009_6,                    
    ##       SCI1009_7,                    
    ##       SCI1009_8,                    
    ##       SCI1009_9,                    
    ##       SCI2011_10,                   
    ##       SCI2011_5,                    
    ##       SCI2011_6,                    
    ##       SCI2011_7,                    
    ##       SCI2011_8,                    
    ##       SCI2011_9,                    
    ##       SSC1005_10,                   
    ##       SSC1005_6,                    
    ##       SSC1005_7,                    
    ##       SSC1005_8,                    
    ##       SSC1005_9,                    
    ##       SSC2006_10,                   
    ##       SSC2006_5,                    
    ##       SSC2006_6,                    
    ##       SSC2006_7,                    
    ##       SSC2006_8,                    
    ##       SSC2006_9}     0177849   20071
    ## [8]  {SSC2025_10,                   
    ##       SSC2025_7,                    
    ##       SSC2025_8,                    
    ##       SSC2025_9}     0177849   20081
    ## [9]  {SSC2019_10,                   
    ##       SSC2019_5,                    
    ##       SSC2019_6,                    
    ##       SSC2019_7,                    
    ##       SSC2019_8,                    
    ##       SSC2019_9}     0177849   20082
    ## [10] {SCI3046_10,                   
    ##       SCI3046_6,                    
    ##       SCI3046_7,                    
    ##       SCI3046_8,                    
    ##       SCI3046_9}     0177849   20084

``` r
rm(make_sequence #, d_transactions
   )
```

Support
-------

``` r
#
# Number of students
n_students <- d_transactions$taken_PF_HL %>%
  select(sequenceID) %>%
  n_distinct


#
# Support
d_support <- list()

# Probability of taking a course
d_support$TNT <- d_transactions$taken_PF_HL %>%
  distinct(
    item,
    sequenceID
    ) %>%
  count(
    item
    ) %>%
  mutate(
    rate.take = n / n_students,
    rate.not  = 1 - rate.take
    )


#
# Probability of failing, having a low grade
d_support$PF_HL <- d_transactions$taken_PF_HL %>%
  group_by(
    item
    ) %>%
  summarise(
    rate.fail = mean(PF == "fail"),
    rate.low  = mean(HL == "low")
    )


#
# Probability oh having a grade equal or lower than x
d_support$G <- d_transactions$taken_PF_HL %>%
  group_by(
    item
    ) %>%
  summarise(
    `0`    = mean(Grade <= 0),
    `1`    = mean(Grade <= 1),
    `2`    = mean(Grade <= 2),
    `3`    = mean(Grade <= 3),
    `4`    = mean(Grade <= 4),
    `5`    = mean(Grade <= 5),
    `6`    = mean(Grade <= 6),
    `7`    = mean(Grade <= 7),
    `8`    = mean(Grade <= 8),
    `9`    = mean(Grade <= 9),
    `10`   = mean(Grade <= 10)
    ) %>%
  gather(
    key   = grade_ceil,
    value = support.grade,
    `0` : `10`,
    convert = TRUE
    )
```

Mining Rules
============

Association Rule
----------------

### Functions

The function `my_apriori()` applies the apriori algorithm on a set of transactions with the parameters that we have chosen.

``` r
my_apriori <- function(data){
  
  data %>%
    
    apriori(
      
      # include all AR possible
      parameter = list(
        supp   = 0, # min support
        smax   = 1, # max support
        conf   = 0, # min confidence
        minlen = 2, # min length of rule
        maxlen = 2  # max length of rule
        ),
      
      # no printing during execution
      control = list(
        verbose = FALSE
        )
      
      )
  
}
```

The function `clean_AR()` transforms the rules generated by the function `my_apriori()` into a readable dataframe

``` r
clean_rules <- function(rules){
  
  rules %>%
    
    as("data.frame") %>%
    
    # Exclude rules with no support
    filter(
      support > 0
      ) %>%
    
    # separate rule
    rename_at(
      vars(matches("rule")),
      function(x) substr(x, 1, 4)
    ) %>%
    mutate(
      rule = str_remove_all(rule, pattern = "[<]|[{]|[}]|[>]")
      ) %>%
    separate(
      rule,
      into = c("lhs", "rhs"),
      sep = " = "
      ) %>%
    
    # separate lhs and rhs
    separate(
      lhs,
      into    = c("lhs_course", "lhs_outcome"),
      sep     = "_",
      remove  = FALSE,
      convert = TRUE
      ) %>%
    separate(
      rhs,
      into    = c("rhs_course", "rhs_outcome"),
      sep     = "_",
      remove  = FALSE,
      convert = TRUE
      )
  
}
```

The function `compute_support()` computes the support of the lhs and rhs of the rules.

``` r
compute_rhs.support <- function(AR, data_support, type_rule){

  AR %>%
    
    left_join(
      select(data_support, item, !!enquo(type_rule)),
      by = c("rhs_course" = "item")
      ) %>%
    mutate(
      rhs.support = !!enquo(type_rule)
      )

}
```

We encapsulate the functions `my_apriori()`, `clean_AR()` and `compute_support()` into the function `make_AR()`.

``` r
make_AR <- function(data, data_support, type_rule){
  
  data %>%
    my_apriori %>%
    clean_rules %>%
    compute_rhs.support(
      data_support = data_support,
      type_rule = !!enquo(type_rule)
      )
  
}
```

### take -&gt; take

``` r
AR <- list()

AR$taken <- transactions$taken %>%
  make_AR(
    data_support = d_support$TNT, 
    type_rule = rate.take
    )
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 17000
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 17000
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

### fail -&gt; fail

``` r
AR$PF <- transactions$PF %>%
  
  make_AR(
    data_support = d_support$PF_HL,
    type_rule = rate.fail
    ) %>%
  
  # Confidence and Lift by hand
  group_by(
    lhs,
    rhs_course
    ) %>%
  mutate(
    lhs.rhsTake.support = sum(support), # Proba of lhs and taking rhs course
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students
    ) %>%
  ungroup %>%
  
  mutate(
    confidence = support / lhs.rhsTake.support,
    lift       = confidence / rhs.support
    ) %>%
  
  # lhs and rhs must be fail
  filter(
    str_detect(lhs, "fail"),
    str_detect(rhs, "fail")
    )
```

### low grade -&gt; low grade

``` r
AR$HL <- transactions$HL %>%
  
  make_AR(
    data_support = d_support$PF_HL,
    type_rule = rate.low
    ) %>%
  
  # Confidence and Lift by hand
  group_by(
    lhs, 
    rhs_course
    ) %>%
  mutate(
    lhs.rhsTake.support = sum(support), # Proba of lhs and taking rhs course
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students
    ) %>%
  ungroup %>%
  
  mutate(
    confidence = support / lhs.rhsTake.support,
    lift       = confidence / rhs.support
    ) %>%
  
  # lhs and rhs must be fail, count >= 5
  filter(
    str_detect(lhs, "low"),
    str_detect(rhs, "low")
    )
```

### not take -&gt; fail

``` r
AR$TPF <- transactions$TPF %>%
  
  make_AR(
    data_support = d_support$PF_HL, 
    type_rule = rate.fail
  ) %>%
  
  # Confidence and Lift
  mutate(
    rhs.taken = str_detect(rhs, "fail|pass")
    ) %>%
  group_by(
    lhs,
    rhs_course
    ) %>%
    mutate(
    lhs.rhsTake.support = sum(support * rhs.taken), # Proba of lhs and taking rhs course
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students
    ) %>%
  ungroup %>%
  
  mutate(
    confidence = support / lhs.rhsTake.support,
    lift       = confidence / rhs.support
    ) %>%
  
  # lhs and rhs must be fail, count >= 5
  filter(
    str_detect(lhs, "not taken"),
    str_detect(rhs, "fail")
    )
```

### grade less than or equal to x -&gt; grade less than or equal to 6

``` r
AR$G <- transactions$G %>%
  
  my_apriori %>%
  clean_rules %>%
  
  # rhs.support
  left_join(
    d_support$G,
    by = c("rhs_course" = "item", "rhs_outcome" = "grade_ceil")
    ) %>%
  mutate(
    rhs.support = support.grade
    ) %>%
  
  # Confidence and lift
  group_by(
    lhs, 
    rhs_course
    ) %>%
  mutate(
    lhs.rhsTake.support = max(support),
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students,
    confidence          = support / lhs.rhsTake.support,
    lift                = confidence / rhs.support
    ) %>%
  ungroup %>%
  
  
  # reduce number of rules
  filter(
    lhs_course != rhs_course, # exclude rules with same course on lhs and rhs
    rhs_outcome == 6            
    ) %>%
  
  # for each combination of courses (lhs and rhs), keep AR with highest lift (most informative AR)
  group_by(
    lhs_course, 
    rhs_course
    ) %>%
  top_n(
    1,
    lift
    ) %>%
  ungroup
```

### Editing AR

``` r
edit_rules <- function(AR){
  
  AR %>%
    
    mutate(
      count = support * n_students
    ) %>%
    
    filter(
      count >= 5
      ) %>%
    
    mutate_if(
      is.numeric,
      funs(round(., 5))
    ) %>%
    
    select(
      lhs       , rhs,
      support   , count,
      confidence, rhs.support,
      lift,
      matches("rhsTake")
    ) %>%
    
    arrange(
      desc(count)
    )
  
}


AR <- lapply(
  X   = AR,
  FUN = edit_rules
  )
```

### Save Association Rules

We save and remove objects:

``` r
#
# Save association rules
save(
  AR,
  file = "App/AR.RDATA"
  )

# Remove objects
rm(
  my_apriori, make_AR,
  AR, transaction
  )
```

    ## Warning in rm(my_apriori, make_AR, AR, transaction): object 'transaction'
    ## not found

Sequence Rules
--------------

### Functions

``` r
my_cspade <- function(data){
  
  data %>%
    
    cspade(
      
      parameter = list(
        
        # only include subsequences of the type {one item} => {one item}
        maxlen  = 2,   # max length of sequence
        maxsize = 1,   # max number item for element
        
        # include all such subsequences
        support = 0,   # min suppor
        mingap  = 1,   # min time difference between consecutive element
        maxgap  = 1e4  # max time difference between consecutive element
        
        ),
      
      control = list(
        verbose = FALSE
        )
      
      )
  
}
```

``` r
my_ruleInduction <- function(rules){
  
  rules %>%
    
    ruleInduction(
      
      # keep all subsequences
      confidence = 0,
      
      control    = list(verbose = FALSE)
      
      )
  
}
```

``` r
make_SR <- function(data, data_support, type_rule){
  
  data %>%
    my_cspade %>%
    my_ruleInduction %>%
    clean_rules %>%
    compute_rhs.support(
      data_support = data_support,
      type_rule = !!enquo(type_rule)
      )
  
}
```

### take -&gt; take

``` r
SR <- list()

SR$taken <- sequences$taken %>%
  
  make_SR(
    data_support = d_support$TNT, 
    type_rule = rate.take
  )
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 14390
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 14390
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

### fail -&gt; fail

``` r
SR$PF <- sequences$PF %>%
  
  make_SR(
    data_support = d_support$PF_HL,
    type_rule = rate.fail
  ) %>%
  
  # Confidenca and lift by hand
  group_by(
    lhs,
    rhs_course
    ) %>%
  mutate(
    lhs.rhsTake.support = sum(support), # Proba of (lhs => taking rhs course)
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students
  ) %>%
  ungroup %>%
  
  mutate(
    confidence = support / lhs.rhsTake.support,
    lift       = confidence / rhs.support
    ) %>%
  
  # lhs and rhs are fail
  filter(
    str_detect(lhs, "fail"),
    str_detect(rhs, "fail")
    )
```

### low grade -&gt; low grade

``` r
SR$HL <- sequences$HL %>%
  
  make_SR(
    data_support = d_support$PF_HL,
    type_rule = rate.low
  ) %>%
  
    # Confidenca and lift by hand
  group_by(
    lhs,
    rhs_course
    ) %>%
  mutate(
    lhs.rhsTake.support = sum(support), # Proba of (lhs => taking rhs course)
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students
  ) %>%
  ungroup %>%
  
  mutate(
    confidence = support / lhs.rhsTake.support,
    lift       = confidence / rhs.support
    ) %>%
  
  # lhs and rhs are low
  filter(
    str_detect(lhs, "low"),
    str_detect(rhs, "low")
    )
```

### not take -&gt; fail

``` r
#
# SR by hand

# rhs = current course
d_transcript_cum_current <- d_transcript_cum %>%
  unnest(
    course_current,
    .drop = FALSE
    ) %>%
  rename(
    rhs = course_current
    )

# lhs = course not so far
d_transcript_cum_current %>%
  unnest(
    course_not_so_far,
    .drop = FALSE
    ) %>%
  rename(
    lhs = course_not_so_far
    ) %>%
  count(
    lhs, 
    rhs,
    sort = TRUE
    ) %>%
  filter(str_detect(rhs, "fail"))
```

### grade less than or equal to x -&gt; grade less than or equal to 6

``` r
SR$G <- sequences$G %>%
  
  my_cspade %>%
  my_ruleInduction %>%
  clean_rules %>%
  
  # rhs.support
  left_join(
    d_support$G,
    by = c("rhs_course" = "item", "rhs_outcome" = "grade_ceil")
    ) %>%
  mutate(
    rhs.support = support.grade
    ) %>%
  
  # Confidence & lift
  group_by(
    lhs,
    rhs_course
    ) %>%
  mutate(
    lhs.rhsTake.support = max(support),
    confidence          = support / lhs.rhsTake.support,
    lift                = confidence / rhs.support
    ) %>%
  ungroup %>%
  
  
  # Reduce number of rules
  filter(
    # exclude rules with same course on lhs and rhs
    lhs_course != rhs_course,
    # rhs is grade less than or equal to 6
    rhs_outcome == 6
    ) %>%
  
  # for each combination of course (lhs and rhs), keep AR with highest lift (most informative)
  group_by(
    lhs_course,
    rhs_course
    ) %>%
  top_n(
    n = 1,
    lift
    ) %>%
  ungroup
```

### Editing SR

``` r
SR <- lapply(
  X   = SR,
  FUN = edit_rules
  )
```

### Save Sequences Rules

``` r
#
# Save Sequence rules
save(
  SR,
  file = "App/SR.RDATA"
)

# Remove objects
rm(
  SR, sequences,
  my_cspade, my_ruleInduction, clean_rules, compute_rhs.support, edit_rules, make_SR,
  n_students
)
```

SR\_TPF are manually computed

``` r
# current, past, not_yet
SR_TPF <- d_transcript_cum                         %>%
  select(sequenceID, eventID, course_current, course_not_so_far)    %>%
  unnest(course_current, .drop = F)                %>%
  unnest(course_not_so_far)                        %>%
  left_join(
    select(d_transactions$TPF, 
           sequenceID, eventID, item, PF), 
    by= c("sequenceID" = "sequenceID", "eventID" = "eventID", "course_current" = "item")
    )                                              %>%
  select(sequenceID,eventID, course_not_so_far, course_current, PF) %>%
  rename(
    lhs="course_not_so_far", 
    rhs.x = "course_current", 
    rhs.PF = "PF")                                 %>%
  mutate(num_transactions = n())                   %>%
  group_by(lhs)                                    %>%
  mutate(
    num_lhs = n(),
    support_lhs         = num_lhs / num_transactions
    )                                              %>%
  group_by(rhs.x)                                  %>%
  mutate(
    num_rhs.x           = n(),
    support_rhs.x       = num_rhs.x / num_transactions
         )                                         %>%
  group_by(rhs.x,rhs.PF)                           %>%
  mutate(
    num_rhs.PF          = n(),
    support_rhs.PF      = num_rhs.PF / num_transactions
  )                                                %>%
  group_by(lhs, rhs.x)                             %>%
  mutate(
    num_rule_x          = n(),
    support_rule_rhs.x  = num_rule_x / num_transactions
  )                                                %>%
  group_by(lhs, rhs.x, rhs.PF)                     %>%
  mutate(
    num_rule_PF          = n(),
    support_rule_rhs.PF  = num_rule_PF / num_transactions,
    support_rhs_bounded  = num_rule_PF / num_rule_x
         )                                         %>%
  ungroup()                                        %>%
  mutate(
    confidence                   = support_rule_rhs.PF / support_lhs,
    confidence_bounded           = support_rhs_bounded / support_lhs, #not sure about this 
    lift                         = confidence / support_rhs.PF,
    lift_bounded                 = confidence_bounded /support_rhs_bounded #not sure about this
  )
```
