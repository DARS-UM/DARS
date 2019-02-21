Pillar 1 - Sequential Pattern Mining
================
DARS
2019-02-21

-   [Setup](#setup)
-   [Data Exploration](#data-exploration)
-   [Creating Transactions and Sequences](#creating-transactions-and-sequences)
    -   [Take, fail, low grade](#take-fail-low-grade)
    -   [Take / not take, pass / fail / not take](#take-not-take-pass-fail-not-take)
    -   [Grade](#grade)
    -   [Transcript with preceding courses (for SR by hand)](#transcript-with-preceding-courses-for-sr-by-hand)
    -   [Transactions](#transactions)
    -   [Sequences](#sequences)
    -   [Support](#support)
-   [Mining Rules](#mining-rules)
    -   [Functions](#functions)
        -   [my\_aprior, my\_cspade and my\_ruleInduction](#my_aprior-my_cspade-and-my_ruleinduction)
        -   [clean\_rules](#clean_rules)
        -   [compute\_rhs.support](#compute_rhs.support)
        -   [compute\_conf\_lift](#compute_conf_lift)
        -   [make\_AR and make\_SR (wrapers)](#make_ar-and-make_sr-wrapers)
    -   [Association Rule](#association-rule)
        -   [take -&gt; take](#take---take)
        -   [fail -&gt; fail](#fail---fail)
        -   [low grade -&gt; low grade](#low-grade---low-grade)
        -   [not -&gt; fail](#not---fail)
        -   [not -&gt; low](#not---low)
        -   [grade less than or equal to x -&gt; grade less than or equal to 6](#grade-less-than-or-equal-to-x---grade-less-than-or-equal-to-6)
    -   [Sequence Rules](#sequence-rules)
        -   [take -&gt; take](#take---take-1)
        -   [fail -&gt; fail](#fail---fail-1)
        -   [low grade -&gt; low grade](#low-grade---low-grade-1)
        -   [not -&gt; fail](#not---fail-1)
        -   [not -&gt; low](#not---low-1)
        -   [grade less than or equal to x -&gt; grade less than or equal to 6](#grade-less-than-or-equal-to-x---grade-less-than-or-equal-to-6-1)
    -   [Editing rules](#editing-rules)

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


join_d_course <- function(data){
  
  data %>%
    inner_join(
    d_course,
    by = c("Course ID")
    )
    
}


join_d_transcript <- function(data){
  
  data %>%
    inner_join(
    d_transcript,
    by = c("Course ID")
    )
    
}
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

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

    ## Warning: Unknown or uninitialised column: 'Course Title'.

    ## NULL

Data Exploration
================

We compute summary statistics (minimum, maximum, mean, median, standard deviation, failure rate, number of failure and count) at different levels (student, course, cluster, concentration, year and course level). We save the results in the environment `Transcript Statistics`.

``` r
#
# function providing statistics
provide_statistics <- function(data, var){
  
  data %>% 
    
    mutate(
      Grade = case_when(
        Grade >= 5.5 ~ Grade,
        T            ~ as.numeric(NA)
        )
      ) %>%
    
    filter(
      !is.na(!!enquo(var))
      ) %>%
    group_by(
      !!enquo(var)
      ) %>%
    
    summarise(
      Count           = n(),
      Min             = min(Grade, na.rm = TRUE),
      Max             = max(Grade, na.rm = TRUE),
      Mean            = mean(Grade, na.rm = TRUE),
      Median          = median(Grade, na.rm = TRUE),
      IQR             = IQR(Grade, na.rm = TRUE),
      SD              = sd(Grade, na.rm = TRUE),
      `Low Rate`      = mean(is.na(Grade) | Grade < high_grade),
      `Fail Rate`     = mean(is.na(Grade))
      ) %>%
    
    mutate_at(
      vars(Mean, SD, `Low Rate`, `Fail Rate`, IQR),
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
  join_d_course %>%
  provide_statistics(`Course ID`)

# Cluster level
statistics$cluster <- d_transcript %>%
  join_d_course %>%
  provide_statistics(Cluster)

# Type level
statistics$type <- d_transcript %>%
  join_d_course %>%
  provide_statistics(Type)

# Concentration level
statistics$concentration <- d_transcript %>%
  join_d_course %>%
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
  join_d_course %>%
  provide_statistics(Level)
```

Creating Transactions and Sequences
===================================

For a first exploration of arules, we conceptualise our framework like this: transaction = student item = course

First we transform our data into transaction data. For this, we first create a vector of mandatory courses that we exclude from transcripts. \#\# Data Preparation

### Take, fail, low grade

``` r
d_transactions <- list()

d_transactions$taken_PF_HL <- d_course %>%
  
  # Exclude 
  filter(
    Type != "Mandatory",  # (i) mandatory courses e.g. COR, CAP, etc
    ! Letters %in% c(
      "SKI", "PRO",       # (ii) skills & projects (taken by majority of students)
      "SAS", "SAH", "SAC" # (iii) courses of semester abroad (uninformative)
      )
    ) %>%
  
  # Join with transcripts
  select(
    - Period
    ) %>%
  join_d_transcript %>%
  
  # Identifying sequenceID
  rename(
    sequenceID = `Student ID`
    ) %>%
  
  # Identifying evenID
  mutate(
    Period  = substr(Period, 1, 1),
    eventID = as.numeric(paste(Year_numerical, Period, sep = ""))
    ) %>%

  # Identifying itemID
  mutate(
    PF = case_when(
      Grade <  pass_grade ~ "fail",
      Grade >= pass_grade ~ "pass"
      ),
    HL = case_when(
      Grade <  high_grade ~ "low",
      Grade >= high_grade ~ "high"
      ),
    
    item    = `Course ID`,
    item_PF = paste(item, PF, sep = "_"),
    item_HL = paste(item, HL, sep = "_")
    )
```

For convenience:

``` r
course_all  <- unique(d_transactions$taken_PF_HL$item      )
student_all <- unique(d_transactions$taken_PF_HL$sequenceID)
n_students  <- length(student_all)
```

### Take / not take, pass / fail / not take

``` r
d_transactions$T_PF_HL <- expand.grid(
  
  # Expand along students (sequenceID) and courses (itemID)
  sequenceID       = student_all,
  item             = course_all,
  stringsAsFactors = FALSE
  ) %>%
  
  # Join with d_transactions
  left_join(
    d_transactions$taken_PF_HL, 
    by = c("sequenceID", "item")
    ) %>%
  
  # Create item
  mutate(
    
    TPF = case_when(
      is.na(Grade)        ~ "not",
      Grade <  pass_grade ~ "fail",
      Grade >= pass_grade ~ "pass"
      ),
    THL = case_when(
      is.na(Grade)        ~ "not",
      Grade <  high_grade ~ "low",
      Grade >= high_grade ~ "high"
      ),

    item_TPF = paste(item, TPF, sep = "_"),
    
    item_THL = paste(item, THL, sep = "_")
    
    )
```

### Grade

``` r
d_transactions$G <- d_transactions$taken_PF_HL %>%
  
  # Spread along ceiling grades
  mutate(
    grade_ceil = ceiling(Grade),
    Values     = TRUE,
    id         = 1 : n() # to ensure each row is unique
    ) %>%
  spread(
    key   = grade_ceil, 
    value = Values
    ) %>%
  
  # grade x or lower
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

#### Save d\_transactions

``` r
save(
  d_transactions,
  file = "Output/transaction_df.RDATA"
  )
```

### Transcript with preceding courses (for SR by hand)

``` r
d_transcript_cum <- d_transactions$taken_PF_HL %>%
  
  # item_take
  mutate(
    item_take = paste(item, "take", sep = "_")
  ) %>%
  
  # course current
  group_by(
    sequenceID,
    eventID
    ) %>%
  summarize_at(
    c("item", "item_take", "item_PF", "item_HL"),
    function(x) list(unique(x))
    ) %>%
  
  # course current all
  rowwise %>%
  mutate(
    course_current = list(c(item, item_take, item_PF, item_HL))
    ) %>%
  
  # course past
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
  
  # course so far, course not so far
  rowwise %>%
  mutate(
    
    course_past_current = list(union  (course_past, course_current      )),
    
    course_not_yet      = list(setdiff(course_all , course_past_current )),
    course_not_yet      = list(paste(course_not_yet, "not", sep = "_"   )),
    
    course_so_far       = list(union  (course_past, course_not_yet))
    
    ) %>%
  ungroup %>%
  
  # lhs: past, rhs: present
  select(
    lhs = course_so_far,
    rhs = course_current
  )
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
transactions$TPF   <- make_transaction(data = d_transactions$T_PF_HL, item = item_TPF)
transactions$THL   <- make_transaction(data = d_transactions$T_PF_HL, item = item_THL)
transactions$G     <- make_transaction(data = d_transactions$G  , item = item_G  )

# inspect(head(transactions$taken, 10))
```

Sequences
---------

``` r
make_sequence <- function(data = d_transactions$taken_PF_HL, item = item){
  
  data_temp <- data %>%
    arrange(
      sequenceID,
      eventID
    ) %>%
    filter(
      !is.na(eventID) # exclude courses that were never taken.
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
  
  # indicate sequence ID and event ID for each transaction
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
sequences$G     <- make_sequence(data = d_transactions$G    , item = item_G  )

#inspect(head(sequences$G, 10))
```

Support
-------

``` r
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

``` r
load("Output/Transaction and Sequences.RDATA")
```

Functions
---------

### my\_aprior, my\_cspade and my\_ruleInduction

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

The function `my_cspade()` applies the cspade algorithm on a set of sequential transactions with the parameters that we have chosen.

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

The function `my_ruleInduction()` creates rules from the set of frequent sequences detemined by cspade.

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

### clean\_rules

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

### compute\_rhs.support

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

### compute\_conf\_lift

``` r
compute_conf_lift <- function(rules){
  
  rules %>%
  
  # support (and count) of doing lhs and taking course of rhs
  group_by(
    lhs,
    rhs_course
    ) %>%
    mutate(
      lhs.rhsTake.support = sum(support),
      lhs.rhsTake.count   = lhs.rhsTake.support * n_students
      ) %>%
    ungroup %>%
    
    mutate(
      confidence = support / lhs.rhsTake.support,
      lift       = confidence / rhs.support
      )
  
}
```

### make\_AR and make\_SR (wrapers)

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

We encapsulate the fuctions `my_cpade`, `my_ruleInduction`, and `clean_rules` into `make_SR`

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

Association Rule
----------------

We apply make\_AR to get different sets of rules:

### take -&gt; take

``` r
AR <- list()

AR$taken <- transactions$taken %>%
  make_AR(
    data_support = d_support$TNT, 
    type_rule = rate.take
    )
```

### fail -&gt; fail

``` r
AR$PF <- transactions$PF %>%
  
  make_AR(
    data_support = d_support$PF_HL,
    type_rule = rate.fail
    ) %>%
  
  compute_conf_lift %>%
  
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
  
  compute_conf_lift %>%
  
  # lhs and rhs must be fail, count >= 5
  filter(
    str_detect(lhs, "low"),
    str_detect(rhs, "low")
    )
```

### not -&gt; fail

``` r
AR$TPF <- transactions$TPF %>%
  
  make_AR(
    data_support = d_support$PF_HL, 
    type_rule = rate.fail
    ) %>%
  
  # Confidence and Lift
  filter(
    str_detect(lhs, "not"),
    str_detect(rhs, "fail|pass") # only keep rhsTake for computing confidence and lift
    ) %>%
  compute_conf_lift %>%
  
  # rhs must be fail, count >= 5
  filter(
    str_detect(rhs, "fail")
    )
```

### not -&gt; low

``` r
AR$THL <- transactions$THL %>%
  
  make_AR(
    data_support = d_support$PF_HL, 
    type_rule = rate.low
    ) %>%
  
  # only keep lhs is not and rhs is low or high to compute Confidence and Lift
  filter(
    str_detect(lhs, "not"),
    str_detect(rhs, "low|high")
    ) %>%
  compute_conf_lift %>%
  
  # lhs and rhs must be fail, count >= 5
  filter(
    str_detect(rhs, "low")
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
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students
    ) %>%
  ungroup %>%
  
  mutate(
    confidence = support / lhs.rhsTake.support,
    lift       = confidence / rhs.support
    ) %>%
  
  # Reduce number of rules
  filter(
    # exclude rules with same course on lhs and rhs
    lhs_course != rhs_course,
    # rhs is grade less than or equal to 6
    rhs_outcome <= 6,
    lhs_outcome <= 7
    )
```

Sequence Rules
--------------

We apply make\_SR to get different types of sequential rules:

### take -&gt; take

``` r
SR <- list()

SR$taken <- sequences$taken %>%
  
  make_SR(
    data_support = d_support$TNT, 
    type_rule = rate.take
  )
```

### fail -&gt; fail

``` r
SR$PF <- sequences$PF %>%
  
  make_SR(
    data_support = d_support$PF_HL,
    type_rule = rate.fail
  ) %>%
  
  compute_conf_lift %>%
  
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
  
  compute_conf_lift %>%
  
  # lhs and rhs are low
  filter(
    str_detect(lhs, "low"),
    str_detect(rhs, "low")
    )
```

### not -&gt; fail

``` r
SR$TPF <- d_transcript_cum %>%
  
  # rhs
  unnest(
    rhs,
    .drop = FALSE
    ) %>%
  filter(
    str_detect(rhs, "pass|fail")
    ) %>%
  
  # lhs
  unnest(
    lhs,
    .drop = FALSE
    ) %>%
  filter(
    str_detect(lhs, "not")
    ) %>%
  
  # rule
  unite(
    col = "rule",
    lhs, rhs,
    sep = " => "
    ) %>%
  
  # rule support
  count(
    rule
    ) %>%
  mutate(
    support = n / n_students
    ) %>%
  
  # regular fucntions
  clean_rules %>%
  
  compute_rhs.support(
    data_support = d_support$PF_HL,
    type_rule    = rate.fail
    ) %>%
  
  compute_conf_lift %>%
  
  # lhs not, rhs low
  filter(
    str_detect(rhs, "fail")
    )
```

### not -&gt; low

``` r
SR$THL <- d_transcript_cum %>%
  
  # rhs
  unnest(
    rhs,
    .drop = FALSE
    ) %>%
  filter(
    str_detect(rhs, "high|low")
    ) %>%
  
  # lhs
  unnest(
    lhs,
    .drop = FALSE
    ) %>%
  filter(
    str_detect(lhs, "not")
    ) %>%
  
  # rule
  unite(
    col = "rule",
    lhs, rhs,
    sep = " => "
    ) %>%
  
  # rule support
  count(
    rule
    ) %>%
  mutate(
    support = n / n_students
    ) %>%
  
  # regular fucntions
  clean_rules %>%
  
  compute_rhs.support(
    data_support = d_support$PF_HL,
    type_rule    = rate.low
    ) %>%
  
  compute_conf_lift %>%
  
  # constraints to respect
  filter(
    str_detect(rhs, "low")
    )
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
    lhs.rhsTake.count   = lhs.rhsTake.support * n_students
    ) %>%
  ungroup %>%
  
  mutate(
    confidence = support / lhs.rhsTake.support,
    lift       = confidence / rhs.support
    ) %>%
  
  # Reduce number of rules
  filter(
    # exclude rules with same course on lhs and rhs
    lhs_course != rhs_course,
    # rhs is grade less than or equal to 6
    rhs_outcome <= 6,
    lhs_outcome <= 7
    )
```

Editing rules
-------------

The function edit\_rules makes AR easier to read. It keeps only rules that appear more than 5 times, rounds numerical variables to 5 significant digits, and drops aiding columns which were only used for computation in previous stages but add no additional information.

``` r
edit_rules_rulesApp <- function(rules){
  
  rules %>%
    
    mutate(
      count = support * n_students
      ) %>%
    
    select(
      lhs, rhs,
      support, count,
      confidence,
      matches("rhsTake.support"),
      lift,
      matches("rhsTake.count")
      ) %>%
    
     mutate_if(
       is.numeric,
       round,
       digits = 3
       ) %>%
    
    arrange(
      desc(count)
      )
  
}
```

``` r
AR_rulesAPP <- lapply(
  X   = AR,
  FUN = edit_rules_rulesApp
  )

SR_rulesAPP <- lapply(
  X   = SR,
  FUN = edit_rules_rulesApp
  )
```

``` r
save(
  AR_rulesAPP, SR_rulesAPP,
  file = "App/rules/rules.RDATA"
  )
```

``` r
print(AR$THL)
```

    ## # A tibble: 19,793 x 14
    ##    lhs   lhs_course lhs_outcome rhs   rhs_course rhs_outcome support
    ##    <chr> <chr>      <chr>       <chr> <chr>      <chr>         <dbl>
    ##  1 SSC1~ SSC1006    not         UGR3~ UGR3003    low         3.94e-4
    ##  2 SCI1~ SCI1016    not         UGR3~ UGR3003    low         3.94e-4
    ##  3 SCI2~ SCI2012    not         UGR3~ UGR3003    low         3.94e-4
    ##  4 SSC1~ SSC1007    not         UGR3~ UGR3003    low         3.94e-4
    ##  5 SSC2~ SSC2046    not         UGR3~ UGR3003    low         3.94e-4
    ##  6 SSC1~ SSC1025    not         UGR3~ UGR3003    low         3.94e-4
    ##  7 SSC2~ SSC2037    not         UGR3~ UGR3003    low         3.94e-4
    ##  8 SSC3~ SSC3032    not         UGR3~ UGR3003    low         3.94e-4
    ##  9 HUM2~ HUM2003    not         UGR3~ UGR3003    low         3.94e-4
    ## 10 SSC1~ SSC1009    not         UGR3~ UGR3003    low         3.94e-4
    ## # ... with 19,783 more rows, and 7 more variables: confidence <dbl>,
    ## #   lift <dbl>, count <dbl>, rate.low <dbl>, rhs.support <dbl>,
    ## #   lhs.rhsTake.support <dbl>, lhs.rhsTake.count <dbl>

``` r
print(SR$THL)
```

    ## # A tibble: 19,810 x 14
    ##    lhs   lhs_course lhs_outcome rhs   rhs_course rhs_outcome     n support
    ##    <chr> <chr>      <chr>       <chr> <chr>      <chr>       <int>   <dbl>
    ##  1 HUM1~ HUM1003    not         HUM1~ HUM1007    low            74 0.0291 
    ##  2 HUM1~ HUM1003    not         HUM1~ HUM1010    low             8 0.00315
    ##  3 HUM1~ HUM1003    not         HUM1~ HUM1011    low            48 0.0189 
    ##  4 HUM1~ HUM1003    not         HUM1~ HUM1012    low            16 0.00630
    ##  5 HUM1~ HUM1003    not         HUM1~ HUM1013    low            58 0.0228 
    ##  6 HUM1~ HUM1003    not         HUM1~ HUM1014    low            16 0.00630
    ##  7 HUM1~ HUM1003    not         HUM2~ HUM2003    low            87 0.0343 
    ##  8 HUM1~ HUM1003    not         HUM2~ HUM2005    low            40 0.0157 
    ##  9 HUM1~ HUM1003    not         HUM2~ HUM2007    low            15 0.00591
    ## 10 HUM1~ HUM1003    not         HUM2~ HUM2008    low            18 0.00709
    ## # ... with 19,800 more rows, and 6 more variables: rate.low <dbl>,
    ## #   rhs.support <dbl>, lhs.rhsTake.support <dbl>, lhs.rhsTake.count <dbl>,
    ## #   confidence <dbl>, lift <dbl>

``` r
edit_rules_RSAPP <- function(rules){
  
  rules %>%
    
    mutate(
      count = support * n_students
      ) %>%
    
    filter(
       count      >= 10,
       lift       >= 1,
       confidence >= 0.4
      )
  
}
```

``` r
AR_RSAPP <- lapply(
  X   = AR,
  FUN = edit_rules_RSAPP
  )

SR_RSAPP <- lapply(
  X   = SR,
  FUN = edit_rules_RSAPP
  )
```
