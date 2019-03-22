Pillar 1 - Sequential Pattern Mining
================
DARS
2019-03-22

-   [Set up](#set-up)
-   [Descriptive statistics](#descriptive-statistics)
-   [Support](#support)
-   [transactions and sequences](#transactions-and-sequences)
    -   [taken\_PF\_HL](#taken_pf_hl)
    -   [T\_PF\_HL (not taken pass/fail high/low)](#t_pf_hl-not-taken-passfail-highlow)
    -   [Grade](#grade)
    -   [making transactions and sequence](#making-transactions-and-sequence)
-   [Transcript with preceding courses](#transcript-with-preceding-courses)
-   [Mining Rules](#mining-rules)
    -   [Helper functions](#helper-functions)
        -   [Aprior, CSPADE and rule induction](#aprior-cspade-and-rule-induction)
        -   [Separate rules](#separate-rules)
        -   [Compute metrics](#compute-metrics)
        -   [informative\_rules](#informative_rules)
    -   [make\_AR, make\_SR (wrapers)](#make_ar-make_sr-wrapers)
    -   [Mine Rules](#mine-rules)
-   [Editing Rules](#editing-rules)
    -   [App - Recommender System](#app---recommender-system)
    -   [App - Rules](#app---rules)

TODO: simplify support into one df: var1 = course id, var2 = type, var3 = rate

AR with not taken on lhs is not useful.

Set up
======

We load the environment `data_pillar1` which we saved at the end of the data preparation. It contains the data sets `d_course` and `d_transcript`.

``` r
load("Output/data_pillar_1.RDATA")
```

``` r
paste_ <- function(...) paste(..., sep = "_")
```

``` r
course_all  <- unique(d_transcript_informative$`Course ID` )
student_all <- unique(d_transcript_informative$`Student ID`)
n_students  <- length(student_all)

pass_grade <- 5.5
high_grade <- 6.5
```

Descriptive statistics
======================

We compute summary statistics (minimum, maximum, mean, median, standard deviation, failure rate, number of failure and count) at different levels (student, course, cluster, concentration, year and course level). We save the results in the environment `Transcript Statistics`.

``` r
# helper function
provide_statistics <- function(var, df = d_transcript_augmented, high_gr = high_grade, pass_gr = pass_grade){
  
  df %>%
    
    filter(!is.na(!!enquo(var))) %>%
    
    group_by(!!enquo(var)) %>%
    
    summarise(
      Count           = n(),
      Min             = min   (Grade),
      Max             = max   (Grade),
      Mean            = mean  (Grade),
      Median          = median(Grade),
      IQR             = IQR   (Grade),
      SD              = sd    (Grade),
      `Low Rate`      = mean  (Grade < high_grade),
      `Fail Rate`     = mean  (Grade < pass_grade)
      ) %>%
    
    mutate_if(is.numeric, round, digits = 2)
  
}
```

``` r
statistics <- list()

# Student level
statistics$student <- provide_statistics(`Student ID`)
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

``` r
# Course level
statistics$course  <- provide_statistics(`Course ID`)

# Cluster level
statistics$cluster <- provide_statistics(Cluster)

# Type level
statistics$type    <- provide_statistics(Type)

# Concentration level
statistics$concentration <- d_transcript_augmented %>%
  gather(
    key = X, 
    value = Concentration, 
    Concentration, `Concentration (additional)`,
    na.rm = TRUE
    ) %>%
  provide_statistics(Concentration, df = .)

# Year level
statistics$year <- provide_statistics(`Academic Year`)

# Level level
statistics$level <- provide_statistics(Level) # TODO: filter for student who completed their studies
```

Support
=======

``` r
d_support <- list()

# Probability of taking / failing / obtaining of low grade a course
d_support$TPH <- d_transcript_informative %>%
  
  group_by(`Course ID`) %>%
  
  summarise(
    rate.take = n_distinct(`Student ID`) / n_students,
    rate.fail = mean(Grade < pass_grade),
    rate.low  = mean(Grade < high_grade)
    )
```

``` r
d_support$G <- d_transcript_informative %>%
  
  group_by(`Course ID`) %>%
  
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
  
  gather(grade_ceil, rate.grade, `0` : `10`, convert = TRUE)
```

transactions and sequences
==========================

For a first exploration of arules, we conceptualise our framework like this: transaction = student item = course

First we transform our data into transaction data. For this, we first create a vector of mandatory courses that we exclude from transcripts.

``` r
d_transactions <- list()
```

taken\_PF\_HL
-------------

``` r
d_transactions$taken_PF_HL <- d_transcript_informative %>%
  
  # sequenceID and event ID
  rename(
    sequenceID = `Student ID`,
    eventID = time
    ) %>%

  # itemID
  mutate(
    item    = `Course ID`,
    item_PF = if_else(Grade >= pass_grade, paste_(item, "pass"), paste_(item, "fail")),
    item_HL = if_else(Grade >= high_grade, paste_(item, "high"), paste_(item, "low" ))
    )
```

T\_PF\_HL (not taken pass/fail high/low)
----------------------------------------

``` r
d_transactions$T_PF_HL <- d_transactions$taken_PF_HL %>%
  
  # Expand along students (sequenceID) and courses (itemID) to make missing combinations of student - course explicit
  complete(sequenceID, item) %>% 
  
  # create a new type of item: "not taken"
  mutate(
    item_TPF = if_else(is.na(item_PF), paste_(item, "not"), item_PF),
    item_THL = if_else(is.na(item_HL), paste_(item, "not"), item_HL)
    )
```

Grade
-----

``` r
d_transactions$G <- d_transactions$taken_PF_HL %>%
  
  mutate(row_of_origin = TRUE) %>%     # non-missing row_of_origin are TRUE
  
  complete(
    nesting(sequenceID, eventID, item, Grade), grade_ceil,
    fill = list(row_of_origin = FALSE) # missing row_of_origin are FALSE
    ) %>%
   
  group_by(sequenceID, eventID, item, Grade) %>%
  
  distinct(sequenceID, eventID, item, grade_ceil, Grade, row_of_origin) %>% # reminder that we should get rid of duplicates oin d_transcript
  
  arrange(grade_ceil) %>%
  
  filter(cumany(row_of_origin)) %>% # only keep grades larger than ceil_grade
  
  mutate(item_g = paste_(item, grade_ceil)) %>%
  
  ungroup()
```

making transactions and sequence
--------------------------------

``` r
make_transaction <- function(data, item){
  
  data %>%
    
    group_by(sequenceID) %>%
    summarise(list_item = list(unique(!!item))) %>%
    ungroup %$%
    
    as(list_item, "transactions")
  
}
```

``` r
make_sequence <- function(ID, data, item){
  
  if(ID %in% c("TPF", "THL")) return("Compute sequences including courses not taken yet by hand")
  
  data_temp <- data %>%
    
    filter(!is.na(eventID)) %>%  # exclude courses that were never taken.
    
    arrange(sequenceID, eventID) %>%
    group_by(sequenceID, eventID) %>%
    summarise(list_item = list(unique(!!enquo(item)))) %>%
    ungroup
  
  # extract transactions
  sequences <- data_temp %>% pull(list_item) %>% as("transactions")
  
  # add attributes fro sequence ID and event ID
  sequences@itemsetInfo <- data_temp %>% select(sequenceID, eventID) %>% as.data.frame
  
  return(sequences)
  
}
```

``` r
data_general <- tribble(
  ~ ID   ,  ~ data                   , ~ item         , ~ data_support, ~ var_support    , ~ suffix_lhs , ~ suffix_rhs ,
  "taken", d_transactions$taken_PF_HL, sym("item"    ), d_support$TPH , sym("rate.take" ), NA_character_, NA_character_,
  "PF"   , d_transactions$taken_PF_HL, sym("item_PF" ), d_support$TPH , sym("rate.fail" ), "fail"       , "fail"       ,
  "HL"   , d_transactions$taken_PF_HL, sym("item_HL" ), d_support$TPH , sym("rate.low"  ), "low"        , "low"        ,
  "TPF"  , d_transactions$T_PF_HL    , sym("item_TPF"), d_support$TPH , sym("rate.fail" ), "not"        , "fail"       ,
  "THL"  , d_transactions$T_PF_HL    , sym("item_THL"), d_support$TPH , sym("rate.low"  ), "not"        , "low"        ,   
  "G"    , d_transactions$G          , sym("item_g"  ), d_support$G   , sym("rate.grade"), "_7"         , "_7"
  )
```

``` r
data_general <- data_general %>% 
  
  mutate(
    transactions = list(    data, item) %>% pmap(make_transaction),
    sequences    = list(ID, data, item) %>% pmap(make_sequence)
    ) %>%
  
  select(-data) # for memory requirement

arules::inspect(head(data_general$transactions[[3]], 10))
```

    ##      items         
    ## [1]  {SSC3012_high,
    ##       SSC3032_high}
    ## [2]  {HUM2005_low, 
    ##       HUM2050_high,
    ##       HUM3019_high,
    ##       SSC1007_low, 
    ##       SSC3032_low} 
    ## [3]  {SSC2020_low, 
    ##       SSC3023_low} 
    ## [4]  {HUM2003_low, 
    ##       HUM2018_low, 
    ##       SSC2019_high}
    ## [5]  {HUM2043_high,
    ##       HUM2043_low, 
    ##       SCI1009_low, 
    ##       SCI2011_low, 
    ##       SCI3046_low, 
    ##       SCI3048_low, 
    ##       SSC1005_low, 
    ##       SSC2006_low, 
    ##       SSC2019_low, 
    ##       SSC2025_low} 
    ## [6]  {HUM3029_low, 
    ##       SCI2011_high,
    ##       SCI2011_low, 
    ##       SCI2034_low, 
    ##       SCI3046_low, 
    ##       SSC1005_low, 
    ##       SSC2004_low, 
    ##       SSC2006_low, 
    ##       SSC2025_low, 
    ##       SSC2050_low, 
    ##       SSC3032_low, 
    ##       SSC3040_low} 
    ## [7]  {HUM2022_high,
    ##       SSC3011_low, 
    ##       SSC3036_low} 
    ## [8]  {SSC3011_low} 
    ## [9]  {SSC2025_low} 
    ## [10] {HUM1011_high,
    ##       SSC2018_high,
    ##       SSC2020_high,
    ##       SSC2036_high,
    ##       SSC2043_low}

``` r
arules::inspect(head(data_general$sequences   [[3]], 10))
```

    ##      items                                             sequenceID eventID
    ## [1]  {SSC3012_high,SSC3032_high}                       0112836    20071  
    ## [2]  {SSC1007_low}                                     0133523    20105  
    ## [3]  {HUM2005_low}                                     0133523    20111  
    ## [4]  {HUM2050_high,HUM3019_high}                       0133523    20114  
    ## [5]  {SSC3032_low}                                     0133523    20115  
    ## [6]  {SSC2020_low,SSC3023_low}                         0144452    20071  
    ## [7]  {HUM2003_low,HUM2018_low,SSC2019_high}            0171247    20071  
    ## [8]  {SCI1009_low,SCI2011_low,SSC1005_low,SSC2006_low} 0177849    20071  
    ## [9]  {SSC2025_low}                                     0177849    20081  
    ## [10] {SSC2019_low}                                     0177849    20082

Transcript with preceding courses
=================================

``` r
d_transcript_cum <- d_transactions$taken_PF_HL %>%
  
  # item_take
  mutate(item_take = paste_(item, "take")) %>%
  
  # current courses
  group_by(sequenceID, eventID) %>%
  
  summarize_at(vars(matches("item")), ~ unique(.) %>% list) %>%
  
  mutate(course_current = list(item, item_take, item_PF, item_HL) %>% pmap(~ c(...) %>% unique)) %>%
  
  # courses so far, past courses and course not taken yet
  group_by(sequenceID) %>%
  
  arrange(eventID) %>%
  
  mutate(
    course_so_far  = course_current %>% accumulate(union),
    course_past    = course_so_far  %>% lag(default = NA_character_),
    course_not_yet = course_past    %>% map(~ setdiff(course_all, .) %>% paste_("not"))
    ) %>%
  
  ungroup %>%
  
  # lhs: past courses and courses not taken yet, rhs: current courses
  mutate(
    lhs = list(course_past, course_not_yet) %>% pmap(union),
    rhs = course_current
    ) %>%
  
  select(lhs, rhs) # for memory requirement
```

Mining Rules
============

Helper functions
----------------

``` r
count_min   <- 1
support_min <- count_min / n_students

confidence_min <- 1e-3
```

``` r
separate_lhs <- function(df) df %>% separate(col = lhs , into = c("lhs_course", "lhs_outcome"), remove  = FALSE, convert = TRUE)
```

``` r
separate_rhs <- function(df) df %>% separate(col = rhs , into = c("rhs_course", "rhs_outcome"), remove  = FALSE, convert = TRUE)
```

``` r
choose_lhs <- function(rules, suffix) rules %>% filter(str_detect(lhs, suffix))
```

``` r
choose_rhs <- function(rules, suffix) rules %>% filter(str_detect(rhs, suffix))
```

### Aprior, CSPADE and rule induction

The function `my_apriori()` applies the apriori algorithm on a set of transactions with the parameters that we have chosen.

``` r
my_apriori <- function(data, supp_min = support_min, conf_min = confidence_min){
  
  data %>%
    
    apriori(
      
      # include all AR possible
      parameter = list(
        supp   = supp_min, # min support
        smax   = 1, # max support
        conf   = conf_min, # min confidence
        minlen = 2, # min length of rule
        maxlen = 2  # max length of rule
        ),
      
      # no printing during execution
      control = list(verbose = FALSE)
      
      )

}
```

The function `my_cspade()` applies the cspade algorithm on a set of sequential transactions with the parameters that we have chosen.

``` r
my_cspade <- function(data, supp_min = support_min){
  
  data %>%
    
    cspade(
      
      parameter = list(
        
        # only include subsequences of the type {one item} => {one item}
        maxlen  = 2,   # max length of sequence
        maxsize = 1,   # max number item for element
        
        # include all such subsequences
        support = supp_min,   # min suppor
        mingap  = 1,   # min time difference between consecutive element
        maxgap  = 1e4  # max time difference between consecutive element
        
        ),
      
      control = list(verbose = FALSE)
      
      )
  
}
```

The function `my_ruleInduction()` creates rules from the set of frequent sequences detemined by cspade.

``` r
my_ruleInduction <- function(rules, conf_min = confidence_min){
  
  rules %>% ruleInduction(confidence = conf_min, control = list(verbose = FALSE))
  
}
```

### Separate rules

The function `clean_AR()` transforms the rules generated by the function `my_apriori()` into a readable dataframe

``` r
separate_rule <- function(rules){
  
  rules %>%
    
    as("data.frame") %>%
    
    # separate rule
    rename_at(vars(matches("rule")), ~ substr(., 1, 4)) %>%
    
    mutate(rule = str_remove_all(rule, pattern = "[<]|[{]|[}]|[>]")) %>%
    
    separate(col = rule, into = c("lhs", "rhs"), sep = " = ") %>%
    
    # separate lhs and rhs
    separate_lhs %>%
    separate_rhs
  
}
```

### Compute metrics

``` r
compute_count <- function(rules) rules %>% mutate(count = support * n_students)
```

``` r
compute_rhs.support <- function(rules, ID, data_support, var_support){
  
  if(ID == "G") by_var <- c("rhs_course" = "Course ID", "rhs_outcome" = "grade_ceil")
  else          by_var <- c("rhs_course" = "Course ID"                              )

  rules %>%
    
    left_join(data_support, by = by_var) %>%
    
    mutate(rhs.support = !!enquo(var_support)) %>%
    
    select(- matches("rate"))
  
}
```

``` r
compute_lhs.rhsTake <- function(rules, ID){
  
  if(ID == "G") rules <- rules %>% group_by(lhs, rhs_course) %>% mutate(lhs.rhsTake.support = max(support)) %>% ungroup
  else          rules <- rules %>% group_by(lhs, rhs_course) %>% mutate(lhs.rhsTake.support = sum(support)) %>% ungroup
  
  rules %>% mutate(lhs.rhsTake.count = lhs.rhsTake.support * n_students)
  
}
```

``` r
compute_confidence <- function(rules, ID){
  
  if(ID != "taken") rules <- rules %>% mutate(confidence = support / lhs.rhsTake.support)

  rules
  
}
```

``` r
compute_lift <- function(rules) rules %>% mutate(lift = confidence / rhs.support)
```

``` r
compute_metrics <- function(rules, ID, data_support, var_support){

  rules %>% # rules with support
    
    compute_count %>%
    
    compute_rhs.support(ID = ID, data_support = data_support, var_support = !!enquo(var_support)) %>%
    
    compute_lhs.rhsTake(ID = ID) %>%
    
    compute_confidence(ID = ID) %>%
    
    compute_lift %>%
    
    arrange(desc(support))
  
}
```

### informative\_rules

``` r
informative_rules <- function(rules, ID, suffix_lhs, suffix_rhs){
  
  if(ID != "taken") rules <- rules %>% choose_lhs(suffix_lhs) %>% choose_rhs(suffix_rhs)
  
  rules %>% filter(lhs_course != rhs_course)

}
```

make\_AR, make\_SR (wrapers)
----------------------------

We encapsulate the functions `my_apriori()`, `clean_AR()` and `compute_support()` into the function `make_AR()`.

``` r
make_AR <- function(ID, transactions, data_support, var_support, suffix_lhs, suffix_rhs){
  
  transactions %>%
    
    my_apriori %>%
    
    separate_rule %>%

    compute_metrics(ID = ID, data_support = data_support, var_support = !!enquo(var_support)) %>%
    
    informative_rules(ID = ID, suffix_lhs = suffix_lhs, suffix_rhs = suffix_rhs) 
  
}
```

We encapsulate the fuctions `my_cpade`, `my_ruleInduction`, and `clean_rules` into `make_SR`

``` r
make_SR_regular <- function(sequences){
  
  sequences %>%
    
    my_cspade %>%
    
    my_ruleInduction %>%
    
    separate_rule
  
}
```

``` r
make_SR_TPF_THL <- function(ID){
  
  if(ID == "TPF") suffix_rhs_take <- "pass|fail"
  else            suffix_rhs_take <- "low|high"
  
  d_transcript_cum %>%
   
    # unnest rhs first for memory requirement
   
    # rhs
    unnest(rhs, .drop = FALSE) %>% 
    choose_rhs(suffix_rhs_take) %>% # keep both pass & fail, or high & low, to compute support support.lhs_rhsTake
 
    # lhs
    unnest(lhs, .drop = FALSE) %>% 
    choose_lhs("not") %>%
    separate_lhs %>%
    
    # rule support
    count(lhs, rhs) %>%
    rename(count = n) %>%
    mutate(support = count / n_students) %>%
    
    separate_lhs %>%
    separate_rhs
  
}
```

``` r
make_SR <- function(ID, sequences, data_support, var_support, suffix_lhs, suffix_rhs){
  
  if(ID %in% c("TPF", "THL")) sequences <- make_SR_TPF_THL(ID = ID)
  else                        sequences <- make_SR_regular(sequences)
    
   sequences %>%

    compute_metrics(ID = ID, data_support = data_support, var_support = !!enquo(var_support)) %>%
    
    informative_rules(ID = ID, suffix_lhs = suffix_lhs, suffix_rhs = suffix_rhs) 
  
}
```

Mine Rules
----------

We apply `make_AR` to get different sets of rules:

``` r
data_general <- data_general %>% 
  
  mutate(
    AR = list(ID, transactions, data_support, var_support, suffix_lhs, suffix_rhs) %>% pmap(make_AR),
    SR = list(ID, sequences   , data_support, var_support, suffix_lhs, suffix_rhs) %>% pmap(make_SR)
    )
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 17170
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 17170
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 11816
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 11816
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

``` r
rules_clean <- data_general %>% 
  
  gather(key = type_rule, value = rules, AR, SR) %>%
  
  select(ID, type_rule, rules)
```

Editing Rules
=============

The function edit\_rules makes AR easier to read. It keeps only rules that appear more than 5 times, rounds numerical variables to 5 significant digits, and drops aiding columns which were only used for computation in previous stages but add no additional information.

App - Recommender System
------------------------

``` r
edit_RS <- function(rules){
  
  rules %>%
    
    filter(
       count      >= 20,
       lift       >  1,
       confidence >= 0.4
       )
  
}
```

``` r
rules_clean <- rules_clean %>% mutate(rules_RS = rules %>% map(edit_RS))
```

App - Rules
-----------

``` r
edit_rules <- function(rules){
  
  rules %>%
    
    select(
      lhs, rhs,
      matches("rhsTake.count"), count,
      matches("rhsTake.support"),
      confidence,
      support, 
      lift
      ) %>%
    
     mutate_if(
       is.numeric,
       round,
       digits = 3
       ) %>%
    
    arrange(desc(count))
  
}
```

``` r
rules_clean <- rules_clean %>% mutate(rules_rules = rules %>% map(edit_rules)) %>% select(-rules)
```

``` r
save(rules_clean, file = "App/rules_clean.RDATA")
save(rules_clean, file = "App/Rules/rules_clean.RDATA")
save(rules_clean, file = "App/Recommender System//rules_clean.RDATA")
```

``` r
save(data_general, file = "Output/rules.RDATA")
```

``` r
print(rules_clean$rules_RS[[10]])
```

    ## # A tibble: 0 x 13
    ## # ... with 13 variables: lhs <chr>, lhs_course <chr>, lhs_outcome <chr>,
    ## #   rhs <chr>, rhs_course <chr>, rhs_outcome <chr>, count <dbl>,
    ## #   support <dbl>, rhs.support <dbl>, lhs.rhsTake.support <dbl>,
    ## #   lhs.rhsTake.count <dbl>, confidence <dbl>, lift <dbl>
