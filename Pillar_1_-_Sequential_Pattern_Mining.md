Pillar 1 - Sequential Pattern Mining
================
DARS
2019-01-31

-   [Setup](#setup)
-   [Data Exploration](#data-exploration)
-   [Association Rules and Sequence Rules](#association-rules-and-sequence-rules)
    -   [Data Prep: Creating Transactions and Sequences](#data-prep-creating-transactions-and-sequences)
        -   [Adding not taken courses](#adding-not-taken-courses)
        -   [Expanded Transcripts](#expanded-transcripts)
        -   [Transactions for Apriori](#transactions-for-apriori)
        -   [Sequences for CSPADE](#sequences-for-cspade)
    -   [Mining Rules](#mining-rules)
        -   [Apriori Algorithm](#apriori-algorithm)
        -   [CSPADE Algorithm](#cspade-algorithm)
    -   [TODO: add AR\_grade, SR\_grade](#todo-add-ar_grade-sr_grade)

``` r
library(arulesSequences)

library(tidyverse)
```

Setup
=====

We load the environment `data_pillar1` which we saved at the end of the data preparation. It contains the data sets `d_course` and `d_transcript`.

``` r
load("Output/data_pillar_1.RDATA")
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

    ## [1] "Songs and Poetry: Theory and Analysis"

Data Exploration
================

We compute summary statistics (minimum, maximum, mean, median, standard deviation, failure rate, number of failure and count) at different levels (student, course, cluster, concentration, year and course level). We save the results in the environment `Transcript Statistics`.

``` r
# For convenience
provide_statistics <- function(data){
  
  data %>%
    summarise(
      Min       = min(Grade),
      Max       = max(Grade), 
      Mean      = mean(Grade), 
      Median    = median(Grade), 
      SD        = sd(Grade),
      `Failure Rate` = mean(Fail),
      `Failure Count`    = sum(Fail),
      Count     = n()
      ) %>%
    mutate_at(
      vars(Mean, SD, `Failure Rate`),
      round,
      digits = 2
    )
  
}

# Student level
statistics_student <- d_transcript %>%
  group_by(`Student ID`) %>%
  provide_statistics()

# Course level
statistics_course <- d_transcript %>%
  inner_join(d_course, by = c("Course ID")) %>%
  group_by(`Course ID`) %>%
  provide_statistics()

# Cluster level
statistics_cluster <- d_transcript %>%
  inner_join(d_course, by = "Course ID") %>%
  filter(!is.na(Cluster)) %>%
  group_by(Cluster) %>%
  provide_statistics()

# Concentration evel
statistics_concentration <- d_transcript %>%
  inner_join(d_course, by = "Course ID") %>%
  gather(X, Concentration, Concentration, `Concentration (additional)`, na.rm = TRUE) %>%
  group_by(Concentration) %>%
  provide_statistics()

# Year level
statistics_year <- d_transcript %>%
  group_by(Year_numerical) %>%
  provide_statistics()

# Level level
statistics_level <- d_transcript %>%
  # TODO: filter for student who completed their studies
  inner_join(d_course, by = c("Course ID")) %>%
  filter(!is.na(Level)) %>%
  group_by(Level) %>%
  provide_statistics()

#
# output
save(statistics_student, statistics_course, statistics_cluster,
     statistics_concentration, statistics_year, statistics_level, file = "Output/Transcript Statistics.RDATA")

rm(provide_statistics, statistics_student, statistics_course, statistics_cluster,
     statistics_concentration, statistics_year, statistics_level)
```

Association Rules and Sequence Rules
====================================

For a first exploration of arules, we conceptualise our framework like this: transaction = student item = course

Data Prep: Creating Transactions and Sequences
----------------------------------------------

First we transform our data into transaction data. For this, we first create a vector of mandatory courses that we exclude from transcripts.

``` r
#
# Threshold: pass grade, high grade
pass_grade <- 5.5
high_grade <- 6.5



#
# Transactions
d_transactions <- d_course %>%
  
  # Exclude 
  filter(
    Type != "Mandatory",                  # (i) mandatory courses e.g. COR, CAP, etc
    ! Letters %in% c("SKI", "PRO",        # (ii) skills & projects (taken by majority of students)
                     "SAS", "SAH", "SAC") # (iii) courses of semester abroad (uninformative)
    ) %>%
  
  # Join with transcripts.
  select(- Period) %>%
  inner_join(d_transcript, by = "Course ID") %>%
  
  # Identifying sequences
  rename(sequenceID = `Student ID`) %>%
  
  # Identifying time of event (sequence)
  mutate(
    Period  = substr(Period, 1, 1),
    eventID = as.numeric(paste(Year_numerical, Period, sep = ""))
    ) %>%

  # Identifying item
  mutate(
    PF = case_when( Grade <  pass_grade ~ "fail",
                    Grade >= pass_grade ~ "pass"),
    HL = case_when( Grade <  high_grade ~ "low",
                    Grade >= high_grade ~ "high"),
    
    item_PF = paste(`Course ID`, PF, sep = "_"),
    item_HL = paste(`Course ID`, HL, sep = "_"),
    item_Grade = paste(`Course ID`, round(Grade,0), sep = "_")
    ) %>%
  rename(
    item = `Course ID`
  )
```

### Adding not taken courses

``` r
d_transactions_not_taken <- expand.grid(
  
  # Expand along students (sequenceID) and courses (itemID)
  sequenceID = unique(d_transactions$sequenceID),
  item       = unique(d_transactions$item),
  stringsAsFactors = FALSE
  ) %>%
  
  # Join with d_transactions
  left_join(
    d_transactions, 
    by = c("sequenceID", "item")
    ) %>%
  
  # Create 
  mutate(
    
    taken = case_when(
      is.na(Grade) ~ "not taken",
      TRUE         ~ "taken"),
    
    item_taken= paste(item, taken, sep = "_")
    
     )
```

### Expanded Transcripts

``` r
d_transactions_expanded <- d_transactions %>%
  mutate(Round_grade = round(Grade, 0),
         Values = T) %>%
  spread(key = "Round_grade", value = Values) %>%
  mutate(`1`  = `0`|`1`,
         `2`  = `1`|`2`,
         `3`  = `2`|`3`,
         `4`  = `3`|`4`,
         `5`  = `4`|`5`,
         `6`  = `5`|`6`,
         `7`  = `6`|`7`,
         `8`  = `7`|`8`,
         `9`  = `8`|`9`,
         `10`  = `9`|`10`
         ) %>%
  gather(key = "Grade_round", value = "Gr_bool", `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, na.rm = TRUE, convert = TRUE) %>%
  select(-Gr_bool) %>%
  arrange(
    sequenceID,
    item
  ) %>%

  # Identifying item
  mutate(
    PF = case_when( Grade_round <  pass_grade ~ "fail",
                    Grade_round >= pass_grade ~ "pass"),
    HL = case_when( Grade_round <  high_grade ~ "low",
                    Grade_round >= high_grade ~ "high"),
    
    item_PF = paste(item, PF, sep = "_"),
    item_HL = paste(item, HL, sep = "_"),
    item_Grade = paste(item, Grade_round, sep = "_")
    )
```

### Transactions for Apriori

``` r
#
# For convenience
make_transaction <- function(data = d_transactions, item = item){
  
  data <- data %>%
    group_by(
      sequenceID
    ) %>%
    summarise(
      list_item = list(!!enquo(item))
    ) %>%
    ungroup
  
  transactions <- as(
    data$list_item,
    "transactions"
    )
  
  return(transactions)
  
}


#
# Making transactions
transactions                  <- make_transaction(item = item   )
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_PF               <- make_transaction(item = item_PF)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_HL               <- make_transaction(item = item_HL)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_Grade            <- make_transaction(item = item_Grade)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_Taken            <- make_transaction(data = d_transactions_not_taken, item = item_taken)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_expanded         <- make_transaction(data = d_transactions_expanded, item = item)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_expanded_PF      <- make_transaction(data = d_transactions_expanded, item = item_PF)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_expanded_HL      <- make_transaction(data = d_transactions_expanded, item = item_HL)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
transactions_expanded_Grade   <- make_transaction(data = d_transactions_expanded, item = item_Grade)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
#
# Checking transactions
#inspect(head(transactions_Grade, 10))

rm(make_transaction)
```

### Sequences for CSPADE

``` r
#
# For convenience
make_sequence <- function(data = d_transactions, item = item){
  
  data <- data %>%
    filter(!is.na(eventID))%>% #this is here casue we have courses that were never taken.
    group_by(
      sequenceID,
      eventID
    ) %>%
    summarise(
      list_item = list(!!enquo(item))
    ) %>%
    arrange(
      sequenceID,
      eventID
    ) %>%
    ungroup
  
  sequences <- as(
    data$list_item,
    "transactions"
    )
  
  sequences@itemsetInfo <- select(
    data,
    sequenceID,
    eventID
    ) %>%
    as.data.frame()
  
  return(sequences)
  
}


#
# Making sequences
sequences          <- make_sequence(item = item   )
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
sequences_PF       <- make_sequence(item = item_PF)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
sequences_HL       <- make_sequence(item = item_HL)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
sequences_Grade    <- make_sequence(item = item_Grade)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
sequences_expanded        <- make_sequence(data = d_transactions_expanded, item = item)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
sequences_expanded_PF     <- make_sequence(data = d_transactions_expanded, item = item_PF)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
sequences_expanded_HL     <- make_sequence(data = d_transactions_expanded, item = item_HL)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
sequences_expanded_Grade  <- make_sequence(data = d_transactions_expanded, item = item_Grade)
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
#
# Checking sequences
#inspect(head(sequences, 10))


rm(make_sequence)
```

Mining Rules
------------

### Apriori Algorithm

``` r
# Transform AR from apriori into a readable data frame
clean_AR <- function(AR){
  
  AR %>%
    as("data.frame") %>%
    filter(
      count >= 1
    ) %>%
    mutate(
      rules = str_remove_all(rules, pattern = "[{]"),
      rules = str_remove_all(rules, pattern = "[}]"),
      `rhs.support` = confidence / lift
    ) %>%
    separate(rules, into = c("lhs", "rhs"), sep="=>") %>%
    mutate_at(
      c("support", "lhs.support", "confidence", "rhs.support", "lift"),
      funs(round(., 5))
    ) %>%
    select(
      lhs, rhs,
      support, count, lhs.support,
      confidence, rhs.support,
      lift
    ) %>%
    arrange(
      desc(count)
    )
  
}
```

We apply Apriori algorithm:

``` r
# Run the apriori algorithm with desired parameters
my_apriori <- function(data, appearance = NULL){
  
  data %>%
    apriori(
      parameter = list(
        supp = 0,    # min support
        smax = 1,    # max support
        conf = 0,    # min confidence
        minlen = 2,  # min length of 
        maxlen = 2,  # max length of rule
        ext = TRUE,
        arem = "diff",
        aval = TRUE,
        minval = 0
        ),
      appearance = appearance,
      control = list(
        verbose = FALSE
        )
      ) %>%
    clean_AR
  
}


#
# Generating asocciation rules
AR_taken <- my_apriori(transactions)

# creating vector of fail course
course_id_fail <- d_transactions %>%
  filter(PF == "fail") %>%
  distinct(`item_PF`)

AR_PF <- my_apriori(transactions_PF,
                    appearance = list(both = course_id_fail$`item_PF`))

# creating vector of low score course
course_id_low <- d_transactions %>%
  filter(HL == "low") %>%
  distinct(`item_HL`)

AR_HL <- my_apriori(transactions_HL,
                    appearance = list(both = course_id_low$`item_HL`))



AR_not_taken <- my_apriori(transactions_Taken)


AR_expanded_taken <- my_apriori(transactions_expanded)
# creating vector of fail course
course_id_fail <- d_transactions_expanded %>%
  filter(PF == "fail") %>%
  distinct(`item_PF`)

AR_expanded_PF <- my_apriori(transactions_expanded_PF,
                    appearance = list(both = course_id_fail$`item_PF`))

# creating vector of low score course
course_id_low <- d_transactions_expanded %>%
  filter(HL == "low") %>%
  distinct(`item_HL`)

AR_expanded_HL <- my_apriori(transactions_expanded_HL,
                    appearance = list(both = course_id_low$`item_HL`))



#
# Save association rules
save(AR_taken, AR_PF, AR_HL, AR_not_taken, 
     AR_expanded_taken, AR_expanded_PF, AR_expanded_HL,
     file = "App/AR.RDATA")

rm(clean_AR, my_apriori,
   transactions, transactions_PF, transactions_HL, transactions_Grade,
   course_id_fail, course_id_low,
   AR_taken, AR_PF, AR_HL, AR_not_taken, AR_expanded_taken, AR_expanded_PF, AR_expanded_HL)
```

### CSPADE Algorithm

TODO: add AR\_grade, SR\_grade
------------------------------

``` r
# Transform rules from ruleInduction into a readable data frame
n_students <- length(unique(d_transactions$sequenceID))

clean_SR<- function(rules){
  
  rules %>%
    as("data.frame") %>%
    mutate(
      rule = str_remove_all(rule, pattern = "[<]"),
      rule = str_remove_all(rule, pattern = "[{]"),
      rule = str_remove_all(rule, pattern = "[}]"),
      rule = str_remove_all(rule, pattern = "[>]")
      ) %>%
    separate(rule, into = c("lhs", "rhs"), sep="=")  %>%
    select(
      lhs, rhs,
      support,
      confidence,
      lift
    ) %>%
    separate(lhs, into=c("LHS Course ID", "LHS Quality"), sep="_", remove= F) %>%
    separate(rhs, into=c("RHS Course ID", "RHS Quality"), sep="_", remove= F) %>%
    # Compute statistic
    group_by(lhs,`RHS Course ID`) %>%
    rename(confidence.old = "confidence", lift.old="lift")%>%
    mutate(
      lhs.rhsCourse.support = sum(support),
      confidence = support / lhs.rhsCourse.support,
      count = support * n_students,
      lhs.rhsCourse.count = lhs.rhsCourse.support * n_students
      
      # TODO: compute rhs.support
      # lift = confidence.corr / rhs.support
     
  ) %>%
  ungroup() %>%
    mutate_at(
      c("support",
        "confidence","confidence.old", "lift.old"),
      funs(round(., 5))
    )
    
}
```

We apply cspade followed by rule induction

``` r
# Run the cspade algorithm with desired parameters. Then run the ruleInduction algorithm with desired parameters.
my_SR<- function(data){
 data %>%
  cspade(
    parameter = list(
      support = 0, # min support of a sequence
      maxsize = 1, # max number of items of an element of a sequence
      maxlen  = 2, # max number of element of a sequence
      mingap  = 1, # min time difference between consecutive element of a sequence
      maxgap  = 1e4 # max time difference between consecutive element of a sequence
      #maxwin  = 1e4  # max time difference between any two elements of a sequence # WARNING: 'maxwin' disabled
      ),
    control = list(
      verbose = FALSE
      )
    ) %>%
    ruleInduction(
      confidence = 0, #we need to play with the parameters.
      control    = list(verbose = FALSE)
      ) %>%
    clean_SR
}

#Generating sequence rules:
SR_taken   <- my_SR(data = sequences   )
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 14390
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 14390
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

``` r
##pass/fail filter
SR_PF      <- my_SR(data = sequences_PF) %>% 
  filter(`RHS Quality`=="fail")

##high/low filter
SR_HL      <- my_SR(data = sequences_HL)%>% 
  filter(`RHS Quality`=="low")

SR_expanded_taken   <- my_SR(data = sequences_expanded   )
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 14390
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 14390
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    ## 20, ...].

``` r
##pass/fail filter
SR_expanded_PF      <- my_SR(data = sequences_expanded_PF)%>% 
  filter(`RHS Quality`=="fail")
##high/low filter
SR_expanded_HL      <- my_SR(data = sequences_expanded_HL)%>% 
  filter(`RHS Quality`=="low")

#Saving sequence rules
save(
    SR_taken, SR_PF, SR_HL,
    
    SR_expanded_taken, SR_expanded_PF, SR_expanded_HL,
    
    file = "App/SR.RDATA")

#clean unnecessary objects
rm(clean_SR, my_SR, 
   sequences, sequences_PF, sequences_HL, sequences_Grade,
   sequences_expanded, sequences_expanded_PF, sequences_expanded_HL, sequences_expanded_Grade,
   n_students,
   SR_taken, SR_PF, SR_HL,
   SR_expanded_taken, SR_expanded_PF, SR_expanded_HL)
```