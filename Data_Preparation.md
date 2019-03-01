Data Preparation
================
DARS
2019-03-01

-   [Import Data](#import-data)
    -   [Setup](#setup)
    -   [Course Data](#course-data)
    -   [Textual Data](#textual-data)
        -   [Course Catalogues](#course-catalogues)
        -   [Course Manuals](#course-manuals)
    -   [Student Data](#student-data)
-   [Variable Engineering](#variable-engineering)
    -   [Course Data](#course-data-1)
        -   [AoD](#aod)
        -   [Courses](#courses)
    -   [Textual Data](#textual-data-1)
        -   [Extracting Text](#extracting-text)
        -   [Tidy Text Format](#tidy-text-format)
        -   [Stemming](#stemming)
        -   [Removing Stopwords](#removing-stopwords)
    -   [Student Data](#student-data-1)
-   [Save Data](#save-data)
-   [Extra](#extra)
    -   [Extracting Descriptions](#extracting-descriptions)

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.4.2

    ## Warning: package 'ggplot2' was built under R version 3.4.4

    ## Warning: package 'tibble' was built under R version 3.4.4

    ## Warning: package 'tidyr' was built under R version 3.4.4

    ## Warning: package 'readr' was built under R version 3.4.4

    ## Warning: package 'purrr' was built under R version 3.4.4

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## Warning: package 'stringr' was built under R version 3.4.4

    ## Warning: package 'forcats' was built under R version 3.4.3

``` r
library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 3.4.4

``` r
library(gsheet) # import spreadsheets from google drive
library(tm)
```

    ## Warning: package 'tm' was built under R version 3.4.4

    ## Warning: package 'NLP' was built under R version 3.4.4

``` r
library(hunspell) # Stemmer
```

    ## Warning: package 'hunspell' was built under R version 3.4.4

Import Data
===========

The datasets we use are saved as spreadsheet on our google drive *DARS* (with exeption of grade data saved as csv files on the computer for privacy reasons). We use the function `gsheet2tbl` to import them to `R Studio` as tibbles. We use the tibble data format (an evolution of the data frame format) because this is the format of reference of the `tidyverse` on whose tools our analysis is heavily based.

Setup
-----

First, we import the spreadsheet with information pertraining the Aims of the Degree (AoD) and Assessments from the drive and save it under `lists_brut`. `lists_brut` contains 4 columns, under which we find the `19` types of assessment, the `18` aims of the degree (AoD) of the degree, and two columns containing binary vectors indicating which assessment types and AoD we will consider when ploting the data\[^1\]. Then we create a list with this same columns, but instead of having binary vectors for the plots, we keep vectors of only the names of relevant assessments and AoDs for the plots (`Assessment_plot`adnd `AoD_plot` respectively). We also any imported emtpy cells.

``` r
lists_brut <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1239912347') %>%
  map(na.omit)

lists <- list()

lists$Assessment      <- lists_brut$Assessment
lists$AoD             <- lists_brut$Aim

# selection of most important types of assessment an AoD to keep plots clear
lists$Assessment_plot <- lists$Assessment[lists_brut$`Assessment for Plot` == 1]
lists$AoD_plot        <- lists$AoD       [lists_brut$`Aim for Plot`        == 1]


rm(lists_brut)
```

Course Data
-----------

We import three spreadsheets from the drive and transform them into the so-called *tidy format*. The tibble `d_course` contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered. The tibble `d_assessment` indicates which type(s) of assessment each course contains with one row per course-assessment; and the tibble `d_ILO` indicates which AoD(s) the intended learning objectives (ILOs) of the courses cover with one row per course-ILO-AoD.

``` r
d_course <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1655700848') %>%
  print
```

    ## # A tibble: 280 x 11
    ##    `Course ID` `Course Title` Period `Period (additi~ `Period (additi~
    ##    <chr>       <chr>          <chr>  <chr>            <lgl>           
    ##  1 AAM2001     Academic Advi~ Perio~ <NA>             NA              
    ##  2 AAM2002     Academic Advi~ Perio~ <NA>             NA              
    ##  3 AAM2003     Academic Advi~ Perio~ <NA>             NA              
    ##  4 AAM2004     Academic Advi~ Perio~ <NA>             NA              
    ##  5 AAM2005     Academic Advi~ Perio~ <NA>             NA              
    ##  6 AAM2006     Academic Advi~ Perio~ <NA>             NA              
    ##  7 AAM2007     Academic Advi~ Perio~ <NA>             NA              
    ##  8 CAP3000     Capstone       Perio~ Period 4         NA              
    ##  9 COR1002     Philosophy of~ Perio~ Period 5         NA              
    ## 10 COR1003     Contemporary ~ Perio~ Period 4         NA              
    ## # ... with 270 more rows, and 6 more variables: Concentration <chr>,
    ## #   `Concentration (additional)` <chr>, Cluster <chr>, `Missing from ILO
    ## #   File` <dbl>, `Most Recent Catalogue` <chr>, Type <chr>

``` r
d_assessment <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1102665750') %>%
  mutate_if(
    .predicate = is.numeric,
    as.logical
    ) %>%
  gather(
    key   = Assessment, 
    value = assessment_covered,
    Paper : Participation
    ) %>%
  filter(
    assessment_covered
    ) %>%
  select(
    - c(assessment_covered, `Comment on Assessment`)
    ) %>%
  arrange(
    `Course ID`
    ) %>%
  print
```

    ## # A tibble: 482 x 2
    ##    `Course ID` Assessment    
    ##    <chr>       <chr>         
    ##  1 AAM2001     Oral          
    ##  2 AAM2002     Oral          
    ##  3 AAM2003     Oral          
    ##  4 AAM2004     Oral          
    ##  5 AAM2005     Oral          
    ##  6 AAM2006     Oral          
    ##  7 AAM2007     Oral          
    ##  8 CAP3000     Paper         
    ##  9 CAP3000     Presentation  
    ## 10 CAP3000     Research Prop.
    ## # ... with 472 more rows

``` r
d_ILO <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1896457050')


d_AoD_ILO <- d_ILO %>%
  rename(
    ILO = Objectives
    ) %>%
  mutate_if(
    .predicate = is.numeric,
    as.logical
    ) %>%
  gather(
    key   = AoD, 
    value = AoD_covered,
    `Matrix Complete` : `Intercultural Skills`
    ) %>%
  filter(
    AoD_covered
    ) %>%
  select(
    - c(AoD_covered, Comments, Comments_for_Jeroen)
    ) %>%
  print
```

    ## # A tibble: 1,232 x 3
    ##    `Course ID` ILO                                                    AoD  
    ##    <chr>       <chr>                                                  <chr>
    ##  1 COR1002     1. To know the major approaches in the philosophy of ~ Core 
    ##  2 COR1002     2. To have knowledge of the major problems or topics ~ Core 
    ##  3 COR1002     3. To have knowledge of a number of specific problems~ Core 
    ##  4 COR1003     1. To understand the main trends in politics, demogra~ Core 
    ##  5 COR1003     2. To develop a critical attitude towards the interpr~ Core 
    ##  6 COR1003     3. To Develop a critical understanding concerning the~ Core 
    ##  7 COR1004     1. To provide the students with a basic understanding~ Core 
    ##  8 COR1004     2. To understand the central concepts like justice an~ Core 
    ##  9 COR1005     1. To offer a broad overview of scientific models and~ Core 
    ## 10 COR1005     2. To teach students how to work with models in diffe~ Core 
    ## # ... with 1,222 more rows

Textual Data
------------

### Course Catalogues

In order to conduct a preliminary topic modeling of course content, we first analyze their description in the course catalogues. The corpus `corpus_catalogues` contains the pdfs of the `5` most recent course catalogues.

``` r
d_text <- list()

d_text$catalogues <- Corpus(
  x             = DirSource("./Input/Catalogues"),
  readerControl = list(reader = readPDF(control = list(text = "-layout")))
  )
```

### Course Manuals

To expand our topic modeling of course content, we analyse the material in the course manuals. The `corpus_manuals` contains the pdfs of the course manuals for the year 2017-2018 (most recently, available).

``` r
d_text$manuals <- Corpus(
  x             = DirSource("./Input/Manuals 2017-18"),
  readerControl = list(reader = readPDF(control = list(text = "-layout")))
  )
```

Student Data
------------

The tibble `d_transcript` contains the transcript information of students as was provided. It has 40 columns, and rows correspond to a type of grade (e.g. final grade, attendance) per student per course per time they took it.

``` r
d_transcript1 <- read_csv(
  file = "Input/Raw Grades/grades1.csv",
  col_types = cols(
    `Program (Abbreviation)`              = "c",
    `Appraisal Status`                    = "c",
    `Module Booking Reason (Description)` = "c"
    )
  )

d_transcript2 <- read_csv(
  file = "Input/Raw Grades/grades2.csv",
  col_types = cols(
    `Program (Abbreviation)`              = "c",
    `Appraisal Status`                    = "c",
      `Object name`                       = "c",
      `Start date`                        = "c",
      `End Date`                          = "c"
    )
  )

d_transcript <- rbind(d_transcript1, d_transcript2)

rm(d_transcript1, d_transcript2)
```

Variable Engineering
====================

Course Data
-----------

The analysis performed on the course data is aimed at discoving what the contribution of each course is towards the fullfilment of the AoDs, and comparing different types of curricula, or programs, based on this infromation.

### AoD

In this analysis, we conside that a course can cover an AoD in two ways: a course covers an AoD if one of its ILOs covers it, or if one of its assessments cover it. For instance, if one of ILO of a course states that the students will learn to analyze empirical data in the context of academic research, then the course in question covers the AoD `Research Skills`; and if one of the assessment is a group presentation, then the course also covers the AoD `Collaborative Skills` and `Communication Skills`.

#### AoD from ILOs

(For this section we use: `d_ILO`-indicates which AoD(s) the intended learning objectives (ILOs) of the courses cover with one row per course-ILO-AoD)

In order to determine which AoD each course covers with its ILOs, we first eliminate the AoD that are not covered by the ILOs, and then we keep only one instance of each combination of course and AoD in case a course had several ILOs covering the same AoD.

``` r
d_AoD_ILO
```

    ## # A tibble: 1,232 x 3
    ##    `Course ID` ILO                                                    AoD  
    ##    <chr>       <chr>                                                  <chr>
    ##  1 COR1002     1. To know the major approaches in the philosophy of ~ Core 
    ##  2 COR1002     2. To have knowledge of the major problems or topics ~ Core 
    ##  3 COR1002     3. To have knowledge of a number of specific problems~ Core 
    ##  4 COR1003     1. To understand the main trends in politics, demogra~ Core 
    ##  5 COR1003     2. To develop a critical attitude towards the interpr~ Core 
    ##  6 COR1003     3. To Develop a critical understanding concerning the~ Core 
    ##  7 COR1004     1. To provide the students with a basic understanding~ Core 
    ##  8 COR1004     2. To understand the central concepts like justice an~ Core 
    ##  9 COR1005     1. To offer a broad overview of scientific models and~ Core 
    ## 10 COR1005     2. To teach students how to work with models in diffe~ Core 
    ## # ... with 1,222 more rows

#### AoD from Assessment

(For this section we use: `d_assessment`-indicates which type(s) of assessment each course contains with one row per course-assessment)

In order to determine which AoD each course covers with its assessments, we need to create a binary matrix which indicates which AoD(s) each assessment type covers. We have created such matrix on our google drive and we save it in the following piece of code as `map_assessment_AoD`. `map_assessment_AoD` indicates that, for instance, the assessment type `Essay` covers the AoD `Critical Thinking Skills`, `Communication Skills` and `Writing Skill`. Thus, the first step is to import this matrix:

``` r
map_assessment_AoD <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=719531216') %>%
  mutate_if(
    .predicate = is.numeric,
    .f         = as.logical
    ) %>%
  gather(
    key   = AoD, 
    value = AoD_covered,
    `Matrix Complete` : `Intercultural Skills`
    ) %>%
  filter(
    AoD_covered
    ) %>%
  select(
    - AoD_covered
    ) %>%
  arrange(
    Assessment
  ) %>%
  print
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

    ## # A tibble: 39 x 2
    ##    Assessment        AoD                     
    ##    <chr>             <chr>                   
    ##  1 Artistic Creation Communication Skills    
    ##  2 Assignments       Problem Solving Skills  
    ##  3 Debate            Critical Thinking Skills
    ##  4 Debate            Communication Skills    
    ##  5 Essay             Critical Thinking Skills
    ##  6 Essay             Communication Skills    
    ##  7 Essay             Writing Skills          
    ##  8 Feedback          Critical Thinking Skills
    ##  9 Feedback          Communication Skills    
    ## 10 Group             Communication Skills    
    ## # ... with 29 more rows

Now that we have the matrix `map_assessment_AoD`, we want to find out which AoDs are covered by a course through its assessments. To do this we create an empty tibble `d_AoD_assessment` to store which assessment is covered by each course, and which AoD said assessment covers.Then, we fill in the information with a loop. In the loop, we first extract a row of `d_assessment` and save it as `observation`. Then, we determine the corresponding assessment which we save as `assessment`. Then, we use the matrix `map_assessment_AoD` to determine which AoD `assessment` covers and use `cbind` and `rbind` to add the information to the tibble `d_AoD_assessment`.

``` r
d_AoD_assessment <- d_assessment %>%
  left_join(
    map_assessment_AoD,
    by = "Assessment"
    ) %>%
  print
```

    ## # A tibble: 1,192 x 3
    ##    `Course ID` Assessment AoD                     
    ##    <chr>       <chr>      <chr>                   
    ##  1 AAM2001     Oral       Communication Skills    
    ##  2 AAM2002     Oral       Communication Skills    
    ##  3 AAM2003     Oral       Communication Skills    
    ##  4 AAM2004     Oral       Communication Skills    
    ##  5 AAM2005     Oral       Communication Skills    
    ##  6 AAM2006     Oral       Communication Skills    
    ##  7 AAM2007     Oral       Communication Skills    
    ##  8 CAP3000     Paper      Problem Solving Skills  
    ##  9 CAP3000     Paper      Critical Thinking Skills
    ## 10 CAP3000     Paper      Communication Skills    
    ## # ... with 1,182 more rows

``` r
rm(map_assessment_AoD)
```

#### Combining AoD from ILOs and from assessments

Finally, we can use a `rbind` to combine the two tibbles `d_AoD_ILO` and `d_AoD_assessment`. We also use `distinct` in case a course covers an AoD with both its ILOS and its assessments.

``` r
d_AoD <- rbind(
  d_AoD_ILO        %>% select(`Course ID`, AoD),
  d_AoD_assessment %>% select(`Course ID`, AoD)
  ) %>%
  distinct %>%
  arrange(
    `Course ID`
    ) %>%
  print
```

    ## # A tibble: 1,330 x 2
    ##    `Course ID` AoD                   
    ##    <chr>       <chr>                 
    ##  1 AAM2001     Academic Expertise    
    ##  2 AAM2001     Reflective Skills     
    ##  3 AAM2001     Decision-making Skills
    ##  4 AAM2001     Communication Skills  
    ##  5 AAM2002     Academic Expertise    
    ##  6 AAM2002     Reflective Skills     
    ##  7 AAM2002     Decision-making Skills
    ##  8 AAM2002     Communication Skills  
    ##  9 AAM2003     Academic Expertise    
    ## 10 AAM2003     Reflective Skills     
    ## # ... with 1,320 more rows

``` r
rm(d_AoD_assessment, d_AoD_ILO)
```

### Courses

(In this section we use: `d_course`- contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered.)

Now that we have a clear overview of the distribution of AoDs (`d_AoD`) and assessments (`d_assessment`) among the courses, let us add variables to the tibble `d_course` that contain the information at the course level. For this we create the following three tibbles: 1) `d_ILO_detail` contains two columns indicating the code of the course and the number of ILOs it contains. 2) `d_assessment_detail` contains three columns indicating the code of the course, the number of assessments it covers and a list of the assessments it covers. 3) `d_AoD_detail` contains three columns indicating the code of the course, the number of AoD it covers and a list of the AoD it covers. Then we use a `full_join` to add these variables to the tibble `d_course`.

``` r
d_ILO_detail <- d_ILO %>%
  count(
    `Course ID`, 
    sort = TRUE
    ) %>%
  rename(
    n_ILO = n
    ) %>%
  print
```

    ## # A tibble: 253 x 2
    ##    `Course ID` n_ILO
    ##    <chr>       <int>
    ##  1 SCI2012        12
    ##  2 SSC2006        12
    ##  3 SKI1005        11
    ##  4 SCI2009        10
    ##  5 SSC3030        10
    ##  6 HUM1012         8
    ##  7 SCI2017         8
    ##  8 SKI1004         8
    ##  9 PRO3008         7
    ## 10 SSC2022         7
    ## # ... with 243 more rows

``` r
d_assessment_detail <- d_assessment %>%
  group_by(
    `Course ID`
    ) %>%
  summarise(
    n_assessment = n(),
    `Assessments Covered` = paste(Assessment, collapse = ", ")
    ) %>%
  print
```

    ## # A tibble: 190 x 3
    ##    `Course ID` n_assessment `Assessments Covered`              
    ##    <chr>              <int> <chr>                              
    ##  1 AAM2001                1 Oral                               
    ##  2 AAM2002                1 Oral                               
    ##  3 AAM2003                1 Oral                               
    ##  4 AAM2004                1 Oral                               
    ##  5 AAM2005                1 Oral                               
    ##  6 AAM2006                1 Oral                               
    ##  7 AAM2007                1 Oral                               
    ##  8 CAP3000                3 Paper, Presentation, Research Prop.
    ##  9 COR1002                2 Essay, Written Exam                
    ## 10 COR1003                3 Paper, Written Exam, Participation 
    ## # ... with 180 more rows

``` r
d_AoD_detail <- d_AoD %>%
  group_by(
    `Course ID`
    ) %>%
  summarise(
    n_AoD = n(),
    `AoD Covered` = paste(AoD, collapse = ", ")
    ) %>%
  print
```

    ## # A tibble: 280 x 3
    ##    `Course ID` n_AoD `AoD Covered`                                         
    ##    <chr>       <int> <chr>                                                 
    ##  1 AAM2001         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  2 AAM2002         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  3 AAM2003         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  4 AAM2004         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  5 AAM2005         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  6 AAM2006         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  7 AAM2007         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  8 CAP3000        10 Advanced Knowledge, Academic Expertise, Graduate Stud~
    ##  9 COR1002         8 Core, Elementary Knowledge, Reflective Skills, Ethica~
    ## 10 COR1003         8 Core, Elementary Knowledge, Critical Thinking Skills,~
    ## # ... with 270 more rows

``` r
d_course <- d_course %>%
  left_join(d_ILO_detail       , by = "Course ID") %>%
  left_join(d_assessment_detail, by = "Course ID") %>%
  left_join(d_AoD_detail       , by = "Course ID") %>%
  select(
    `Course ID`, `Course Title`, Cluster,
    n_ILO,
    n_assessment, `Assessments Covered`,
    n_AoD, `AoD Covered`,
    everything()
    ) %>%
  print
```

    ## # A tibble: 280 x 16
    ##    `Course ID` `Course Title` Cluster n_ILO n_assessment `Assessments Co~
    ##    <chr>       <chr>          <chr>   <int>        <int> <chr>           
    ##  1 AAM2001     Academic Advi~ <NA>        1            1 Oral            
    ##  2 AAM2002     Academic Advi~ <NA>        1            1 Oral            
    ##  3 AAM2003     Academic Advi~ <NA>        1            1 Oral            
    ##  4 AAM2004     Academic Advi~ <NA>        1            1 Oral            
    ##  5 AAM2005     Academic Advi~ <NA>        1            1 Oral            
    ##  6 AAM2006     Academic Advi~ <NA>        1            1 Oral            
    ##  7 AAM2007     Academic Advi~ <NA>        1            1 Oral            
    ##  8 CAP3000     Capstone       <NA>        3            3 Paper, Presenta~
    ##  9 COR1002     Philosophy of~ Philos~     3            2 Essay, Written ~
    ## 10 COR1003     Contemporary ~ History     3            3 Paper, Written ~
    ## # ... with 270 more rows, and 10 more variables: n_AoD <int>, `AoD
    ## #   Covered` <chr>, Period <chr>, `Period (additional)` <chr>, `Period
    ## #   (additional bis)` <lgl>, Concentration <chr>, `Concentration
    ## #   (additional)` <chr>, `Missing from ILO File` <dbl>, `Most Recent
    ## #   Catalogue` <chr>, Type <chr>

``` r
rm(d_ILO_detail, d_assessment_detail, d_AoD_detail)
```

The undegraduate research courses (*UGR-*) are only present at the `3000` level (advanced level). Yet, these course are also offered at the `2000` level (intermediate level). We use an `rbind` to duplicate the rows of the course `UGR3000` and mutate their `Code` to `UGR2000`.

``` r
d_course_UGR_2000 <- d_course %>%
  filter(
    `Course ID` %in% c("UGR3001", "UGR3002", "UGR3003", "UGR3005")
    ) %>%
  mutate(
    `Course ID` = case_when(
      `Course ID` == "UGR3001" ~ "UGR2001",
      `Course ID` == "UGR3002" ~ "UGR2002",
      `Course ID` == "UGR3003" ~ "UGR2003",
      `Course ID` == "UGR3005" ~ "UGR2005"
      )
    ) %>%
  print
```

    ## # A tibble: 4 x 16
    ##   `Course ID` `Course Title` Cluster n_ILO n_assessment `Assessments Co~
    ##   <chr>       <chr>          <chr>   <int>        <int> <chr>           
    ## 1 UGR2001     Undergraduate~ Methods     5            2 Paper, Presenta~
    ## 2 UGR2002     Undergraduate~ Skills     NA            6 Paper, Essay, P~
    ## 3 UGR2003     Applied Resea~ Skills     NA            5 Paper, Presenta~
    ## 4 UGR2005     Artistic Rese~ Methods    NA            2 Paper, Research~
    ## # ... with 10 more variables: n_AoD <int>, `AoD Covered` <chr>,
    ## #   Period <chr>, `Period (additional)` <chr>, `Period (additional
    ## #   bis)` <lgl>, Concentration <chr>, `Concentration (additional)` <chr>,
    ## #   `Missing from ILO File` <dbl>, `Most Recent Catalogue` <chr>,
    ## #   Type <chr>

``` r
d_course <- d_course %>%
  rbind(
    d_course_UGR_2000
    )

print(
  d_course %>%
    filter(
      str_detect(`Course ID`, "UGR")
      )
  )
```

    ## # A tibble: 8 x 16
    ##   `Course ID` `Course Title` Cluster n_ILO n_assessment `Assessments Co~
    ##   <chr>       <chr>          <chr>   <int>        <int> <chr>           
    ## 1 UGR3001     Undergraduate~ Methods     5            2 Paper, Presenta~
    ## 2 UGR3002     Undergraduate~ Skills     NA            6 Paper, Essay, P~
    ## 3 UGR3003     Applied Resea~ Skills     NA            5 Paper, Presenta~
    ## 4 UGR3005     Artistic Rese~ Methods    NA            2 Paper, Research~
    ## 5 UGR2001     Undergraduate~ Methods     5            2 Paper, Presenta~
    ## 6 UGR2002     Undergraduate~ Skills     NA            6 Paper, Essay, P~
    ## 7 UGR2003     Applied Resea~ Skills     NA            5 Paper, Presenta~
    ## 8 UGR2005     Artistic Rese~ Methods    NA            2 Paper, Research~
    ## # ... with 10 more variables: n_AoD <int>, `AoD Covered` <chr>,
    ## #   Period <chr>, `Period (additional)` <chr>, `Period (additional
    ## #   bis)` <lgl>, Concentration <chr>, `Concentration (additional)` <chr>,
    ## #   `Missing from ILO File` <dbl>, `Most Recent Catalogue` <chr>,
    ## #   Type <chr>

``` r
rm(d_course_UGR_2000)
```

Finally, we add a series of informative variable at the course level.

``` r
d_course <- d_course %>%
  separate(
    col     = `Course ID`,
    into    = c("Letters", "Number"),
    sep     = 3,
    remove  = FALSE,
    convert = TRUE
    ) %>%
  mutate(
    Level   = case_when(
      between(Number, 1000, 1999) ~ "Introductory",
      between(Number, 2000, 2999) ~ "Intermediate",
      between(Number, 3000, 3999) ~ "Advanced"    )
    ) %>%
  select(`Course ID`, Letters, Number, Level, everything()
    ) %>%
  print
```

    ## # A tibble: 284 x 19
    ##    `Course ID` Letters Number Level `Course Title` Cluster n_ILO
    ##    <chr>       <chr>    <int> <chr> <chr>          <chr>   <int>
    ##  1 AAM2001     AAM       2001 Inte~ Academic Advi~ <NA>        1
    ##  2 AAM2002     AAM       2002 Inte~ Academic Advi~ <NA>        1
    ##  3 AAM2003     AAM       2003 Inte~ Academic Advi~ <NA>        1
    ##  4 AAM2004     AAM       2004 Inte~ Academic Advi~ <NA>        1
    ##  5 AAM2005     AAM       2005 Inte~ Academic Advi~ <NA>        1
    ##  6 AAM2006     AAM       2006 Inte~ Academic Advi~ <NA>        1
    ##  7 AAM2007     AAM       2007 Inte~ Academic Advi~ <NA>        1
    ##  8 CAP3000     CAP       3000 Adva~ Capstone       <NA>        3
    ##  9 COR1002     COR       1002 Intr~ Philosophy of~ Philos~     3
    ## 10 COR1003     COR       1003 Intr~ Contemporary ~ History     3
    ## # ... with 274 more rows, and 12 more variables: n_assessment <int>,
    ## #   `Assessments Covered` <chr>, n_AoD <int>, `AoD Covered` <chr>,
    ## #   Period <chr>, `Period (additional)` <chr>, `Period (additional
    ## #   bis)` <lgl>, Concentration <chr>, `Concentration (additional)` <chr>,
    ## #   `Missing from ILO File` <dbl>, `Most Recent Catalogue` <chr>,
    ## #   Type <chr>

Textual Data
------------

### Extracting Text

In order to conduct a topic analysis of course content, we extract the *overview* and *description* of each course from the course catalogues, as well as the *full text* from the Course Manuals.

#### Extractin Text: Course Catalogues

(For this section we use:`corpus_catalogues` contains the pdfs of the `5` most recent course catalogues.)

##### Extracting Overviews

We start by extracting the overviews which are 1 or 2 page long and contain important information of a course. To accomplish this, for each catalogue (loop), we first use `grep` to determine which pages should be excluded from the analysis (`pages_to_exclude`). These pages are typically headers. Excluding them before the analysis allows use to keep the code relatively simple. We also make use the fact that the overviews always start with the code of the course, i.e. it starts with one of the elements of `course_code`; and we use `grep` to identify the first page of each overview. We use grep a 3rd time to identify the page containing the header *Core Coureses (COR)* and which marks the beginning of the overviews in the catalogue, and we loop from this page to the last page of the catalogue.

In the loop, we first determine the content of the current and following page. If the current page is the first page of an overview, then we identify the overview (one or two pages) and save it to the tibble `d_description`. To identify the overview of a course, we check if the following page is either the first page of an overview or a page to exclude. If it is either of these, then the overview correspond to the content of the current page; but if the following page is neiter of these, then the overview correspond to the content of the current page and that of the following page.

``` r
#
# Set up
years <- c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019")

headers <- c(
  "^Core Courses \\(COR\\)",
  "^Humanities \\(HUM\\)",
  "^Sciences \\(SCI\\)",
  "^Social Sciences \\(SSC\\)",
  "^Skills Trainings? \\(SKI\\)",
  "^Projects? \\(PRO\\)",
  "^Undergraduate Research", "UCM Undergraduate\r\nResearch", "UCM Undergraduate\r\n  Research",
  "Appendix"
  ) %>%
  paste(collapse = "|")

overview_start <- c(
  "COR",
  "HUM", "SCI", "SSC", 
  "SKI", "PRO",
  "UGR", "CAP"
  ) %>% 
  paste(collapse = "|")

is_start_overview_section  <- function(string) string %>% str_detect(pattern = "^Core Courses \\(COR\\)")
is_end_overview_section    <- function(string) string %>% str_detect(pattern = "Appendix")
is_header                  <- function(string) string %>% str_detect(pattern = headers)

extract_code               <- function(string) string %>% str_sub(start = 1, end = 7)
is_overview_start          <- function(string) string %>% extract_code %>% str_detect(pattern = overview_start)
paste_if_two_page_overview <- function(page, page_following){
  
  # if the page is the start of an overview,
  if(page %>% is_overview_start){
    
    # then, if the following page is also the start of an overview, the overview is simply the current page
    if(page_following %>% is_overview_start | is.na(page_following)){
      
      page
    
    # if the following page is also the start of an overview, the overview spans over the current and the following page
    }else{
    
    paste(page, page_following)
      
    }
  
  # if the page is not the start of an overview, return an NA
  }else{
   
    NA_character_
    
  }
  
}



#
# Loop

for(i in seq_along(d_text$catalogues)){
  
  catalogue_all <- d_text$catalogues %>%
    
    # extract catalogue from corpus
    .[[i]] %>%
    content %>%
  
    # create a tibble
    tibble(
      page = .
      ) %>%
  
    # identify start and end of overview section of catalogue
    mutate(
      page_n                    = row_number(), 
      is_start_overview_section = page %>% is_start_overview_section,
      is_end_overview_section   = page %>% is_end_overview_section
      )
  
  # Check that there is one and only one is_start_overview_section and no more than 1 is_end_overview_section.
  if(sum(catalogue_all$is_start_overview_section) == 0) print("error: no overview_section_start"      )
  if(sum(catalogue_all$is_start_overview_section) >  1) print("error: multiple overview_section_start")
  if(sum(catalogue_all$is_end_overview_section  ) >  1) print("error: multiple overview_section_end"  )
  
  
  # Extract overview section from catalogue
  catalogue_overview <- catalogue_all %>%
  
    # identify and filter the pages that are in the overview section (is_overview_section)  
    mutate(
      is_after_start_overview_section = cumany( is_start_overview_section),
      is_before_end_overview_section  = cumall(!is_end_overview_section  ),
      is_overview_section             = is_after_start_overview_section & is_before_end_overview_section
      ) %>%
      
    filter(
      is_overview_section
      ) %>%
      
    # exlude headers (is_to_exclude) as well as the first and the last page 
    filter(
      ! page %>% is_header
      ) %>%
      
    select(
      - c(
        is_start_overview_section, is_end_overview_section,
        is_after_start_overview_section, is_before_end_overview_section,
        is_overview_section
        )
      )
  
  d_text$overview[[i]] <- catalogue_overview %>%
    transmute(
      text          = list(page, lead(page)) %>% pmap_chr(paste_if_two_page_overview),
      `Course ID` = text %>% extract_code,
      year          = years[i]
    )
  
}

d_text <- d_text[c("overview", "manuals")]
d_text$overview <- bind_rows(d_text$overview)

rm(
  is_end_overview_section, is_header, is_overview_start, is_start_overview_section, paste_if_two_page_overview,
  headers, i, overview_start, years,
  catalogue_all, catalogue_overview
  )
```

#### Extracting Text: Course Manuals

(For this section we use:`corpus_manuals`-contains the pdfs of the course manuals for the year 2017-2018 (most recently, available))

Since we want to use tools from dplyr and tidytext we must first transform our corpus into a dataframe.

``` r
d_text$manuals <- d_text$manuals %>%
  tidy %>%
  select(
    id, text
    ) %>%
  mutate(
    `Course ID` = id %>% extract_code
    ) %>%
  select(
    -id
    ) %>%
  print
```

    ## # A tibble: 153 x 2
    ##    text                                                         `Course ID`
    ##    <chr>                                                        <chr>      
    ##  1 "P h i l o s o p hy o f S c i e n c e\r\n                  ~ COR1002    
    ##  2 "General information\r\nIntroduction\r\nNew things are happ~ COR1003    
    ##  3 "University College Maastricht. 2017-18, Period 2\r\n      ~ COR1004    
    ##  4 "Theory Construction\r\nand\r\nModelling Techniques\r\nCode~ COR1005    
    ##  5 "  Cultural Studies I\r\nDoing Cultural Studies\r\n Univers~ HUM1003    
    ##  6 "AN INTRODUCTORY COURSE TO PHILOSOPHY\n                    ~ HUM1007    
    ##  7 "                 HU1010\r\nCOMMON FOUNDATIONS OF LAW IN EU~ HUM1010    
    ##  8 "Introduction to Art\r\nRepresentations, Performances and I~ HUM1011    
    ##  9 "Pop Songs and Poetry: Theory and Analysis\r\n            C~ HUM1012    
    ## 10 "\n              The Idea of Europe:\r\nThe Intellectual Hi~ HUM1013    
    ## # ... with 143 more rows

### Tidy Text Format

To put everything into tidy text format, we first create three tibbles `d_overview_tidy`, `d_description_tidy`, `d_manual` that respectively store the overviews, descriptions and text from manuals in the tidy text format, with one row per course-year-word (and course-word for `d_manual`).

``` r
tidy_text <- function(df){
  
  df %>%
    unnest_tokens(
      output = word,
      input  = text
      )
  
}

d_text <- d_text %>% map(tidy_text)
```

### Stemming

We then use the function `hunspell_stem`, which returns valid stems for a given word, to stem the words in the tibbles `d_overview_tidy` and `d_description_tidy`.

We first create a function `stem_hunspell` which, given a term, returns its most basic stem (the last one from the list of stem returned by `hunspell_stem`). We would like to apply `stem_hunspell` on the words of `d_overview_tidy` and `d_manual`, since they have similar structures, we row bind them into `dictionary` to ease the application of `stem_hunspell`. However, since, `stem_hunspell` is not a vectorized function and the number of words in `dictionary` is large, we first use distinct on the `word` variable to find a list containing *once* each term present in the course overviews and manuals. Then, we use `mutate` to apply the function `stem_hunspell` on each word from our dictionary and save its results as a new column `word_stem`.

``` r
stem_hunspell <- function(term) {
  
  stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
  
  n_stems <- length(stems)            # number of stems
  
  if (n_stems == 0) term              # if no stems, return original term
  else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem
  
}

dictionary <- d_text$overview$word %>%
  union(
    d_text$manuals$word
    ) %>%
  tibble(
    word = .
    ) %>%
  mutate(
    word_stem = word %>% map_chr(stem_hunspell)
    )

# TODO: "destem" terms by hand: cuss => discuss
filter(dictionary, word_stem == "cuss")
```

    ## # A tibble: 4 x 2
    ##   word       word_stem
    ##   <chr>      <chr>    
    ## 1 discussed  cuss     
    ## 2 discuss    cuss     
    ## 3 discussing cuss     
    ## 4 discusses  cuss

``` r
# terms for which the stem differs from the original word
filter(dictionary, word != word_stem)
```

    ## # A tibble: 11,129 x 2
    ##    word        word_stem 
    ##    <chr>       <chr>     
    ##  1 humanities  humanity  
    ##  2 sciences    science   
    ##  3 recommended commend   
    ##  4 is          i         
    ##  5 strongly    strong    
    ##  6 objective   object    
    ##  7 students    student   
    ##  8 foundations foundation
    ##  9 starting    start     
    ## 10 positions   position  
    ## # ... with 11,119 more rows

Finally, we create a function `stem_with_dictionary` that performs a left join on the dataframe it takes as input with `dictionary`, thus adding the word stems to the original dataframe.

Then, we use `stem_with_dictionary` to include the stems of all words in `d_description`, `d_overview`, and `d_manual`.

``` r
stem_with_dictionary <- function(data){
  
  data %>%
    left_join(
      dictionary,
      by = "word"
      ) %>%
    rename(
      word_original = word,
      word          = word_stem
      )
  
}

d_text <- d_text %>% map(stem_with_dictionary)

rm(stem_hunspell, stem_with_dictionary)
```

### Removing Stopwords

Finally, we want to filter out some uninformative words from our textual data. For this, we first store all the unwanted words in a vector `sw_owm`. We also create function `remove_sw` which receives a dataframe as input and filters out all words in `sw_own`. Then we apply this function to `d_description`, `d_overview`, and `d_manual`.

``` r
# Own stop words
sw_own <- tibble(
    word = c(
      as.character(1 : 1e3)
      )
    )

remove_sw <- function(data){
  
  data %>%
    anti_join(
      stop_words,
      by = "word"
      ) %>%
    anti_join(
      sw_own,
      by = "word"
      )
  
}

d_text <- d_text %>% 
  map(remove_sw) %>% 
  print
```

    ## $overview
    ## # A tibble: 172,206 x 4
    ##    `Course ID` year      word_original word       
    ##    <chr>       <chr>     <chr>         <chr>      
    ##  1 COR1002     2014-2015 cor1002       cor1002    
    ##  2 COR1002     2014-2015 philosophy    philosophy 
    ##  3 COR1002     2014-2015 science       science    
    ##  4 COR1002     2014-2015 coordinator   coordinator
    ##  5 COR1002     2014-2015 prof          prof       
    ##  6 COR1002     2014-2015 dr            dr         
    ##  7 COR1002     2014-2015 boon          boon       
    ##  8 COR1002     2014-2015 faculty       faculty    
    ##  9 COR1002     2014-2015 humanities    humanity   
    ## 10 COR1002     2014-2015 sciences      science    
    ## # ... with 172,196 more rows
    ## 
    ## $manuals
    ## # A tibble: 518,739 x 3
    ##    `Course ID` word_original word      
    ##    <chr>       <chr>         <chr>     
    ##  1 COR1002     hy            hy        
    ##  2 COR1002     fall          fall      
    ##  3 COR1002     2017          2017      
    ##  4 COR1002     cor           cor       
    ##  5 COR1002     1002          1002      
    ##  6 COR1002     cor           cor       
    ##  7 COR1002     1002          1002      
    ##  8 COR1002     philosophy    philosophy
    ##  9 COR1002     science       science   
    ## 10 COR1002     contents      content   
    ## # ... with 518,729 more rows

``` r
rm(sw_own, remove_sw)
```

Student Data
------------

(In this section we use: `d_transcript`- contains the transcript information of students as was provided. It has 40 columns, and rows correspond roughly to a type of grade (e.g. final grade, attendance) per student per course per time they took it).

Our dataframe contains many variables and rows that are either empty or meaningless for our analysis. First, we filter, the final grades of studets to keep only relevant rows (Final Confirmed Grades are those with `Appraisal (Description)` as "Grade supervisor"). Then we select ony the 10 variables that we will use for the anlysis, and give them more comprehensible names.

``` r
d_transcript <- d_transcript %>%
  
  # for simplicity, only keep courses of UCM program
  filter(
    `Program (Abbreviation)` == "7501",
    !str_detect(`Module (Abbrev.)`, "EXT")
    ) %>%
  
  # folowing guidelines of Richard Vos, we only consider: `Appraisal (Description)` == "Grade supervisor" and `Appraisal Type`          == "7055"
  filter(
    `Appraisal (Description)` == "Grade supervisor",
    `Appraisal Type`          == "7055" # removes ~ 15 observations
  ) %>%
  
  # Remove variables with only one value
  select_if(
    function(x) n_distinct(x) > 1
    ) %>%
  
  # Remove variables with more than 99% NA
  select_if(
    function(x) mean(is.na(x)) < 0.99
    ) %>%
  
  # Select: Student ID, course ID, when course taken and grade 
  select(
    `Student ID`   = `Student Number`,
    `Course ID`    = `Module (Abbrev.)`,
    Year_numerical =`Academic Year`,
    Period         = `Academic Session`,
    Grade          = `Grade symbol`
    ) %>%
  
  # Clean grade variable
  mutate(
    
    Grade = case_when(
      is.na(Grade)  ~ "0",
      Grade == "NG" ~ "0",
      TRUE          ~ str_replace(
        Grade,
        pattern     = ",",
        replacement = "."
      )
    ) %>% 
      as.numeric
    
  ) %>%

  # if student took resit (recorder in same year and period), only keep failing grade
  # group_by(
  #   `Student ID`,
  #   `Course ID`,
  #   Year,
  #   Period
  #   ) %>%
  # summarize(
  #   Grade = min(Grade)
  #   ) %>%
  # ungroup %>%
  
  # Clean Time variables
  mutate(
    
    Period = case_when(
      Period == 1   ~ "1 to 6"  ,
      Period == 2   ~ "1 to 3"  ,
      Period == 3   ~ "4 to 6"  ,
      between(Period, 100, 199) ~ "1",
      between(Period, 200, 299) ~ "2",
      between(Period, 300, 399) ~ "3",
      between(Period, 400, 499) ~ "4",
      between(Period, 500, 599) ~ "5",
      between(Period, 600, 660) ~ "6"
      ),
    
    time = paste(
      Year_numerical, substr(Period, 1, 1),
      sep = ""
      ),
    
    `Academic Year`= paste(
      Year_numerical, Year_numerical + 1,
      sep = "-")
    
    )
```

``` r
print(d_transcript)
```

    ## # A tibble: 80,182 x 7
    ##    `Student ID` `Course ID` Year_numerical Period Grade time 
    ##    <chr>        <chr>                <dbl> <chr>  <dbl> <chr>
    ##  1 6051398      COR1005               2012 1        5.7 20121
    ##  2 6051398      SSC1009               2012 1        7.8 20121
    ##  3 6051398      SKI1008               2012 1        7.9 20121
    ##  4 6051398      SCI1016               2012 2        5.8 20122
    ##  5 6051398      SKI1009               2012 2        7.5 20122
    ##  6 6051398      HUM1013               2012 2        5.9 20122
    ##  7 6051398      PRO1010               2012 3        8.2 20123
    ##  8 6051398      SKI1004               2012 4        4.6 20124
    ##  9 6051398      SKI1004               2012 4        5.6 20124
    ## 10 6051398      SCI2012               2012 4        5.5 20124
    ## # ... with 80,172 more rows, and 1 more variable: `Academic Year` <chr>

Save Data
=========

``` r
save(d_course, lists, d_AoD, d_assessment, d_ILO,
     file = "Output/data_general.RDATA")
save(d_transcript, d_course,
     file = "Output/data_pillar_1.RDATA")
save(d_text,
     file = "Output/data_pillar_2.RDATA")
```

Extra
=====

Extracting Descriptions
-----------------------

Since the overviews contain a lot of information that does not interest us, we extract the description section from the overviews. The description section contains a brief description (200-500 words) of the content of the course. To accomplish this, for each overview, we need to identify the line where the description starts and the line where it ends. The description section is always preceeded by a header saying *Description* or *Course Description* (`start_descrip`), making it relatively easy to find the starting line of the decription section with `grep`. The description section is usually followed by the literature section which starts with header saying *Literature*, *Recommended Literature* or similar (see `end_descrip`). Using `grep` together with `end_descrip`, we can again identify the ending line of the description section of almost all overviews. A handful overviews do not contain a *Literature* section. For these, the section *Instructional Format* marks the end of the description section.

``` r
# Setup
symbol_to_line <- c("\r\n", "\n") %>%
  paste(collapse = "|")
start_descrip <- c("^Description", "^Course Description") %>%
  paste(collapse = "|")
end_descrip   <- c("^Literature$", "^Recommended Literature", "^ Required Litera",
                   "^Required Litera", "^ Literature$", "Literature \\(all") %>%
  paste(collapse = "|")

# Loop
for(course in 1 : nrow(d_description)){
  
  overview <- d_description$Overview[course]
  
  overview_by_line <- strsplit(x = toString(overview), split = symbol_to_line)[[1]]
  line_start    <- grep(pattern = start_descrip, x = overview_by_line) + 1
  line_end      <- grep(pattern = end_descrip  , x = overview_by_line) - 1
  # for overviews w/o Literature section, find section Instructional Format.
  if(is_empty(line_end)){
    line_end <- grep(pattern = "^Instructional format$", x = overview_by_line) - 1
    print(paste("line_end irregular for", d_description$`Course ID`[course],
                "in catalogue", d_description$`Calendar Year`[course]))
    } # close if-statement
  
  description <- paste(overview_by_line[line_start : line_end], collapse = " ")
  
  d_description$Description[course] <- description
  
}
rm(symbol_to_line, start_descrip, end_descrip,
   course, overview, overview_by_line, line_start, line_end, description)
print(d_description)
```
