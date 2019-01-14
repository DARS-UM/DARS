Data Preparation
================
DARS
2019-01-14

-   [Import Data](#import-data)
    -   [Setup](#setup)
    -   [Course Data](#course-data)
    -   [Textual Data](#textual-data)
        -   [Course Catalogues](#course-catalogues)
        -   [Course Manuals](#course-manuals)
    -   [Student Data](#student-data)
-   [Variable Engineering](#variable-engineering)
    -   [Program Data](#program-data)
        -   [AoD](#aod)
        -   [Courses](#courses)
    -   [Textual Data](#textual-data-1)
        -   [Extracting Text](#extracting-text)
        -   [Tidy Text Format](#tidy-text-format)
        -   [Stemming](#stemming)
        -   [Removing Stopwords](#removing-stopwords)
    -   [Student Data](#student-data-1)
        -   [Selecting Variables](#selecting-variables)
    -   [Estimating concentrations:](#estimating-concentrations)
        -   [Getting UCM\_YEAR](#getting-ucm_year)
        -   [Removing master Program data and Cleaning Capstone Data](#removing-master-program-data-and-cleaning-capstone-data)
-   [Save Data](#save-data)

``` r
library(tidyverse)
library(tidytext)
library(gsheet) # import spreadsheets from google drive
library(tm)
library(hunspell) # Stemmer
```

Import Data
===========

The datasets we use are saved as spreadsheet on our google drive *DARS* (with exeption of grade data saved as csv files on the computer for privacy reasons). We use the function `gsheet2tbl` to import them to `R Studio` as tibbles. We use the tibble data format (an evolution of the data frame format) because this is the format of reference of the `tidyverse` on whose tools our analysis is heavily based.

Setup
-----

First, we import the spreadsheet with information pertraining the Aims of the Degree (AoD) and Assessments from the drive and save it under `lists_brut`. `lists_brut` contains 4 columns, under which we find the `19` types of assessment, the `18` aims of the degree (AoD) of the degree, and two columns containing binary vectors indicating which assessment types and AoD we will consider when ploting the data[1]. Then we create a list with this same columns, but instead of having binary vectors for the plots, we keep vectors of only the names of relevant assessments and AoDs for the plots (`Assessment_plot`adnd `AoD_plot` respectively). We also any imported emtpy cells.

``` r
lists_brut <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1239912347')
lists <- list(
  Assessment      = lists_brut$Assessment,
  Assessment_plot = lists_brut$Assessment[lists_brut$`Assessment for Plot` == 1],
  AoD             = lists_brut$Aim[!is.na(lists_brut$Aim)],
  AoD_plot        = lists_brut$Aim[lists_brut$`Aim for Plot`[!is.na(lists_brut$Aim)] == 1]
  )
rm(lists_brut)
```

Course Data
-----------

We import three spreadsheets from the drive and transform them into the so-called *tidy format*. The tibble `d_course` contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered. The tibble `d_assessment` indicates which type(s) of assessment each course contains with one row per course-assessment; and the tibble `d_ILO` indicates which AoD(s) the intended learning objectives (ILOs) of the courses cover with one row per course-ILO-AoD.

``` r
d_course <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1655700848')
print(d_course)
```

    ## # A tibble: 280 x 11
    ##    Code  `Course Title` Period `Period (additi~ `Period (additi~
    ##    <chr> <chr>          <chr>  <chr>            <chr>           
    ##  1 AAM2~ Academic Advi~ Perio~ <NA>             <NA>            
    ##  2 AAM2~ Academic Advi~ Perio~ <NA>             <NA>            
    ##  3 AAM2~ Academic Advi~ Perio~ <NA>             <NA>            
    ##  4 AAM2~ Academic Advi~ Perio~ <NA>             <NA>            
    ##  5 AAM2~ Academic Advi~ Perio~ <NA>             <NA>            
    ##  6 AAM2~ Academic Advi~ Perio~ <NA>             <NA>            
    ##  7 AAM2~ Academic Advi~ Perio~ <NA>             <NA>            
    ##  8 CAP3~ Capstone       Perio~ Period 4         <NA>            
    ##  9 COR1~ Philosophy of~ Perio~ Period 5         <NA>            
    ## 10 COR1~ Contemporary ~ Perio~ Period 4         <NA>            
    ## # ... with 270 more rows, and 6 more variables: Concentration <chr>,
    ## #   `Concentration (additional)` <chr>, Cluster <chr>, `Missing from ILO
    ## #   File` <int>, `Most Recent Catalogue` <chr>, Type <chr>

``` r
d_assessment <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1102665750') %>%
  select(- `Comment on Assessment`) %>%
  gather(Assessment, assessment_covered, - Code, factor_key = T) %>%
  mutate(assessment_covered = as.logical(assessment_covered)) %>%
  filter(assessment_covered) %>%
  select(- assessment_covered) %>%
  arrange(Code)
print(d_assessment)
```

    ## # A tibble: 482 x 2
    ##    Code    Assessment    
    ##    <chr>   <fct>         
    ##  1 AAM2001 Oral          
    ##  2 AAM2002 Oral          
    ##  3 AAM2003 Oral          
    ##  4 AAM2004 Oral          
    ##  5 AAM2005 Oral          
    ##  6 AAM2006 Oral          
    ##  7 AAM2007 Oral          
    ##  8 CAP3000 Paper         
    ##  9 CAP3000 Presentation  
    ## 10 CAP3000 Research Prop.
    ## # ... with 472 more rows

``` r
d_ILO <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1896457050') %>%
  select(- c(Comments, Comments_for_Jeroen)) %>%
  gather(AoD, AoD_covered, -c(Code, Objectives), factor_key = T) %>%
  mutate(AoD_covered = as.logical(AoD_covered)) %>%
  rename(ILO = Objectives) %>%
  filter(AoD_covered) %>%
  select(- AoD_covered) %>%
  arrange(Code)
print(d_ILO)
```

    ## # A tibble: 1,232 x 3
    ##    Code    ILO         AoD                   
    ##    <chr>   <chr>       <fct>                 
    ##  1 AAM2001 Not Defined Academic Expertise    
    ##  2 AAM2001 Not Defined Reflective Skills     
    ##  3 AAM2001 Not Defined Decision-making Skills
    ##  4 AAM2002 Not Defined Academic Expertise    
    ##  5 AAM2002 Not Defined Reflective Skills     
    ##  6 AAM2002 Not Defined Decision-making Skills
    ##  7 AAM2003 Not Defined Academic Expertise    
    ##  8 AAM2003 Not Defined Reflective Skills     
    ##  9 AAM2003 Not Defined Decision-making Skills
    ## 10 AAM2004 Not Defined Academic Expertise    
    ## # ... with 1,222 more rows

Textual Data
------------

### Course Catalogues

In order to conduct a preliminary topic modeling of course content, we first analyze their description in the course catalogues. The corpus `corpus_catalogues` contains the pdfs of the `5` most recent course catalogues.

``` r
corpus_catalogues <- Corpus(x             = DirSource("Catalogues"),
                            readerControl = list(reader = readPDF(control = list(text = "-layout"))))
```

### Course Manuals

To expand our topic modeling of course content, we analyse the material in the course manuals. The `corpus_manuals` contains the pdfs of the course manuals for the year 2017-2018 (most recently, available).

``` r
corpus_manuals <- Corpus(x             = DirSource("Manuals 2017-18"),
                         readerControl = list(reader = readPDF(control = list(text = "-layout"))))
```

Student Data
------------

The tibble `d_transcript` contains the transcript information of students as was provided. It has 40 columns, and rows correspond to a type of grade (e.g. final grade, attendance) per student per course per time they took it.

``` r
d_transcript <- rbind(
  read_csv(
    "Raw Grades/grades1.csv",
    col_types = cols(
      `Program (Abbreviation)` = col_character(),
      `Appraisal Status` = col_character(),
      `Module Booking Reason (Description)` = col_character(),
      `Object name` = col_character(),
      `Start date` = col_character(),
      `End Date` = col_character()
      )
    ),
  read_csv(
    "Raw Grades/grades2.csv",
    col_types = cols(
      `Program (Abbreviation)` = col_character(),
      `Appraisal Status` = col_character(), 
      `Module Booking Reason (Description)` = col_character(), 
      `Object name` = col_character(), 
      `Start date` = col_character(), 
      `End Date` = col_character()
      )
    )
  )
```

Variable Engineering
====================

Program Data
------------

The analysis performed on the course data is aimed at discoving what the contribution of each course is towards the fullfilment of the AoDs, and comparing different types of curricula, or programs, based on this infromation.

### AoD

In this analysis, we conside that a course can cover an AoD in two ways: a course covers an AoD if one of its ILOs covers it, or if one of its assessments cover it. For instance, if one of ILO of a course states that the students will learn to analyze empirical data in the context of academic research, then the course in question covers the AoD `Research Skills`; and if one of the assessment is a group presentation, then the course also covers the AoD `Collaborative Skills` and `Communication Skills`.

#### AoD from ILOs

(For this section we use: `d_ILO`-indicates which AoD(s) the intended learning objectives (ILOs) of the courses cover with one row per course-ILO-AoD)

In order to determine which AoD each course covers with its ILOs, we first eliminate the AoD that are not covered by the ILOs, and then we keep only one instance of each combination of course and AoD in case a course had several ILOs covering the same AoD.

``` r
d_AoD_ILO <- d_ILO %>%
  distinct(Code, AoD)
print(d_AoD_ILO)
```

    ## # A tibble: 681 x 2
    ##    Code    AoD                   
    ##    <chr>   <fct>                 
    ##  1 AAM2001 Academic Expertise    
    ##  2 AAM2001 Reflective Skills     
    ##  3 AAM2001 Decision-making Skills
    ##  4 AAM2002 Academic Expertise    
    ##  5 AAM2002 Reflective Skills     
    ##  6 AAM2002 Decision-making Skills
    ##  7 AAM2003 Academic Expertise    
    ##  8 AAM2003 Reflective Skills     
    ##  9 AAM2003 Decision-making Skills
    ## 10 AAM2004 Academic Expertise    
    ## # ... with 671 more rows

#### AoD from Assessment

(For this section we use: `d_assessment`-indicates which type(s) of assessment each course contains with one row per course-assessment)

In order to determine which AoD each course covers with its assessments, we need to create a binary matrix which indicates which AoD(s) each assessment type covers. We have created such matrix on our google drive and we save it in the following piece of code as `map_assessment_AoD`. `map_assessment_AoD` indicates that, for instance, the assessment type `Essay` covers the AoD `Critical Thinking Skills`, `Communication Skills` and `Writing Skill`. Thus, the first step is to import this matrix:

``` r
map_assessment_AoD <- as.matrix(gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=719531216')[,lists$AoD] %>%
                                  mutate_all(as.logical))
rownames(map_assessment_AoD) <- lists$Assessment
print(map_assessment_AoD[1:5, 8:12])
```

    ##              Problem Solving Skills Critical Thinking Skills
    ## Paper                          TRUE                     TRUE
    ## Essay                         FALSE                     TRUE
    ## Presentation                  FALSE                     TRUE
    ## Written Exam                  FALSE                    FALSE
    ## Take Home                     FALSE                     TRUE
    ##              Communication Skills Writing Skills Learning Skills
    ## Paper                        TRUE           TRUE           FALSE
    ## Essay                        TRUE           TRUE           FALSE
    ## Presentation                 TRUE          FALSE           FALSE
    ## Written Exam                FALSE          FALSE           FALSE
    ## Take Home                    TRUE           TRUE           FALSE

Now that we have the matrix `map_assessment_AoD`, we want to find out which AoDs are covered by a course through its assessments. To do this we create an empty tibble `d_AoD_assessment` to store which assessment is covered by each course, and which AoD said assessment covers.Then, we fill in the information with a loop. In the loop, we first extract a row of `d_assessment` and save it as `observation`. Then, we determine the corresponding assessment which we save as `assessment`. Then, we use the matrix `map_assessment_AoD` to determine which AoD `assessment` covers and use `cbind` and `rbind` to add the information to the tibble `d_AoD_assessment`.

``` r
d_AoD_assessment <- tibble(Code        = character(0),
                           Assessment  = character(0),
                           AoD         = character(0))
for(i in 1 : nrow(d_assessment)){
  
  observation <- d_assessment[i, ]
  
  assessments <- observation$Assessment
  AoD_covered <- map_assessment_AoD[assessments, ]
  AoD         <- lists$AoD[AoD_covered]
  
  if(length(AoD) >= 1) d_AoD_assessment <- rbind(d_AoD_assessment,
                                                 as.tibble(cbind(observation,
                                                                 AoD)))
}
rm(map_assessment_AoD, observation, AoD, AoD_covered, assessments, i)
```

Next, we keep ony one copy of each distinct combination of course and AoD in case a course had several assessments covering the same AoD (this is analogous to what we did with the tibble `d_AoD_ILO`).

``` r
d_AoD_assessment <- d_AoD_assessment %>%
  distinct(Code, AoD)
print(d_AoD_assessment)
```

    ## # A tibble: 732 x 2
    ##    Code    AoD                     
    ##    <chr>   <fct>                   
    ##  1 AAM2001 Communication Skills    
    ##  2 AAM2002 Communication Skills    
    ##  3 AAM2003 Communication Skills    
    ##  4 AAM2004 Communication Skills    
    ##  5 AAM2005 Communication Skills    
    ##  6 AAM2006 Communication Skills    
    ##  7 AAM2007 Communication Skills    
    ##  8 CAP3000 Problem Solving Skills  
    ##  9 CAP3000 Critical Thinking Skills
    ## 10 CAP3000 Communication Skills    
    ## # ... with 722 more rows

#### Combining AoD from ILOs and from assessments

Finally, we can use a `rbind` to combine the two tibbles `d_AoD_ILO` and `d_AoD_assessment`. We also use `distinct` in case a course covers an AoD with both its ILOS and its assessments.

``` r
d_AoD <- rbind(d_AoD_ILO,
               d_AoD_assessment) %>%
  distinct %>%
  arrange(Code)
rm(d_AoD_ILO, d_AoD_assessment)
print(d_AoD)
```

    ## # A tibble: 1,231 x 2
    ##    Code    AoD                   
    ##    <chr>   <fct>                 
    ##  1 AAM2001 Academic Expertise    
    ##  2 AAM2001 Reflective Skills     
    ##  3 AAM2001 Decision-making Skills
    ##  4 AAM2001 Communication Skills  
    ##  5 AAM2002 Academic Expertise    
    ##  6 AAM2002 Reflective Skills     
    ##  7 AAM2002 Decision-making Skills
    ##  8 AAM2002 Communication Skills  
    ##  9 AAM2003 Academic Expertise    
    ## 10 AAM2003 Reflective Skills     
    ## # ... with 1,221 more rows

### Courses

(In this section we use: `d_course`- contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered.)

Now that we have a clear overview of the distribution of AoDs (`d_AoD`) and assessments (`d_assessment`) among the courses, let us add variables to the tibble `d_course` that contain the information at the course level. For this we create the following three tibbles: 1) `d_ILO_detail` contains two columns indicating the code of the course and the number of ILOs it contains. 2) `d_assessment_detail` contains three columns indicating the code of the course, the number of assessments it covers and a list of the assessments it covers. 3) `d_AoD_detail` contains three columns indicating the code of the course, the number of AoD it covers and a list of the AoD it covers. Then we use a `full_join` to add these variables to the tibble `d_course`.

``` r
d_ILO_detail <- d_ILO %>% 
  distinct(Code, ILO) %>%
  count(Code) %>%
  rename(n_ILO = n)

d_assessment_detail <- d_assessment %>%
  group_by(Code) %>%
  summarise(n_assessment = n(),
            `Assessments Covered` = paste(Assessment, collapse = ", "))

d_AoD_detail <- d_AoD %>%
  group_by(Code) %>%
  summarise(n_AoD = n(),
            `AoD Covered` = paste(AoD, collapse = ", "))

d_course_detail <- d_ILO_detail %>%
  full_join(d_assessment_detail, by = "Code") %>%
  full_join(d_AoD_detail       , by = "Code")

rm(d_ILO_detail, d_assessment_detail, d_AoD_detail)
print(d_course_detail)
```

    ## # A tibble: 280 x 6
    ##    Code   n_ILO n_assessment `Assessments Cove~ n_AoD `AoD Covered`       
    ##    <chr>  <int>        <int> <chr>              <int> <chr>               
    ##  1 AAM20~     1            1 Oral                   4 Academic Expertise,~
    ##  2 AAM20~     1            1 Oral                   4 Academic Expertise,~
    ##  3 AAM20~     1            1 Oral                   4 Academic Expertise,~
    ##  4 AAM20~     1            1 Oral                   4 Academic Expertise,~
    ##  5 AAM20~     1            1 Oral                   4 Academic Expertise,~
    ##  6 AAM20~     1            1 Oral                   4 Academic Expertise,~
    ##  7 AAM20~     1            1 Oral                   4 Academic Expertise,~
    ##  8 CAP30~     3            3 Paper, Presentati~    10 Advanced Knowledge,~
    ##  9 COR10~     3            2 Essay, Written Ex~     7 Core, Elementary Kn~
    ## 10 COR10~     3            3 Paper, Written Ex~     7 Core, Elementary Kn~
    ## # ... with 270 more rows

``` r
d_course <- d_course %>%
  full_join(d_course_detail, by = "Code") %>%
  select(Code, `Course Title`, Cluster,
         n_ILO,
         n_assessment, `Assessments Covered`,
         n_AoD, `AoD Covered`, 
         everything())

rm(d_course_detail)
print(d_course)
```

    ## # A tibble: 280 x 16
    ##    Code  `Course Title` Cluster n_ILO n_assessment `Assessments Co~ n_AoD
    ##    <chr> <chr>          <chr>   <int>        <int> <chr>            <int>
    ##  1 AAM2~ Academic Advi~ <NA>        1            1 Oral                 4
    ##  2 AAM2~ Academic Advi~ <NA>        1            1 Oral                 4
    ##  3 AAM2~ Academic Advi~ <NA>        1            1 Oral                 4
    ##  4 AAM2~ Academic Advi~ <NA>        1            1 Oral                 4
    ##  5 AAM2~ Academic Advi~ <NA>        1            1 Oral                 4
    ##  6 AAM2~ Academic Advi~ <NA>        1            1 Oral                 4
    ##  7 AAM2~ Academic Advi~ <NA>        1            1 Oral                 4
    ##  8 CAP3~ Capstone       <NA>        3            3 Paper, Presenta~    10
    ##  9 COR1~ Philosophy of~ Philos~     3            2 Essay, Written ~     7
    ## 10 COR1~ Contemporary ~ History     3            3 Paper, Written ~     7
    ## # ... with 270 more rows, and 9 more variables: `AoD Covered` <chr>,
    ## #   Period <chr>, `Period (additional)` <chr>, `Period (additional
    ## #   bis)` <chr>, Concentration <chr>, `Concentration (additional)` <chr>,
    ## #   `Missing from ILO File` <int>, `Most Recent Catalogue` <chr>,
    ## #   Type <chr>

The undegraduate research courses (*UGR-*) are only present at the `3000` level (advanced level). Yet, these course are also offered at the `2000` level (intermediate level). We use an `rbind` to duplicate the rows of the course `UGR3000` and mutate their `Code` to `UGR2000`.

``` r
d_course <- d_course %>%
  rbind(filter(., Code %in% c("UGR3001", "UGR3002", "UGR3003", "UGR3005")) %>%
          mutate(Code = case_when(Code == "UGR3001" ~ "UGR2001",
                                  Code == "UGR3002" ~ "UGR2002",
                                  Code == "UGR3003" ~ "UGR2003",
                                  Code == "UGR3005" ~ "UGR2005")))
print(filter(d_course, Code %in% c("UGR2001", "UGR2002", "UGR2003", "UGR2005",
                                   "UGR3001", "UGR3002", "UGR3003", "UGR3005")))
```

    ## # A tibble: 8 x 16
    ##   Code  `Course Title` Cluster n_ILO n_assessment `Assessments Co~ n_AoD
    ##   <chr> <chr>          <chr>   <int>        <int> <chr>            <int>
    ## 1 UGR3~ Undergraduate~ Methods     5            2 Paper, Presenta~    10
    ## 2 UGR3~ Undergraduate~ Skills     NA            6 Paper, Essay, P~     6
    ## 3 UGR3~ Applied Resea~ Skills     NA            5 Paper, Presenta~     5
    ## 4 UGR3~ Artistic Rese~ Methods    NA            2 Paper, Research~     5
    ## 5 UGR2~ Undergraduate~ Methods     5            2 Paper, Presenta~    10
    ## 6 UGR2~ Undergraduate~ Skills     NA            6 Paper, Essay, P~     6
    ## 7 UGR2~ Applied Resea~ Skills     NA            5 Paper, Presenta~     5
    ## 8 UGR2~ Artistic Rese~ Methods    NA            2 Paper, Research~     5
    ## # ... with 9 more variables: `AoD Covered` <chr>, Period <chr>, `Period
    ## #   (additional)` <chr>, `Period (additional bis)` <chr>,
    ## #   Concentration <chr>, `Concentration (additional)` <chr>, `Missing from
    ## #   ILO File` <int>, `Most Recent Catalogue` <chr>, Type <chr>

Finally, we add a series of informative variable at the course level.

``` r
d_course <- d_course %>%
  mutate(
    Letters = substring(Code, 1, 3),
    Number  = as.numeric(substring(Code, 4, 7)),
    Level   = case_when(between(Number, 1000, 1999) ~ "Introductory",
                        between(Number, 2000, 2999) ~ "Intermediate",
                        between(Number, 3000, 3999) ~ "Advanced"    ))

print(select(d_course, Code, Letters, Number, Level))
```

    ## # A tibble: 284 x 4
    ##    Code    Letters Number Level       
    ##    <chr>   <chr>    <dbl> <chr>       
    ##  1 AAM2001 AAM       2001 Intermediate
    ##  2 AAM2002 AAM       2002 Intermediate
    ##  3 AAM2003 AAM       2003 Intermediate
    ##  4 AAM2004 AAM       2004 Intermediate
    ##  5 AAM2005 AAM       2005 Intermediate
    ##  6 AAM2006 AAM       2006 Intermediate
    ##  7 AAM2007 AAM       2007 Intermediate
    ##  8 CAP3000 CAP       3000 Advanced    
    ##  9 COR1002 COR       1002 Introductory
    ## 10 COR1003 COR       1003 Introductory
    ## # ... with 274 more rows

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
# Setup
n_catalogue <- length(corpus_catalogues)
calendar_years <- c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019")
d_description <- tibble(Code            = character(0),
                        `Calendar Year` = character(0),
                        Overview        = character(0))

course_code <- c("COR", "HUM", "SCI", "SSC", "SKI", "PRO", "UGR", "CAP") %>%
  paste(collapse = "|")

content_to_exclude <- c(
  "^Core Courses \\(COR\\)",
  "^Humanities \\(HUM\\)",
  "^Sciences \\(SCI\\)",
  "^Social Sciences \\(SSC\\)",
  "^Skills Trainings \\(SKI\\)", "^Skills Training \\(SKI\\)",
  "^Project \\(PRO\\)", "^Projects \\(PRO\\)",
  "^Undergraduate Research \\(UGR\\)",
  " UCM Undergraduate\r\nResearch", "UCM Undergraduate\r\n  Research"
  ) %>%
  paste(collapse = "|")

# Loop 1
for(n in 1 : n_catalogue){
  
  cat <- content(corpus_catalogues[[n]])
  
  # Overview section in catalogue
  page_overview_start <- grep(pattern = "^Core Courses \\(COR\\)", x = cat) + 1
  page_overview_end   <- grep(pattern = "Appendix", x = cat) - 1
    if(length(page_overview_end) == 0) page_overview_end <- length(cat)
  cat_overview <- cat[page_overview_start : page_overview_end]
  
  # Course description only
  pages_to_exclude <- grep(pattern = content_to_exclude, x = cat_overview)
  cat_overview <- cat_overview[! 1 : length(cat_overview) %in% pages_to_exclude]
  
  # First pages of course overviews
  first_three_letters <- substr(cat_overview, start = 1, stop = 3)
  first_pages_overview <- grep(pattern = course_code, x = first_three_letters)
  
  # Loop 2
  for(page in first_pages_overview){
    
    # Extract description
    overview <- if(page + 1 %in% first_pages_overview) cat_overview[page]
                else paste(cat_overview[page : (page + 1)], collapese = " ")
    
    # Save overview
    Code <- substr(overview, start = 1, stop = 7)
    year <- calendar_years[n]
    d_description <- d_description %>%
      add_row(
        Code = Code,
        `Calendar Year` = year,
        Overview = overview
      )
    
  } # close for-loop (page)
  
} # close for-loop (n)

rm(corpus_catalogues,
   n_catalogue, calendar_years, course_code, content_to_exclude,
   n, cat,
   page_overview_start, page_overview_end,
   pages_to_exclude, cat_overview,
   first_three_letters, first_pages_overview,
   page, overview, Code, year)
print(d_description)
```

    ## # A tibble: 831 x 3
    ##    Code    `Calendar Year` Overview                                       
    ##    <chr>   <chr>           <chr>                                          
    ##  1 COR1002 2014-2015       "COR1002 - Philosophy of Science\r\nCourse coo~
    ##  2 COR1003 2014-2015       "COR1003 - Contemporary World History\r\nCours~
    ##  3 COR1004 2014-2015       "COR1004 - Political Philosophy\r\nCourse coor~
    ##  4 COR1005 2014-2015       "COR1005 - Modeling Nature\r\nCourse coordinat~
    ##  5 HUM1003 2014-2015       "HUM1003 - Cultural Studies I: Doing Cultural ~
    ##  6 HUM1007 2014-2015       "HUM1007 - Introduction to Philosophy\r\nCours~
    ##  7 HUM1010 2014-2015       "HUM1010 - Common Foundations of Law in Europe~
    ##  8 HUM1011 2014-2015       "HUM1011 - Introduction to Art; Representation~
    ##  9 HUM1012 2014-2015       "HUM1012 - Pop Songs and Poetry: Theory and An~
    ## 10 HUM1013 2014-2015       "HUM1013 - The Idea of Europe: The Intellectua~
    ## # ... with 821 more rows

##### Extracting Descriptions

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
d_description <- d_description %>%
  mutate(Description = NA)
# Loop
for(course in 1 : nrow(d_description)){
  
  overview <- d_description$Overview[course]
  
  overview_by_line <- strsplit(x = toString(overview), split = symbol_to_line)[[1]]
  line_start    <- grep(pattern = start_descrip, x = overview_by_line) + 1
  line_end      <- grep(pattern = end_descrip  , x = overview_by_line) - 1
  # for overviews w/o Literature section, find section Instructional Format.
  if(is_empty(line_end)){
    line_end <- grep(pattern = "^Instructional format$", x = overview_by_line) - 1
    print(paste("line_end irregular for", d_description$Code[course],
                "in catalogue", d_description$`Calendar Year`[course]))
    } # close if-statement
  
  description <- paste(overview_by_line[line_start : line_end], collapse = " ")
  
  d_description$Description[course] <- description
  
}
```

    ## [1] "line_end irregular for UGR3003 in catalogue 2016-2017"
    ## [1] "line_end irregular for PRO2012 in catalogue 2017-2018"
    ## [1] "line_end irregular for UGR3003 in catalogue 2017-2018"
    ## [1] "line_end irregular for PRO2012 in catalogue 2018-2019"
    ## [1] "line_end irregular for UGR3003 in catalogue 2018-2019"
    ## [1] "line_end irregular for UGR3005 in catalogue 2018-2019"

``` r
rm(symbol_to_line, start_descrip, end_descrip,
   course, overview, overview_by_line, line_start, line_end, description)
print(d_description)
```

    ## # A tibble: 831 x 4
    ##    Code   `Calendar Year` Overview                Description             
    ##    <chr>  <chr>           <chr>                   <chr>                   
    ##  1 COR10~ 2014-2015       "COR1002 - Philosophy ~ Starting from classical~
    ##  2 COR10~ 2014-2015       "COR1003 - Contemporar~ The course intends to t~
    ##  3 COR10~ 2014-2015       "COR1004 - Political P~ Politics is a complex a~
    ##  4 COR10~ 2014-2015       "COR1005 - Modeling Na~ The aim of the course i~
    ##  5 HUM10~ 2014-2015       "HUM1003 - Cultural St~ Cultural Studies is a w~
    ##  6 HUM10~ 2014-2015       "HUM1007 - Introductio~ One of the greatest and~
    ##  7 HUM10~ 2014-2015       "HUM1010 - Common Foun~ What do Europeans have ~
    ##  8 HUM10~ 2014-2015       "HUM1011 - Introductio~ The traditional term fo~
    ##  9 HUM10~ 2014-2015       "HUM1012 - Pop Songs a~ This course is based on~
    ## 10 HUM10~ 2014-2015       "HUM1013 - The Idea of~ This course deals with ~
    ## # ... with 821 more rows

#### Extracting Text: Course Manuals

(For this section we use:`corpus_manuals`-contains the pdfs of the course manuals for the year 2017-2018 (most recently, available))

Since we want to use tools from dplyr and tidytext we must first transform our corpus into a dataframe.

``` r
d_manual <- tidy(corpus_manuals) %>%
  select(id, text) %>%
  mutate(Code = substring(id, 1, 7)) %>%
  select(-id) %>%
  rename(Manual = text)
rm(corpus_manuals)
print(d_manual)
```

    ## # A tibble: 153 x 2
    ##    Manual                                                           Code  
    ##    <chr>                                                            <chr> 
    ##  1 "P h i l o s o p hy o f S c i e n c e\r\n                      ~ COR10~
    ##  2 "General information\r\nIntroduction\r\nNew things are happenin~ COR10~
    ##  3 "University College Maastricht. 2017-18, Period 2\r\n          ~ COR10~
    ##  4 "Theory Construction\r\nand\r\nModelling Techniques\r\nCode: CO~ COR10~
    ##  5 "  Cultural Studies I\r\nDoing Cultural Studies\r\n University ~ HUM10~
    ##  6 "AN INTRODUCTORY COURSE TO PHILOSOPHY\n                        ~ HUM10~
    ##  7 "                 HU1010\r\nCOMMON FOUNDATIONS OF LAW IN EUROPE~ HUM10~
    ##  8 "Introduction to Art\r\nRepresentations, Performances and Inter~ HUM10~
    ##  9 "Pop Songs and Poetry: Theory and Analysis\r\n            Code:~ HUM10~
    ## 10 "\n              The Idea of Europe:\r\nThe Intellectual Histor~ HUM10~
    ## # ... with 143 more rows

Separately, we want to add the manual textual information to`d_course`which contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered.

``` r
d_course <- full_join(d_course, d_manual, by = "Code")
```

### Tidy Text Format

To put everything into tidy text format, we first create three tibbles `d_overview_tidy`, `d_description_tidy`, `d_manual` that respectively store the overviews, descriptions and text from manuals in the tidy text format, with one row per course-year-word (and course-word for `d_manual`).

``` r
d_overview <- d_description %>%
  select(Code, `Calendar Year`, Overview) %>%
  unnest_tokens(output = word, input = Overview)
d_description <- d_description %>%
  select(Code, `Calendar Year`, Description) %>%
  unnest_tokens(output = word, input = Description)
d_manual <- unnest_tokens(d_manual, output = word, input = Manual)
```

### Stemming

We then use the function `hunspell__stem`, which returns valid stems for a given word, to stem the words in the tibbles `d_overview_tidy` and `d_description_tidy`.

We first create a function `stem_hunspell` which, given a term, returns its most basic stem (the last one from the list of stem returned by `hunspell__stem`). We would like to apply `stem_hunspell` on the words of `d_overview_tidy` and `d_manual`, since they have similar structures, we row bind them into `dictionary` to ease the application of `stem_hunspell`. However, since, `stem_hunspell` is not a vectorized function and the number of words in `dictionary` is large, we first use distinct on the `word` variable to find a list containing *once* each term present in the course overviews and manuals. Then, we use `mutate` to apply the function `stem_hunspell` on each word from our dictionary and save its results as a new column `word_stem`.

``` r
stem_hunspell <- function(term) {
  stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
  n_stems <- length(stems)            # number of stems
  if (n_stems == 0) term              # if no stems, return original term
  else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem
}
dictionary <- select(d_overview, Code, word) %>%
  rbind(d_manual) %>% 
  select(word) %>%
  distinct %>%
  mutate(word_stem = purrr::map_chr(.x = word,
                                    .f = stem_hunspell))
# terms for which the stem differs from the original word
filter(dictionary, word != word_stem)
```

    ## # A tibble: 11,125 x 2
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
    ## # ... with 11,115 more rows

Finally, we create a function `stem_with_dictionary` that performs a left join on the dataframe it takes as input with `dictionary`, thus adding the word stems to the original dataframe. Then, we use `stem_with_dictionary` to include the stems of all words in `d_description`, `d_overview`, and `d_manual`.

``` r
stem_with_dictionary <- function(data) data %>%
                                         left_join(dictionary, by = "word") %>%
                                         rename(word_original = word,
                                                word = word_stem)
  
d_description <- stem_with_dictionary(d_description)
d_overview <- stem_with_dictionary(d_overview)
d_manual <- stem_with_dictionary(d_manual)
rm(stems, stem_hunspell, stem_with_dictionary)
```

    ## Warning in rm(stems, stem_hunspell, stem_with_dictionary): object 'stems'
    ## not found

``` r
print(d_overview) # See humanities - humanity
```

    ## # A tibble: 340,609 x 4
    ##    Code    `Calendar Year` word_original word       
    ##    <chr>   <chr>           <chr>         <chr>      
    ##  1 COR1002 2014-2015       cor1002       cor1002    
    ##  2 COR1002 2014-2015       philosophy    philosophy 
    ##  3 COR1002 2014-2015       of            of         
    ##  4 COR1002 2014-2015       science       science    
    ##  5 COR1002 2014-2015       course        course     
    ##  6 COR1002 2014-2015       coordinator   coordinator
    ##  7 COR1002 2014-2015       prof          prof       
    ##  8 COR1002 2014-2015       dr            dr         
    ##  9 COR1002 2014-2015       l             l          
    ## 10 COR1002 2014-2015       boon          boon       
    ## # ... with 340,599 more rows

### Removing Stopwords

Finally, we want to filter out some uninformative words from our textual data. For this, we first store all the unwanted words in a vector `sw_owm`. We also create function `remove_sw` which receives a dataframe as input and filters out all words in `sw_own`. Then we apply this function to `d_description`, `d_overview`, and `d_manual`.

``` r
# Own stop words
sw_own <- c("______________________________________________________________________________")
remove_sw <- function(data) data %>%
  filter(! word %in% c(stop_words$word, sw_own))
d_description <- remove_sw(d_description)
d_overview <- remove_sw(d_overview)
d_manual <- remove_sw(d_manual)
rm(sw_own, remove_sw)
```

Student Data
------------

(In this section we use: `d_transcript`- contains the transcript information of students as was provided. It has 40 columns, and rows correspond roughly to a type of grade (e.g. final grade, attendance) per student per course per time they took it).

### Selecting Variables

Our dataframe contains many variables and rows that are either empty or meaningless for our analysis. First, we filter, the final grades of studets to keep only relevant rows (Final Confirmed Grades are those with `Appraisal (Description)` as "Grade supervisor"). Then we select ony the 10 variables that we will use for the anlysis, and give them more comprehensible names.

``` r
d_transcript <- d_transcript %>%
  filter(`Appraisal (Description)` == "Grade supervisor") %>%
  select(`Student Number`, `Module (Abbrev.)`, `Module (Desc.)`,
         `Program (Description)`, `Academic Year`, `Academic Session`,
         `Number rep. attemp`, `Grade symbol`, `Booking Status (Desc.)`) %>%
  rename(`Student ID Number` = `Student Number`,
         `Course Code` = `Module (Abbrev.)`,
         `Course Title` = `Module (Desc.)`,
         `Study Program` = `Program (Description)`,
         `Civil Year` = `Academic Year`, # double check
         `Period` = `Academic Session`,
         `Number Repeated Attempt` = `Number rep. attemp`,
         Grade = `Grade symbol`,
         `Status` = `Booking Status (Desc.)`) %>% # is there a better name for the variable?
  distinct
```

Each faculty had a different code for their calendars, we want to standardise them so we can compare. We convert everything into UCM calendar codes (the correspondance between calendars was given to us by R. Vos) and save it under a variable `Period`:

``` r
d_transcript <- d_transcript %>%
  mutate(Period = case_when(Period == 1 ~ "1 to 6"  ,
                            Period == 2 ~ "1 to 3"  ,
                            Period == 3 ~ "4 to 6"  ,
                            Period == 680 ~ "5"     , # exception
                            Period == 350 ~ "3 to 5", # exception
                            Period == 403 ~ "4 to 5", # exception
                            Period == 900 ~ "1 to 6", # exception
                            between(Period, 100, 199) ~ "1",
                            between(Period, 200, 299) ~ "2",
                            between(Period, 300, 399) ~ "3",
                            between(Period, 400, 499) ~ "4",
                            between(Period, 500, 599) ~ "5",
                            between(Period, 600, 660) ~ "6",
                            between(Period, 901, 999) ~ "1"))
```

Estimating concentrations:
--------------------------

For our analysis we would also like to know what the concentration of each student is. However, this is not given, so we will have to estimate. We know that the maximimum amount of courses a student can take outside of their concentration is 2. Therefore, if any student has more than two courses in one concetration, this should count towards the concentration. We expect students who addhere strictly to single concentrations, and then students who have a mixed concentration. However, it is possible that a person has not yet taken sufficient courses to make a call on their concentration, we will mark these people as "Undecided". Furthermore, it is possible that a student has taken too many courses out of all the concentrations, we will label these "Confused" and for all other cases we need to check the specifics, therefore we will label them "oops"

NOTE TO RAPHAEL: we might want o have your course concentrations because they take double concentrations into account!

``` r
d_concentration <- d_course %>%
  select(Code, Concentration, `Concentration (additional)`) %>%
  gather(key = X, value = Concentration, Concentration, `Concentration (additional)`, na.rm = TRUE) %>%
  left_join(d_transcript, by = c("Code" = "Course Code")) %>%
  select(Concentration, `Student ID Number`) %>%
  count(Concentration, `Student ID Number`) %>%
  spread(key = Concentration, value = n, fill = 0) %>%
  mutate(n_course = HUM + SCI + SSC,
         `Student Concentration` = case_when(
           n_course < 20 ~ "not enough courses",
           HUM > 5 & SSC > 5 & SCI > 5 ~ "confused",
           HUM > 5 & SSC > 5           ~ "HUM/SCI",
           HUM > 5                     ~ "HUM",
                     SSC > 5 & SCI > 5 ~ "SSC/SCI",
                     SSC > 5           ~ "SSC",
                               SCI > 5 ~ "SCI"
  ))
 
  transmute(student_Concentration = case_when(
    (HUM  >  2    &   SSC >  2   &  SCI <= 2) ~ "HUM/SSC",
    (HUM  >  2    &   SSC <= 2   &  SCI >  2) ~ "HUM/SCI",
    (HUM  >  2    &   SSC <= 2   &  SCI <= 2) ~ "HUM",
    (HUM  <= 2    &   SSC >  2   &  SCI >  2)  ~ "SSC/SCI",
    (HUM  <= 2    &   SSC >  2   &  SCI <= 2) ~ "SSC",
    (HUM  <= 2    &   SSC <= 2   &  SCI >  2)  ~ "SCI",
    (HUM  <  2    &   SSC <  2   &  SCI <  2) ~ "UNDECIDED",
    (HUM  >  2    &   SSC >  2   &  SCI >  2) ~ "CONFUSED", #normal cases ^
    (HUM > SCI    &   HUM > SSC)              ~ "HUM",
    (SSC > SCI    &   SSC > HUM)              ~ "SSC",
    (SCI > HUM    &   SCI > SSC)              ~ "HUM",
    TRUE ~ "oops"
  ))
```

Since we will need to check each student with an "oops" concentration to see what happened, lets separate this data for later:

``` r
checkup <- filter(d_student_concentration, student_Concentration == "oops") 
```

### Getting UCM\_YEAR

In order to compare the trajectories of students, we would like to know if the courses were taken in their first, second, third, or whatever year. However, we only have the calendar years. Lets find out to what year of study each calendar year corresponds per student.

First, lets find out, which year was a student's first year and which year was a student's last year (we will also create a variable `Subs_year` that we will substract later to transform from Calendar Year to UCM Year, and a second variable `LengthUCM` that shows us how many years each student stayed at the college):

``` r
d_year <- d_grade %>%
  select(ID, Code, Year, Period, Grade,`Booking Status Description`, `Course concentration`) %>%
  group_by(ID) %>%
  summarise(FirstYear=min(Year), LastYear=max(Year)) %>%
  mutate(LengthUCM = LastYear-FirstYear, Subs_year=FirstYear-1)
```

Now, lets make a dataframe of students against years, where the year appears in the corresponding cell if a student was in the college at that time:

``` r
d_year1 <- d_grade %>%
  select(ID, Code, Year, Period, Grade, `Booking Status Description`, `Course concentration`) %>%  
  group_by( ID, Year) %>%
  summarise(Count = n())%>%
  mutate(Present = case_when(Count > 0 ~ Year))%>%
  select(-Count) %>%
  spread(Year,Present, fill = 0)
```

Now we have collected all the years a student has been at the college in a single row (one row per student), and we also know what is the first and last year of each student. Let's join these dataframes:

``` r
d_time <- left_join(d_year1, d_year)
```

Ideally, we would like to have a dataframe of students against calendar years filled with the corresponding UCM year (e.g. if the year 2017 was the first year of student 45, we would like to see the cell corresponding to student 45 and year 2017 marked with a "1").

``` r
var_years <- as.character(2007:2017)
d_time <- d_time %>% 
  mutate_at(
    vars(var_years),
    funs(. - Subs_year)) %>%
  mutate_at(
    vars(var_years),
    funs(case_when(. < 0 ~ 9999999999, #HORRIBLE FIX
                   T     ~ .)))
d_time[d_time == 9999999999] <- NA
```

Now, lets get the data as we want it for our analysis. For our analysis we would like to know what course a student took, when, whether they passed or failed, what grade they got, what the concentration of the student is, and what the concentration of the course is (we will convert grades to numeric here). Then, we would like to know on what year of their studies the student took the course.For this we will create a years dictionary and join it with our student data:

``` r
d_students <- d_grade %>%
  select(ID, Code, Year, Period, Grade, Attempt,`Booking Status Description`, `Course concentration`) %>%
  arrange(ID, Year, Period)%>%
  left_join(d_student_concentration, by="ID") %>%
  mutate(Grade = as.numeric( sub(",",".", Grade))) %>%
  replace_na(list(Grade=-1))
# Years dictionary:
year_dic <- d_time %>%
  select(-FirstYear,-LastYear,-LengthUCM,-Subs_year)%>%
  gather(Cal_Year,UCM_Year,var_years)%>%
  drop_na(.) %>%
  mutate(Year=as.integer(Cal_Year))%>%
  select(-Cal_Year)
#adding the ucm year to transcripts:
d_students <- d_students %>%
  left_join(year_dic, by=c("ID","Year"))
#removing repeated values:
d_students <- d_students[!duplicated(d_students),]
```

### Removing master Program data and Cleaning Capstone Data

We noticed that we had the data for people who stayed at this university for their masters. To filter these courses out we find out in which year people took their capstone courses.

For this we keep only the capstones. We expect some people to have repeated capstones:

``` r
capstone_count <- d_students %>%
  filter(Code=="CAP3000") %>%
  group_by(ID)%>%
  summarise(Count=n())
capstone_count
```

However, what is surptising is that some people have two different passed capstones:

``` r
passed_capstone_count <- d_students %>%
  filter(Code=="CAP3000", `Booking Status Description`=="Completed with Success") %>%
  group_by(ID) %>%
  summarise(Count=n(), Max = max(Grade), Min = min(Grade), Attempts= max(Attempt))
```

We can check the grades for people who had repeated passed capstones (lets just bring them to the top of the list):

``` r
passed_capstone_count %>%
  arrange(desc(Count), desc(Min), Attempts)
```

There are 1712 people who did capstone. We see that most students who have two passed Capstones, have in reality only 1, since the other capstone had a failing grade and the passing grade was on the second attempt (count starts at 0) was indeed failed. However there are two students who indeed have two different passed Capstones: one is student 282995 who has two different passing grades(because of a change in grading system at the collegue during the year 2008-2009), the other is student 544450 who has the same grade passed at two different moment in time.

We can check if there are any students with passing capstone who do not have a grade of at least 5.5. We do this by looking at the lowest Max values:

``` r
passed_capstone_count %>%
  arrange(Max)
```

We see the lowest grade for a passing capstone is 5.6, so this is fine.

Now lets just keep the highest value for each student and correct for student 282995:

``` r
passed_capstone_grades <- passed_capstone_count %>%
  mutate(`Booking Status Description`= "Completed with Success", Code="CAP3000", Grade= case_when(ID== 282995~7.7,
                                                                                  T~Max)) %>%
  select( ID, Code, Grade,`Booking Status Description`)
passed_capstone_grades
```

#### Capstone Year

Now we can figure out the Capstone Year.

First we get the years for which there is a corresponding passing capstone grade, and check for repetition:

``` r
capstone_finish_year <- inner_join(passed_capstone_grades, d_students) 
head(capstone_finish_year)
count_before <- passed_capstone_grades %>%
  group_by(ID) %>%
  summarise(counts_of_id= n())
count_after <- capstone_finish_year %>%
  group_by(ID) %>%
  summarise(counts_of_id= n())%>%
  filter(counts_of_id > 1)  #these students have repeated capstones.
count_after
```

From count\_after we see that student 544450 has two passed capstones at different moments in time, we already knew this and we must ask Richard why this is the case. However, since both capstones where passed in 2010, this creates no problem for our current purposes. However, let's remove the later version. We will also rename `Year` to `Capstone Year` and keep only the columns `Capstone Year` and `ID`

``` r
to_remove <- capstone_finish_year%>%
  filter(ID==544450)
to_remove <- to_remove[duplicated(to_remove$ID),]
capstone_finish_year <- capstone_finish_year%>%
  anti_join(to_remove) %>%
  rename(`Capstone Year`= Year)%>%
  select(ID,`Capstone Year`)
  
capstone_finish_year
```

Lets create a dataframe where we include the capstone year. Then, if a course is given later than the capstone year, we want to remove it. Therefore, lets substitute na values with the year 9999 (or will ucm be open in the year 9999?).

``` r
d_transcripts <- d_students %>%
  group_by(ID) %>%
  left_join(capstone_finish_year) %>%
  ungroup() %>%
  replace_na(list(`Capstone Year`=9999)) %>%
  mutate(`After Graduation`=`Capstone Year`-`Year`)
d_transcripts
```

Now we will remove all courses with negative `After Graduation`:

``` r
master_courses <- d_transcripts%>%
  filter(`After Graduation`<0)
d_transcripts <- d_transcripts %>%
  anti_join(master_courses)
head(d_transcripts)
```

One last thing. The variable `Booking Status Description` shows whether a student passed or failed the course. However, some mistakes in the way the data was handled meant that the course status of some students was never confirmed. We need to ASK RICHARD, but for the moment, we will just look at the grades of those courses which were not confirmed to change into pass and fail.

``` r
booked <- filter(d_transcripts, d_transcripts$`Booking Status Description`=="Booked")%>%
  mutate(`Booking Status Description`= case_when(Grade<5.5~"Completed Unsuccessfully",
                                                 Grade>5.5~"Completed with Success"))
clean_transcripts <- d_transcripts %>% 
  left_join(booked, by= c("ID", "Code", "Year", "Period", "Grade", "Attempt", "Course concentration", "student_Concentration", "Capstone Year", "After Graduation"), suffix=c("-old", "-new"))%>%
  select(-`After Graduation`)%>%
  mutate(Pass= case_when(
    (`Booking Status Description-new`=="Completed with Success")~"Pass",
    (`Booking Status Description-new`=="Completed Unsuccessfully")~"Fail",
    (`Booking Status Description-old`=="Completed with Success")~"Pass",
    (`Booking Status Description-old`=="Completed Unsuccessfully")~"Fail",
    T~"error in clean_transcripts"
  )) %>%
  select(-`Booking Status Description-new`,-`Booking Status Description-old`,-"UCM_Year-new")
```

#### Ordering in Time

We would like to have a column `Time` that codes for both year and period (e.g. Year\_1 Period\_2 should be 1.2), as well as a column `Periods` with the year and period as numbers (e.g. Year\_2 Period\_1 should be period 7).

CHECK: we still have students who's first record is of their last year. This appears as UCM\_Year=1.

``` r
a_clean_transcripts <- clean_transcripts%>%
  separate(Period, 
           c("T1","T2","T3","T4","T5","T6"), 
           sep="," 
           ) %>%
  gather( key = "Time", value = "Period", T1,T2,T3,T4,T5,T6) %>%
  drop_na(Period) %>%
  select(-Time) %>%
  mutate(Periods = (`UCM_Year-old` - 1) * 6 + 
           as.numeric(Period)
         )
  
a_clean_transcripts$Time= paste(a_clean_transcripts$`UCM_Year-old`,
                                a_clean_transcripts$Period, 
                                sep="."
                                )
#FINAL DATA:
d_transcript <-a_clean_transcripts%>%
  select(-`UCM_Year-old`,-Period)
```

NOTES: The data is now arranged so as to respect the order in time for each individual student. However, the comparison is still tricky as people who finished in 2008 will have CAP recorded as taken on their first year since this is all the data we have. COMMENT SOFIA: save a\_clean\_transcript as d\_transcript

Save Data
=========

``` r
save(lists, d_course, d_AoD, d_assessment, d_transcript,
     file = "data_pillar_1.RDATA")
save(lists, d_course, d_AoD, d_assessment, d_transcript,
     d_overview, d_description, d_manual,
     file = "data_pillar_2.RDATA")
```

[1] We do not include all assessments and all AoDs on the plots in order to keep them readable.
