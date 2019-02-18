Data Preparation
================
DARS
2019-02-15

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

``` r
knitr::opts_chunk$set(cache.path = "Cache/Data Preparation/")
```

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

First, we import the spreadsheet with information pertraining the Aims of the Degree (AoD) and Assessments from the drive and save it under `lists_brut`. `lists_brut` contains 4 columns, under which we find the `19` types of assessment, the `18` aims of the degree (AoD) of the degree, and two columns containing binary vectors indicating which assessment types and AoD we will consider when ploting the data\[^1\]. Then we create a list with this same columns, but instead of having binary vectors for the plots, we keep vectors of only the names of relevant assessments and AoDs for the plots (`Assessment_plot`adnd `AoD_plot` respectively). We also any imported emtpy cells.

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
  select(- `Comment on Assessment`) %>%
  gather(Assessment, assessment_covered, - `Course ID`, factor_key = T) %>%
  filter(assessment_covered == 1) %>%
  select(- assessment_covered) %>%
  arrange(`Course ID`)
print(d_assessment)
```

    ## # A tibble: 482 x 2
    ##    `Course ID` Assessment    
    ##    <chr>       <fct>         
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
d_ILO <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1896457050') %>%
  select(- c(Comments, Comments_for_Jeroen)) %>%
  gather(AoD, AoD_covered, -c(`Course ID`, Objectives), factor_key = T) %>%
  filter(AoD_covered == 1) %>%
  select(- AoD_covered) %>%
  rename(ILO = Objectives) %>%
  arrange(`Course ID`)
print(d_ILO)
```

    ## # A tibble: 1,232 x 3
    ##    `Course ID` ILO         AoD                   
    ##    <chr>       <chr>       <fct>                 
    ##  1 AAM2001     Not Defined Academic Expertise    
    ##  2 AAM2001     Not Defined Reflective Skills     
    ##  3 AAM2001     Not Defined Decision-making Skills
    ##  4 AAM2002     Not Defined Academic Expertise    
    ##  5 AAM2002     Not Defined Reflective Skills     
    ##  6 AAM2002     Not Defined Decision-making Skills
    ##  7 AAM2003     Not Defined Academic Expertise    
    ##  8 AAM2003     Not Defined Reflective Skills     
    ##  9 AAM2003     Not Defined Decision-making Skills
    ## 10 AAM2004     Not Defined Academic Expertise    
    ## # ... with 1,222 more rows

Textual Data
------------

### Course Catalogues

In order to conduct a preliminary topic modeling of course content, we first analyze their description in the course catalogues. The corpus `corpus_catalogues` contains the pdfs of the `5` most recent course catalogues.

``` r
corpus_catalogues <- Corpus(x             = DirSource("./Input/Catalogues"),
                            readerControl = list(reader = readPDF(control = list(text = "-layout"))))
```

### Course Manuals

To expand our topic modeling of course content, we analyse the material in the course manuals. The `corpus_manuals` contains the pdfs of the course manuals for the year 2017-2018 (most recently, available).

``` r
corpus_manuals <- Corpus(x             = DirSource("./Input/Manuals 2017-18"),
                         readerControl = list(reader = readPDF(control = list(text = "-layout"))))
```

Student Data
------------

The tibble `d_transcript` contains the transcript information of students as was provided. It has 40 columns, and rows correspond to a type of grade (e.g. final grade, attendance) per student per course per time they took it.

``` r
d_transcript <- rbind(
  read_csv(
    "./Input/Raw Grades/grades1.csv",
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
    "./Input/Raw Grades/grades2.csv",
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

Course Data
-----------

The analysis performed on the course data is aimed at discoving what the contribution of each course is towards the fullfilment of the AoDs, and comparing different types of curricula, or programs, based on this infromation.

### AoD

In this analysis, we conside that a course can cover an AoD in two ways: a course covers an AoD if one of its ILOs covers it, or if one of its assessments cover it. For instance, if one of ILO of a course states that the students will learn to analyze empirical data in the context of academic research, then the course in question covers the AoD `Research Skills`; and if one of the assessment is a group presentation, then the course also covers the AoD `Collaborative Skills` and `Communication Skills`.

#### AoD from ILOs

(For this section we use: `d_ILO`-indicates which AoD(s) the intended learning objectives (ILOs) of the courses cover with one row per course-ILO-AoD)

In order to determine which AoD each course covers with its ILOs, we first eliminate the AoD that are not covered by the ILOs, and then we keep only one instance of each combination of course and AoD in case a course had several ILOs covering the same AoD.

``` r
d_AoD_ILO <- distinct(d_ILO, `Course ID`, AoD)
print(d_AoD_ILO)
```

    ## # A tibble: 681 x 2
    ##    `Course ID` AoD                   
    ##    <chr>       <fct>                 
    ##  1 AAM2001     Academic Expertise    
    ##  2 AAM2001     Reflective Skills     
    ##  3 AAM2001     Decision-making Skills
    ##  4 AAM2002     Academic Expertise    
    ##  5 AAM2002     Reflective Skills     
    ##  6 AAM2002     Decision-making Skills
    ##  7 AAM2003     Academic Expertise    
    ##  8 AAM2003     Reflective Skills     
    ##  9 AAM2003     Decision-making Skills
    ## 10 AAM2004     Academic Expertise    
    ## # ... with 671 more rows

#### AoD from Assessment

(For this section we use: `d_assessment`-indicates which type(s) of assessment each course contains with one row per course-assessment)

In order to determine which AoD each course covers with its assessments, we need to create a binary matrix which indicates which AoD(s) each assessment type covers. We have created such matrix on our google drive and we save it in the following piece of code as `map_assessment_AoD`. `map_assessment_AoD` indicates that, for instance, the assessment type `Essay` covers the AoD `Critical Thinking Skills`, `Communication Skills` and `Writing Skill`. Thus, the first step is to import this matrix:

``` r
map_assessment_AoD <- as.matrix(gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=719531216')[,lists$AoD] %>%
                                  mutate_all(as.logical))
rownames(map_assessment_AoD) <- lists$Assessment

# Print section of matrix
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
d_AoD_assessment <- tibble(`Course ID` = character(0),
                           Assessment  = character(0),
                           AoD         = character(0))

for(i in 1 : nrow(d_assessment)){
  
  observation <- d_assessment[i, ]
  
  assessments <- observation$Assessment
  AoD_covered <- map_assessment_AoD[assessments, ]
  AoD         <- lists$AoD[AoD_covered]
  
  if(length(AoD) >= 1) d_AoD_assessment <- rbind(d_AoD_assessment,
                                                 as_tibble(cbind(observation,
                                                                 AoD)))
}
rm(map_assessment_AoD, observation, AoD, AoD_covered, assessments, i)
```

Next, we keep ony one copy of each distinct combination of course and AoD in case a course had several assessments covering the same AoD (this is analogous to what we did with the tibble `d_AoD_ILO`).

``` r
d_AoD_assessment <- distinct(d_AoD_assessment, `Course ID`, AoD)
print(d_AoD_assessment)
```

    ## # A tibble: 732 x 2
    ##    `Course ID` AoD                     
    ##    <chr>       <fct>                   
    ##  1 AAM2001     Communication Skills    
    ##  2 AAM2002     Communication Skills    
    ##  3 AAM2003     Communication Skills    
    ##  4 AAM2004     Communication Skills    
    ##  5 AAM2005     Communication Skills    
    ##  6 AAM2006     Communication Skills    
    ##  7 AAM2007     Communication Skills    
    ##  8 CAP3000     Problem Solving Skills  
    ##  9 CAP3000     Critical Thinking Skills
    ## 10 CAP3000     Communication Skills    
    ## # ... with 722 more rows

#### Combining AoD from ILOs and from assessments

Finally, we can use a `rbind` to combine the two tibbles `d_AoD_ILO` and `d_AoD_assessment`. We also use `distinct` in case a course covers an AoD with both its ILOS and its assessments.

``` r
d_AoD <- rbind(d_AoD_ILO,
               d_AoD_assessment) %>%
  distinct %>%
  arrange(`Course ID`)

rm(d_AoD_ILO, d_AoD_assessment)
print(d_AoD)
```

    ## # A tibble: 1,231 x 2
    ##    `Course ID` AoD                   
    ##    <chr>       <fct>                 
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
    ## # ... with 1,221 more rows

### Courses

(In this section we use: `d_course`- contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered.)

Now that we have a clear overview of the distribution of AoDs (`d_AoD`) and assessments (`d_assessment`) among the courses, let us add variables to the tibble `d_course` that contain the information at the course level. For this we create the following three tibbles: 1) `d_ILO_detail` contains two columns indicating the code of the course and the number of ILOs it contains. 2) `d_assessment_detail` contains three columns indicating the code of the course, the number of assessments it covers and a list of the assessments it covers. 3) `d_AoD_detail` contains three columns indicating the code of the course, the number of AoD it covers and a list of the AoD it covers. Then we use a `full_join` to add these variables to the tibble `d_course`.

``` r
d_ILO_detail <- d_ILO %>% 
  distinct(`Course ID`, ILO) %>%
  count(`Course ID`) %>%
  rename(n_ILO = n)

d_assessment_detail <- d_assessment %>%
  group_by(`Course ID`) %>%
  summarise(n_assessment = n(),
            `Assessments Covered` = paste(Assessment, collapse = ", "))

d_AoD_detail <- d_AoD %>%
  group_by(`Course ID`) %>%
  summarise(n_AoD = n(),
            `AoD Covered` = paste(AoD, collapse = ", "))

d_course_detail <- d_ILO_detail %>%
  full_join(d_assessment_detail, by = "Course ID") %>%
  full_join(d_AoD_detail       , by = "Course ID")

rm(d_ILO_detail, d_assessment_detail, d_AoD_detail)
print(d_course_detail)
```

    ## # A tibble: 280 x 6
    ##    `Course ID` n_ILO n_assessment `Assessments Cove~ n_AoD `AoD Covered`   
    ##    <chr>       <int>        <int> <chr>              <int> <chr>           
    ##  1 AAM2001         1            1 Oral                   4 Academic Expert~
    ##  2 AAM2002         1            1 Oral                   4 Academic Expert~
    ##  3 AAM2003         1            1 Oral                   4 Academic Expert~
    ##  4 AAM2004         1            1 Oral                   4 Academic Expert~
    ##  5 AAM2005         1            1 Oral                   4 Academic Expert~
    ##  6 AAM2006         1            1 Oral                   4 Academic Expert~
    ##  7 AAM2007         1            1 Oral                   4 Academic Expert~
    ##  8 CAP3000         3            3 Paper, Presentati~    10 Advanced Knowle~
    ##  9 COR1002         3            2 Essay, Written Ex~     7 Core, Elementar~
    ## 10 COR1003         3            3 Paper, Written Ex~     7 Core, Elementar~
    ## # ... with 270 more rows

``` r
d_course <- d_course %>%
  full_join(d_course_detail, by = "Course ID") %>%
  select(`Course ID`, `Course Title`, Cluster,
         n_ILO,
         n_assessment, `Assessments Covered`,
         n_AoD, `AoD Covered`, 
         everything())

rm(d_course_detail)
print(d_course)
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

The undegraduate research courses (*UGR-*) are only present at the `3000` level (advanced level). Yet, these course are also offered at the `2000` level (intermediate level). We use an `rbind` to duplicate the rows of the course `UGR3000` and mutate their `Code` to `UGR2000`.

``` r
d_course <- d_course %>%
  rbind(filter(., `Course ID` %in% c("UGR3001", "UGR3002", "UGR3003", "UGR3005")) %>%
          mutate(`Course ID` = case_when(`Course ID` == "UGR3001" ~ "UGR2001",
                                  `Course ID` == "UGR3002" ~ "UGR2002",
                                  `Course ID` == "UGR3003" ~ "UGR2003",
                                  `Course ID` == "UGR3005" ~ "UGR2005")))

print(filter(d_course, `Course ID` %in% c("UGR2001", "UGR2002", "UGR2003", "UGR2005",
                                          "UGR3001", "UGR3002", "UGR3003", "UGR3005")))
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

Finally, we add a series of informative variable at the course level.

``` r
d_course <- d_course %>%
  mutate(
    Letters = substring(`Course ID`, 1, 3),
    Number  = as.numeric(substring(`Course ID`, 4, 7)),
    Level   = case_when(between(Number, 1000, 1999) ~ "Introductory",
                        between(Number, 2000, 2999) ~ "Intermediate",
                        between(Number, 3000, 3999) ~ "Advanced"    ))

print(select(d_course, `Course ID`, Letters, Number, Level))
```

    ## # A tibble: 284 x 4
    ##    `Course ID` Letters Number Level       
    ##    <chr>       <chr>    <dbl> <chr>       
    ##  1 AAM2001     AAM       2001 Intermediate
    ##  2 AAM2002     AAM       2002 Intermediate
    ##  3 AAM2003     AAM       2003 Intermediate
    ##  4 AAM2004     AAM       2004 Intermediate
    ##  5 AAM2005     AAM       2005 Intermediate
    ##  6 AAM2006     AAM       2006 Intermediate
    ##  7 AAM2007     AAM       2007 Intermediate
    ##  8 CAP3000     CAP       3000 Advanced    
    ##  9 COR1002     COR       1002 Introductory
    ## 10 COR1003     COR       1003 Introductory
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
academic_years <- c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019")
d_description <- tibble(`Course ID`     = character(0),
                        `Academic Year` = character(0),
                        Overview        = character(0),
                        Description     = NA)

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
    overview <- if((page + 1) %in% first_pages_overview) cat_overview[page]
                else paste(cat_overview[page : (page + 1)], collapse = " ")
    
    # Save overview
    code <- substr(overview, start = 1, stop = 7)
    year <- academic_years[n]
    d_description <- d_description %>%
      add_row(
        `Course ID` = code,
        `Academic Year` = year,
        Overview = overview,
        Description = NA
      )
    
  } # close for-loop (page)
  
} # close for-loop (n)

rm(corpus_catalogues,
   n_catalogue, academic_years, course_code, content_to_exclude,
   n, cat,
   page_overview_start, page_overview_end,
   pages_to_exclude, cat_overview,
   first_three_letters, first_pages_overview,
   page, overview, code, year)
print(d_description)
```

    ## # A tibble: 831 x 4
    ##    `Course ID` `Academic Year` Overview                         Description
    ##    <chr>       <chr>           <chr>                            <lgl>      
    ##  1 COR1002     2014-2015       "COR1002 - Philosophy of Scienc~ NA         
    ##  2 COR1003     2014-2015       "COR1003 - Contemporary World H~ NA         
    ##  3 COR1004     2014-2015       "COR1004 - Political Philosophy~ NA         
    ##  4 COR1005     2014-2015       "COR1005 - Modeling Nature\r\nC~ NA         
    ##  5 HUM1003     2014-2015       "HUM1003 - Cultural Studies I: ~ NA         
    ##  6 HUM1007     2014-2015       "HUM1007 - Introduction to Phil~ NA         
    ##  7 HUM1010     2014-2015       "HUM1010 - Common Foundations o~ NA         
    ##  8 HUM1011     2014-2015       "HUM1011 - Introduction to Art;~ NA         
    ##  9 HUM1012     2014-2015       "HUM1012 - Pop Songs and Poetry~ NA         
    ## 10 HUM1013     2014-2015       "HUM1013 - The Idea of Europe: ~ NA         
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
```

    ## Warning: Unknown or uninitialised column: 'Calendar Year'.

    ## [1] "line_end irregular for UGR3003 in catalogue "

    ## Warning: Unknown or uninitialised column: 'Calendar Year'.

    ## [1] "line_end irregular for PRO2012 in catalogue "

    ## Warning: Unknown or uninitialised column: 'Calendar Year'.

    ## [1] "line_end irregular for UGR3003 in catalogue "

    ## Warning: Unknown or uninitialised column: 'Calendar Year'.

    ## [1] "line_end irregular for PRO2012 in catalogue "

    ## Warning: Unknown or uninitialised column: 'Calendar Year'.

    ## [1] "line_end irregular for UGR3003 in catalogue "

    ## Warning: Unknown or uninitialised column: 'Calendar Year'.

    ## [1] "line_end irregular for UGR3005 in catalogue "

``` r
rm(symbol_to_line, start_descrip, end_descrip,
   course, overview, overview_by_line, line_start, line_end, description)
print(d_description)
```

    ## # A tibble: 831 x 4
    ##    `Course ID` `Academic Year` Overview              Description           
    ##    <chr>       <chr>           <chr>                 <chr>                 
    ##  1 COR1002     2014-2015       "COR1002 - Philosoph~ Starting from classic~
    ##  2 COR1003     2014-2015       "COR1003 - Contempor~ The course intends to~
    ##  3 COR1004     2014-2015       "COR1004 - Political~ Politics is a complex~
    ##  4 COR1005     2014-2015       "COR1005 - Modeling ~ The aim of the course~
    ##  5 HUM1003     2014-2015       "HUM1003 - Cultural ~ Cultural Studies is a~
    ##  6 HUM1007     2014-2015       "HUM1007 - Introduct~ One of the greatest a~
    ##  7 HUM1010     2014-2015       "HUM1010 - Common Fo~ What do Europeans hav~
    ##  8 HUM1011     2014-2015       "HUM1011 - Introduct~ The traditional term ~
    ##  9 HUM1012     2014-2015       "HUM1012 - Pop Songs~ This course is based ~
    ## 10 HUM1013     2014-2015       "HUM1013 - The Idea ~ This course deals wit~
    ## # ... with 821 more rows

#### Extracting Text: Course Manuals

(For this section we use:`corpus_manuals`-contains the pdfs of the course manuals for the year 2017-2018 (most recently, available))

Since we want to use tools from dplyr and tidytext we must first transform our corpus into a dataframe.

``` r
d_manual <- tidy(corpus_manuals) %>%
  select(id, text) %>%
  mutate(`Course ID` = substring(id, 1, 7)) %>%
  select(-id) %>%
  rename(Manual = text)

rm(corpus_manuals)
print(d_manual)
```

    ## # A tibble: 153 x 2
    ##    Manual                                                       `Course ID`
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
d_overview <- d_description %>%
  select(`Course ID`, `Academic Year`, Overview) %>%
  unnest_tokens(output = word, input = Overview)

d_description <- d_description %>%
  select(`Course ID`, `Academic Year`, Description) %>%
  unnest_tokens(output = word, input = Description)

d_manual <- unnest_tokens(d_manual, output = word, input = Manual)
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

dictionary <- select(d_overview, `Course ID`, word) %>%
  rbind(d_manual) %>% 
  distinct(word) %>%
  mutate(word_stem = purrr::map_chr(.x = word,
                                    .f = stem_hunspell))

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
stem_with_dictionary <- function(data) data %>%
                                         left_join(dictionary, by = "word") %>%
                                         rename(word_original = word,
                                                word = word_stem)
  
d_description <- stem_with_dictionary(d_description)
d_overview <- stem_with_dictionary(d_overview)
d_manual <- stem_with_dictionary(d_manual)

rm(stem_hunspell, stem_with_dictionary)
print(d_description) # See starting - start
```

    ## # A tibble: 185,054 x 4
    ##    `Course ID` `Academic Year` word_original word       
    ##    <chr>       <chr>           <chr>         <chr>      
    ##  1 COR1002     2014-2015       starting      start      
    ##  2 COR1002     2014-2015       from          from       
    ##  3 COR1002     2014-2015       classical     classical  
    ##  4 COR1002     2014-2015       positions     position   
    ##  5 COR1002     2014-2015       on            on         
    ##  6 COR1002     2014-2015       the           the        
    ##  7 COR1002     2014-2015       objectivity   objectivity
    ##  8 COR1002     2014-2015       and           and        
    ##  9 COR1002     2014-2015       methodology   methodology
    ## 10 COR1002     2014-2015       of            of         
    ## # ... with 185,044 more rows

### Removing Stopwords

Finally, we want to filter out some uninformative words from our textual data. For this, we first store all the unwanted words in a vector `sw_owm`. We also create function `remove_sw` which receives a dataframe as input and filters out all words in `sw_own`. Then we apply this function to `d_description`, `d_overview`, and `d_manual`.

``` r
# Own stop words
sw_own <- c("______________________________________________________________________________",
            as.character(1:10))

remove_sw <- function(data) data %>%
  filter(! word %in% c(stop_words$word, sw_own))

d_description <- remove_sw(d_description)
d_overview <- remove_sw(d_overview)
d_manual <- remove_sw(d_manual)

rm(sw_own, remove_sw)
print(d_description) # stop words (on, the, of, etc, are excluded)
```

    ## # A tibble: 83,928 x 4
    ##    `Course ID` `Academic Year` word_original word       
    ##    <chr>       <chr>           <chr>         <chr>      
    ##  1 COR1002     2014-2015       starting      start      
    ##  2 COR1002     2014-2015       classical     classical  
    ##  3 COR1002     2014-2015       positions     position   
    ##  4 COR1002     2014-2015       objectivity   objectivity
    ##  5 COR1002     2014-2015       methodology   methodology
    ##  6 COR1002     2014-2015       science       science    
    ##  7 COR1002     2014-2015       logical       logical    
    ##  8 COR1002     2014-2015       empiricism    empiricism 
    ##  9 COR1002     2014-2015       critical      critical   
    ## 10 COR1002     2014-2015       rationalism   rationalism
    ## # ... with 83,918 more rows

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
save(d_course, d_transcript,
     file = "Output/data_pillar_1.RDATA")
save(lists, d_course, d_AoD, d_assessment, d_transcript,
     d_overview, d_description, d_manual,
     file = "Output/data_pillar_2.RDATA")
```
