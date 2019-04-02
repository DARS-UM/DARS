Data Preparation - Course Catalogues
================
DARS
2019-03-28

-   [Helper functions](#helper-functions)
-   [Importing course data](#importing-course-data)
    -   [Application](#application)
-   [Extracting course overviews](#extracting-course-overviews)
    -   [ES, PSY](#es-psy)
    -   [UCM, UCV](#ucm-ucv)
    -   [Application](#application-1)
-   [Clean data](#clean-data)
    -   [Tidy data](#tidy-data)
    -   [Stem data](#stem-data)
-   [Remove stopwords](#remove-stopwords)
-   [Save data](#save-data)

Helper functions
================

``` r
extract_id_pdf    <- function(string) string %>% str_sub(start = 1, end = - 5)
extract_id_course <- function(string) string %>% str_sub(start = 1, end =   7)

collapse_or       <- function(string) string %>% str_c(collapse = "|")
collapse_space    <- function(string) string %>% str_c(collapse = " ")

symbol_to_line    <- c("\r\n", "\n") %>% collapse_or
split             <- function(string) string %>% str_split(pattern = symbol_to_line) %>% .[[1]]

remove_header_UCV <- function(string) string %>% str_sub(29, -1)

overview_section_start <- function(string) string %>% str_detect(pattern = "^ *Core Courses")
overview_section_end   <- function(string) string %>% str_detect(pattern = "Appendix|^ *Courses at\r\n *Maastricht Science Programme")

headers_UCM_UCV <- c("^ *Core Courses",
                     "^Humanities \\(HUM\\)",
                     "^ *Life Sciences Courses",
                     "^Sciences \\(SCI\\)",
                     "^ *Social Sciences",
                     "^ *Skills Trainings",
                     "^ *Projects",
                     "^Undergraduate Research",
                     "Appendix") %>% collapse_or
is_header     <- function(string) string %>% str_detect(pattern = headers_UCM_UCV)

course_code_UCM_UCV <- c("COR", "HUM", "SCI", "SSC", "SKI", "PRO", "UGR", "CAP", # UCM
                         "VCO", "VSC", "VSS", "VSK", "VPR", "VCA" # UCV
                         ) %>% collapse_or
overview_start  <- function(string) string %>% extract_id_course %>% str_detect(pattern = course_code_UCM_UCV) # simplify with %in% course_code_UCM
```

Importing course data
=====================

``` r
read_in_pdfs <- function(file){
  
  Corpus(
    x             = DirSource(file),
    readerControl = list(reader = readPDF(control = list(text = "-layout")))
    ) %>%
    
    # turn corpus into a tibble to facilitate manipulation
    as.list %>% tibble(raw = .) %>%
    
    transmute(
      ID   = raw %>% map(function(x) x$meta$id) %>% map_chr(extract_id_pdf),
      text = raw %>% map(content) 
      )
  
}
```

Application
-----------

``` r
d_text <- list()
```

``` r
d_text$catalogues <- tribble(
  ~ ID              ,  ~ page_top                  , ~ before_overview                       ,
  "European_studies", "^.Bachelor European Studies", "Faculty of Arts and Social Sciences"   ,
  "Psychology"      , "^.Bachelor Psychology"      , "Faculty of Psychology and Neuroscience",
  "UCV"             , NA_character_                , NA_character_                           ,
  "UCM"             , NA_character_                , NA_character_                                   
  ) %>%
  
  full_join(read_in_pdfs("./Input/Catalogues/UM"), by = "ID") %>%
  
  mutate(year = "2018-2019") %>%
  rename(catalogue = text)
```

    ## Warning: Column `ID` has different attributes on LHS and RHS of join

``` r
d_text$manuals <- read_in_pdfs("./Input/Manuals 2017-18") %>% rename(`Course ID` = ID) %>% mutate(text = text %>% map_chr(str_c, collapse = " "))
```

Extracting course overviews
===========================

In order to conduct a topic analysis of course content, we extract the *overview* and *description* of each course from the course catalogues, as well as the *full text* from the Course Manuals.

(For this section we use:`corpus_catalogues` contains the pdfs of the `5` most recent course catalogues.)

We start by extracting the overviews which are 1 or 2 page long and contain important information of a course. To accomplish this, for each catalogue (loop), we first use `grep` to determine which pages should be excluded from the analysis (`pages_to_exclude`). These pages are typically headers. Excluding them before the analysis allows use to keep the code relatively simple. We also make use the fact that the overviews always start with the code of the course, i.e. it starts with one of the elements of `course_code`; and we use `grep` to identify the first page of each overview. We use grep a 3rd time to identify the page containing the header *Core Coureses (COR)* and which marks the beginning of the overviews in the catalogue, and we loop from this page to the last page of the catalogue.

In the loop, we first determine the content of the current and following page. If the current page is the first page of an overview, then we identify the overview (one or two pages) and save it to the tibble `d_description`. To identify the overview of a course, we check if the following page is either the first page of an overview or a page to exclude. If it is either of these, then the overview correspond to the content of the current page; but if the following page is neiter of these, then the overview correspond to the content of the current page and that of the following page.

ES, PSY
-------

``` r
overview_ES_PSY <- function(catalogue, ID, page_top, before_overview){

  catalogue %>%
    
    # create tibble to facilitate manipulation
    collapse_space %>% split %>% tibble(line = .) %>%
    
    # remove page numbers and page top to faciliate the identification of the course description starts
    mutate(page_top    = str_detect(line, page_top),
           page_number = page_top %>% lead(default = FALSE)) %>%
    
    filter(!page_top, !page_number) %>%
    
    # identify course descriptions
    mutate(overview_start = str_detect(line, before_overview) %>% lag(default = FALSE)) %>%
    mutate(overview_id    = cumsum(overview_start)) %>%
    
    filter(overview_id != 0) %>% 
    
    # extract course description
    group_by(overview_id) %>% 
    summarize(text        = line %>% str_c(collapse = " "),
              `Course ID` = line %>% str_c %>% .[[1]]) %>%
    mutate(department  = ID) %>%
    select(-overview_id)
  
}
```

UCM, UCV
--------

``` r
paste_if_2_page_overview <- function(page, page_following){
  
  if(! page %>% overview_start) NA_character_ # if the page is not the start of an overview, return an NA
  
  else{
    if(page_following %>% overview_start | is.na(page_following))  page # include possibility of NA for last page of overview section
    else                                                           str_c(page, page_following)
  }
  
}
```

``` r
overview_UCM_UCV <- function(catalogue, ID){
  
  if(ID == "UCV") catalogue <- catalogue %>% remove_header_UCV
  
  # create tibble to facilitate manipulation
  catalogue %>% tibble(page = .) %>%
  
  # identify start and end of overview section of catalogue
  mutate(overview_section_start = page %>% overview_section_start,
         overview_section_end   = page %>% overview_section_end  ) %>%
  
  # filter the overview section
  mutate(after_overview_section_start = cumany( overview_section_start),
         before_overview_section_end  = cumall(!overview_section_end  )) %>%
  filter(after_overview_section_start & before_overview_section_end    ) %>%
  
  # exclude headers to only keep the course overviews themselves
  filter(! page %>% is_header) %>%
  
  # paste current page and following page (`lead(page)`) if overview is two page long
  transmute(text        = list(page, lead(page)) %>% pmap_chr(paste_if_2_page_overview),
            `Course ID` = text %>% extract_id_course) %>%
    mutate(department  = ID) %>%
    filter(!is.na(text))
    
}
```

Application
-----------

``` r
overview <- function(ID, catalogue, page_top, before_overview){
  
  if(ID %in% c("European_studies", "Psychology")){
    catalogue %>% overview_ES_PSY(ID = ID, page_top = page_top, before_overview = before_overview)
  
  }else if(ID %in% c("UCM", "UCV")){
    catalogue %>% overview_UCM_UCV(ID = ID)
  
  }else "to do"
  
}
```

``` r
d_text$catalogues <- d_text$catalogues %>% mutate(overviews = list(ID, catalogue, page_top, before_overview) %>% pmap(overview))
```

``` r
d_text$catalogues <- d_text$catalogues$overviews %>% bind_rows
```

Clean data
==========

Tidy data
---------

To put everything into tidy text format, we first create three tibbles `d_overview_tidy`, `d_description_tidy`, `d_manual` that respectively store the overviews, descriptions and text from manuals in the tidy text format, with one row per course-year-word (and course-word for `d_manual`).

``` r
tidy_text <- function(df) df %>% unnest_tokens(output = word, input = text)
```

``` r
d_text <- d_text %>% map(tidy_text) %T>% print
```

    ## $catalogues
    ## # A tibble: 157,172 x 3
    ##    `Course ID`                                   department      word      
    ##    <chr>                                         <chr>           <chr>     
    ##  1 Language & professional skills: English Diag~ European_studi~ language  
    ##  2 Language & professional skills: English Diag~ European_studi~ professio~
    ##  3 Language & professional skills: English Diag~ European_studi~ skills    
    ##  4 Language & professional skills: English Diag~ European_studi~ english   
    ##  5 Language & professional skills: English Diag~ European_studi~ diagnostic
    ##  6 Language & professional skills: English Diag~ European_studi~ test      
    ##  7 Language & professional skills: English Diag~ European_studi~ full      
    ##  8 Language & professional skills: English Diag~ European_studi~ course    
    ##  9 Language & professional skills: English Diag~ European_studi~ descripti~
    ## 10 Language & professional skills: English Diag~ European_studi~ this      
    ## # ... with 157,162 more rows
    ## 
    ## $manuals
    ## # A tibble: 1,114,195 x 2
    ##    `Course ID` word 
    ##    <chr>       <chr>
    ##  1 COR1002     p    
    ##  2 COR1002     h    
    ##  3 COR1002     i    
    ##  4 COR1002     l    
    ##  5 COR1002     o    
    ##  6 COR1002     s    
    ##  7 COR1002     o    
    ##  8 COR1002     p    
    ##  9 COR1002     hy   
    ## 10 COR1002     o    
    ## # ... with 1,114,185 more rows

``` r
rm(tidy_text)
```

Stem data
---------

We then use the function `hunspell_stem`, which returns valid stems for a given word, to stem the words in the tibbles `d_overview_tidy` and `d_description_tidy`.

We first create a function `stem_hunspell` which, given a term, returns its most basic stem (the last one from the list of stem returned by `hunspell_stem`). We would like to apply `stem_hunspell` on the words of `d_overview_tidy` and `d_manual`, since they have similar structures, we row bind them into `dictionary` to ease the application of `stem_hunspell`. However, since, `stem_hunspell` is not a vectorized function and the number of words in `dictionary` is large, we first use distinct on the `word` variable to find a list containing *once* each term present in the course overviews and manuals. Then, we use `mutate` to apply the function `stem_hunspell` on each word from our dictionary and save its results as a new column `word_stem`.

``` r
stem <- function(term) {
  
  stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
  n_stems <- length(stems)            
  if (n_stems == 0) term              # if no stems, return original term
  else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem
  
}
```

``` r
dictionary <- union(d_text$catalogues$word, d_text$manuals$word) %>%
  
  tibble(word = .) %>%
  
  mutate(word_stem = word %>% map_chr(stem))

# terms for which the stem differs from the original word
filter(dictionary, word != word_stem)
```

    ## # A tibble: 11,180 x 2
    ##    word        word_stem
    ##    <chr>       <chr>    
    ##  1 skills      skill    
    ##  2 aims        aim      
    ##  3 determining determine
    ##  4 students    student  
    ##  5 making      make     
    ##  6 studies     study    
    ##  7 lowest      low      
    ##  8 scoring     score    
    ##  9 writing     writ     
    ## 10 trainers    train    
    ## # ... with 11,170 more rows

``` r
# TODO: "destem" terms by hand: cuss => discuss
filter(dictionary, word_stem == "cuss")
```

    ## # A tibble: 4 x 2
    ##   word       word_stem
    ##   <chr>      <chr>    
    ## 1 discussed  cuss     
    ## 2 discussing cuss     
    ## 3 discuss    cuss     
    ## 4 discusses  cuss

Finally, we create a function `stem_with_dictionary` that performs a left join on the dataframe it takes as input with `dictionary`, thus adding the word stems to the original dataframe.

Then, we use `stem_with_dictionary` to include the stems of all words in `d_description`, `d_overview`, and `d_manual`.

``` r
stem_with_dictionary <- function(data){
  
  data %>% left_join(dictionary, by = "word") %>%
    
    rename(word_original = word,
           word          = word_stem)
  
}
```

``` r
d_text <- d_text %>% map(stem_with_dictionary)

rm(stem, stem_with_dictionary)
```

Remove stopwords
================

Finally, we want to filter out some uninformative words from our textual data. For this, we first store all the unwanted words in a vector `sw_owm`. We also create function `remove_sw` which receives a dataframe as input and filters out all words in `sw_own`. Then we apply this function to `d_description`, `d_overview`, and `d_manual`.

``` r
sw_own <- 1 : 1e3 %>% as.character %>% tibble( word = .)
```

``` r
remove_sw <- function(data){
  
  data %>%
    
    anti_join(stop_words, by = "word") %>%
    anti_join(sw_own    , by = "word") %>%
    
    # remove words of 1 or 2 letters
    filter(nchar(word) > 2) %>%
    
    # Remove words occuring once or twice in corpus (usually names, website links or typos)
    add_count(word) %>%
    filter(n > 2) %>%
    select(-n) %>%
    
    # remove NA
    filter(!is.na(word))
  
}
```

``` r
d_text <- d_text %>% map(remove_sw) %T>% print
```

    ## $catalogues
    ## # A tibble: 76,981 x 4
    ##    `Course ID`                         department    word_original word    
    ##    <chr>                               <chr>         <chr>         <chr>   
    ##  1 Language & professional skills: En~ European_stu~ language      language
    ##  2 Language & professional skills: En~ European_stu~ professional  profess~
    ##  3 Language & professional skills: En~ European_stu~ skills        skill   
    ##  4 Language & professional skills: En~ European_stu~ english       english 
    ##  5 Language & professional skills: En~ European_stu~ diagnostic    diagnos~
    ##  6 Language & professional skills: En~ European_stu~ test          test    
    ##  7 Language & professional skills: En~ European_stu~ description   descrip~
    ##  8 Language & professional skills: En~ European_stu~ compulsory    compuls~
    ##  9 Language & professional skills: En~ European_stu~ diagnostic    diagnos~
    ## 10 Language & professional skills: En~ European_stu~ test          test    
    ## # ... with 76,971 more rows
    ## 
    ## $manuals
    ## # A tibble: 482,526 x 3
    ##    `Course ID` word_original word       
    ##    <chr>       <chr>         <chr>      
    ##  1 COR1002     fall          fall       
    ##  2 COR1002     2017          2017       
    ##  3 COR1002     cor           cor        
    ##  4 COR1002     1002          1002       
    ##  5 COR1002     cor           cor        
    ##  6 COR1002     1002          1002       
    ##  7 COR1002     philosophy    philosophy 
    ##  8 COR1002     science       science    
    ##  9 COR1002     contents      content    
    ## 10 COR1002     information   information
    ## # ... with 482,516 more rows

``` r
rm(sw_own, remove_sw)
```

Save data
=========

``` r
save(d_text, file = "Output/d_text.RDATA")
```
