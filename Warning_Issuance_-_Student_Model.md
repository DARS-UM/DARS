Pillar 1 - Student Topic Profile
================
DARS
2019-05-29

-   [Set up](#set-up)
-   [Topic Model](#topic-model)
-   [Students' Topic Expertise](#students-topic-expertise)
-   [Students' Academic Performance](#students-academic-performance)
-   [Join student TP and GPA](#join-student-tp-and-gpa)
-   [Save](#save)

**Considerations**:

-   extract course descriptions from courses not offer in 2018-2019 e.g. SCI2012.

-   give more weight to 3000-level courses

-   consider following predictors:
    -   GPA (per concentration)

Set up
======

Topic Model
===========

``` r
gamma <- app_model$Gamma[[1]] %>%  mutate(topic = topic %>% str_replace(" ", "_"))
gamma_spread <- gamma %>% spread(topic, gamma)
remove(app_model)
```

Students' Topic Expertise
=========================

``` r
student_TP <- d_transcript_augmented %>%
  left_join(gamma_spread, by = c("Course ID" = "document")) %>%
  
  # Knowledge acquired in each course
  mutate_at(vars(matches("Topic")), funs(if_else(is.na(.), 0, .))) %>% # give a value of 0 to the topics of the courses missing from the topic model.
  mutate_at(vars(matches("Topic")), funs(Grade/10 * .)) %>% # weigh by grade
  
  # Knowledge acquired in each period
  group_by(`Student ID`, time) %>%
  summarize_at(vars(matches("Topic")), sum) %>% 
  
  # Cumulative knowledge acquired over time
  group_by(`Student ID`) %>%
  arrange(time) %>%
  mutate_at(vars(matches("Topic")), lag, default = 0) %>% # knowledge only acquired at the END of the course
  mutate_at(vars(matches("Topic")), cumsum) %>%
  ungroup %>%
  
  arrange(`Student ID`, time)
```

Raphael's topic profile is coherent: topics with a high value correspond to my academic focus and topics with a low score to themes I never covered (i.e. law, literature).

Students' Academic Performance
==============================

``` r
d_transcript_augmented <- d_transcript_augmented %>%
  
  mutate(is_HUM = `Course ID` %>% str_detect("HUM|SAH"),
         is_SCI = `Course ID` %>% str_detect("SCI|SAS"),
         is_SSC = `Course ID` %>% str_detect("SSC|SAC"),
         is_COR = `Course ID` %>% str_detect("COR"),
         is_SKI = `Course ID` %>% str_detect("SKI|LAN"),
         is_PRO = `Course ID` %>% str_detect("PRO|UGR|CAP")) %>%
  
  mutate(grade_HUM = if_else(is_HUM, Grade, NA_real_),
         grade_SCI = if_else(is_SCI, Grade, NA_real_),
         grade_SSC = if_else(is_SSC, Grade, NA_real_),
         grade_COR = if_else(is_COR, Grade, NA_real_),
         grade_SKI = if_else(is_SKI, Grade, NA_real_),
         grade_PRO = if_else(is_PRO, Grade, NA_real_)) %>%  
  
  mutate(ECTS = case_when(`Course ID` %>% str_detect("HUM|SCI|SSC|COR|PRO|SA|EXT") ~ 5  ,
                          `Course ID` %>% str_detect("SKI|LAN"                   ) ~ 2.5,
                          `Course ID` %>% str_detect("CAP|UGR"                   ) ~ 10 ,
                          TRUE                                                     ~ 5  )) %>%

  mutate(ECTS_HUM = if_else(is_HUM, ECTS, NA_real_),
         ECTS_SCI = if_else(is_SCI, ECTS, NA_real_),
         ECTS_SSC = if_else(is_SSC, ECTS, NA_real_),
         ECTS_COR = if_else(is_COR, ECTS, NA_real_),
         ECTS_SKI = if_else(is_SKI, ECTS, NA_real_),
         ECTS_PRO = if_else(is_PRO, ECTS, NA_real_)) %>%
  
  mutate(grade_ECTS     = Grade     * ECTS,
         grade_ECTS_HUM = grade_HUM * ECTS_HUM,
         grade_ECTS_SCI = grade_SCI * ECTS_SCI,
         grade_ECTS_SSC = grade_SSC * ECTS_SSC,
         grade_ECTS_COR = grade_COR * ECTS_COR,
         grade_ECTS_SKI = grade_SKI * ECTS_SKI,
         grade_ECTS_PRO = grade_PRO * ECTS_PRO)
```

``` r
student_GPA <- d_transcript_augmented %>%
  
  # Courses taken in each period
  group_by(`Student ID`, time) %>%
  summarize_at(vars(matches("ECTS")), list) %>%
  
  # Cumulative GPAs
  group_by(`Student ID`) %>%
  arrange(time) %>%
  mutate_at(vars(matches("ECTS")), purrr::accumulate, c) %>%
  mutate_at(vars(matches("ECTS")), map_dbl, sum, na.rm = TRUE) %>%
  mutate(GPA     = grade_ECTS     / ECTS,
         GPA_HUM = grade_ECTS_HUM / ECTS_HUM,
         GPA_SCI = grade_ECTS_SCI / ECTS_SCI,
         GPA_SSC = grade_ECTS_SSC / ECTS_SSC,
         GPA_COR = grade_ECTS_COR / ECTS_COR,
         GPA_SKI = grade_ECTS_SKI / ECTS_SKI,
         GPA_PRO = grade_ECTS_PRO / ECTS_PRO) %>%
  mutate_at(vars(matches("GPA")), lag) %>%
  mutate_at(vars(matches("GPA")), ~ if_else(is.na(.), mean(d_transcript_augmented$Grade), .)) %>% # substitute missing GPA with the mean GPA across all courses (GPA_HUM is missing if student has no taken any HUM course yet)
  
  arrange(`Student ID`, time) 
```

Join student TP and GPA
=======================

``` r
d_student_model <- d_transcript_augmented %>%
  
  left_join(student_GPA, by = c("Student ID", "time")) %>%
  
  left_join(student_TP , by = c("Student ID", "time")) %>%
  
  select(`Student ID`, `Course ID`, Year_numerical, Period, Grade, time, matches("GPA"), matches("Topic")) %>%
  arrange(`Student ID`, time)
```

Save
====

``` r
student_model_nest_app <- d_student_model %>%
  
  nest(.key = model, matches("GPA|Topic")) %>%
  mutate(model = model %>% map(as.matrix)) %>%
  
  group_by(`Student ID`) %>% # only keep most recent profile of each student
  top_n(1, time) %>%
  
  select(`Student ID`, model)
```

``` r
save(d_student_model, file = "Output/student_model.RDATA")

save(student_model_nest_app, file = "APP/Recommender System/student_model_nest_app.RDATA")
```
