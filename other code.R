# TODO

## Estimating concentrations
For our analysis we would also like to know what the concentration of each student is. However, this is not given, so we will have to estimate. We know that the maximimum amount of courses a student can take outside of their concentration is 2. Therefore, if any student has more than two courses in one concetration, this should count towards the concentration. We expect students who addhere strictly to single concentrations, and then students who have a mixed concentration. However, it is possible that a person has not yet taken sufficient courses to make a call on their concentration, we will mark these people as "Undecided". Furthermore, it is possible that a student has taken too many courses out of all the concentrations, we will label these "Confused" and for all other cases we need to check the specifics, therefore we will label them "oops" 

```{r, eval = FALSE}
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
  ```{r, eval = FALSE}
checkup <- filter(d_student_concentration, student_Concentration == "oops") 
```

###Getting UCM_YEAR
In order to compare the trajectories of students, we would like to know if the courses were taken in their first, second, third, or whatever year. However, we only have the calendar years. Lets find out to what year of study each calendar year corresponds per student.

First, lets find out, which year was a student's first year and which year was a student's last year (we will also create a variable `Subs_year` that we will substract later to transform from Calendar Year to UCM Year, and a second variable `LengthUCM` that shows us how many years each student stayed at the college):
  ```{r, eval = FALSE}
d_year <- d_grade %>%
  select(ID, Code, Year, Period, Grade,`Booking Status Description`, `Course concentration`) %>%
  group_by(ID) %>%
  summarise(FirstYear=min(Year), LastYear=max(Year)) %>%
  mutate(LengthUCM = LastYear-FirstYear, Subs_year=FirstYear-1)
```

Now, lets make a dataframe of students against years, where the year appears in the corresponding cell if a student was in the college at that time:
  
  ```{r, eval = FALSE}
d_year1 <- d_grade %>%
  select(ID, Code, Year, Period, Grade, `Booking Status Description`, `Course concentration`) %>%  
  group_by( ID, Year) %>%
  summarise(Count = n())%>%
  mutate(Present = case_when(Count > 0 ~ Year))%>%
  select(-Count) %>%
  spread(Year,Present, fill = 0)
```

Now we have collected all the years a student has been at the college in a single row (one row per student), and we also know what is the first and last year of each student. Let's join these dataframes:

```{r, eval = FALSE}
d_time <- left_join(d_year1, d_year)
```

Ideally, we would like to have a dataframe of students against calendar years filled with the corresponding UCM year (e.g. if the year 2017 was the first year of student 45, we would like to see the cell corresponding to student 45 and year 2017 marked with a "1").

```{r, eval = FALSE}
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

```{r, eval = FALSE}
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

###Removing master Program data and Cleaning Capstone Data 
We noticed that we had the data for people who stayed at this university for their masters. To filter these courses out we find out in which year people took their capstone courses.

For this we keep only the capstones. We expect some people to have repeated capstones:
```{r, eval = FALSE}
capstone_count <- d_students %>%
filter(Code=="CAP3000") %>%
group_by(ID)%>%
summarise(Count=n())
capstone_count
```
However, what is surptising is that some people have two different passed capstones:

```{r, eval = FALSE}
passed_capstone_count <- d_students %>%
filter(Code=="CAP3000", `Booking Status Description`=="Completed with Success") %>%
group_by(ID) %>%
summarise(Count=n(), Max = max(Grade), Min = min(Grade), Attempts= max(Attempt))
```

We can check the grades for people who had repeated passed capstones (lets just bring them to the top of the list):
```{r, eval = FALSE}
passed_capstone_count %>%
arrange(desc(Count), desc(Min), Attempts)
```
There are 1712 people who did capstone. We see that most students who have two passed Capstones, have in reality only 1, since the other capstone had a failing grade and the passing grade was on the second attempt (count starts at 0) was indeed failed. However there are two students who indeed have two different passed Capstones: one is student 282995 who has two different passing grades(because of a change in grading system at the collegue during the year 2008-2009), the other is student 544450 who has the same grade passed at two different moment in time.

We can check if there are any students with passing capstone who do not have a grade of at least 5.5. We do this by looking at the lowest Max values:
```{r, eval = FALSE}
passed_capstone_count %>%
arrange(Max)
```
We see the lowest grade for a passing capstone is 5.6, so this is fine. 


Now lets just keep the highest value for each student and correct for student 282995:
```{r, eval = FALSE}
passed_capstone_grades <- passed_capstone_count %>%
mutate(`Booking Status Description`= "Completed with Success", Code="CAP3000", Grade= case_when(ID== 282995~7.7,
T~Max)) %>%
select( ID, Code, Grade,`Booking Status Description`)
passed_capstone_grades
```

####Capstone Year
Now we can figure out the Capstone Year.

First we get the years for which there is a corresponding passing capstone grade, and check for repetition:
```{r, eval = FALSE}
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
From count_after we see that student 544450 has two passed capstones at different moments in time, we already knew this and we must ask Richard why this is the case. However, since both capstones where passed in 2010, this creates no problem for our current purposes. However, let's remove the later version.
We will also rename `Year` to `Capstone Year` and keep only the columns `Capstone Year` and `ID`
```{r, eval = FALSE}
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
```{r, eval = FALSE}
d_transcripts <- d_students %>%
  group_by(ID) %>%
  left_join(capstone_finish_year) %>%
  ungroup() %>%
  replace_na(list(`Capstone Year`=9999)) %>%
  mutate(`After Graduation`=`Capstone Year`-`Year`)
d_transcripts
```
Now we will remove all courses with negative `After Graduation`:
  ```{r, eval = FALSE}
master_courses <- d_transcripts%>%
  filter(`After Graduation`<0)
d_transcripts <- d_transcripts %>%
  anti_join(master_courses)
head(d_transcripts)
```

One last thing. The variable `Booking Status Description` shows whether a student passed or failed the course. However, some mistakes in the way the data was handled meant that the course status of some students was never confirmed. We need to ASK RICHARD, but for the moment, we will just look at the grades of those courses which were not confirmed to change into pass and fail.

```{r, eval = FALSE}
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
####Ordering in Time
We would like to have a column `Time` that codes for both year and period (e.g. Year_1 Period_2 should be 1.2), as well as a column `Periods` with the year and period as numbers (e.g. Year_2 Period_1 should be period 7).

CHECK: we still have students who's first record is of their last year. This appears as UCM_Year=1.

```{r, eval = FALSE}
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
NOTES: The data is now arranged so as to respect the order in time for each individual student. However, the comparison is still tricky as people who finished in 2008 will have CAP recorded as taken on their first year since this is all the data we have. 
COMMENT SOFIA: save a_clean_transcript as d_transcript

